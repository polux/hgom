------------------------------------------------------------------
-- |
-- Module      : Common.CodeGen.GenMonad
-- Copyright   : (c) Paul Brauner 2009
--               (c) Emilie Balland 2009
--               (c) INRIA 2009
-- Licence     : GPL (see COPYING)
--
-- Maintainer  : paul.brauner@inria.fr
-- Stability   : provisional
-- Portability : portable
--
-- Defines the 'Gen' monad, a context containing a read-only symbol table.
--------------------------------------------------------------------

module Common.CodeGen.GenMonad (
  -- * Definition
  Gen(),
  runGen,
  -- * Basic access
  askSt,
  askConf,
  ifConf,
  ifConfM,
  iterOverFields,
  iterOverSortFields,
  -- * High-Level access
  packagePrefix,
  qualifiedSort,
  qualifiedCtor,
  abstractType,
  qualifiedAbstractType,
  qualifiedStratPrefix
) where

import Common.Sig
import Common.SymbolTable
import Common.CodeGen.Builtins
import Common.Config
import Text.PrettyPrint.Leijen
import Control.Monad.Trans.Reader
import Control.Applicative
import Data.Char(toLower)
import Data.List(nub, intersperse)

-- | A computation inside a context containing a read-only symbol table
-- and a configuration of type @c@.
newtype Gen c a = Gen { unGen :: Reader (SymbolTable, c) a }

-- we redifine the instances to keep hgon h98 compatible
instance Functor (Gen c) where
  fmap f = Gen . fmap f . unGen

instance Applicative (Gen c) where
  f <*> x = Gen (unGen f <*> unGen x)
  pure = Gen . pure

instance Monad (Gen c) where
  return        = Gen . return
  (Gen m) >>= f = Gen (m >>= unGen . f)

-- | Run the Gen monad
runGen :: Gen c a -> SymbolTable -> c -> a
runGen (Gen comp) st c = runReader comp (st,c)

-- | Query symbol table.
askSt :: (SymbolTable -> a) -> Gen c a
askSt f = Gen $ asks (f . fst)

-- | Query configuration.
askConf :: (c -> a) -> Gen c a
askConf f = Gen $ asks (f . snd)

-- | @ifConf f e d@ is @return e@ if @f config@ holds, else @return d@ .
--
-- Example usage: 
--
-- > do hs <- ifConf haskell [rMethod ...] []
-- >    return rClass ... (vcat $ defaultMethods ++ hs)
ifConf :: (c -> Bool) -> a -> a -> Gen c a
ifConf f e d = ifConfM f (return e) (return d) 

-- | @ifConfM f e d@ is @e@ if @f config@ holds, else @d@ .
--
-- Example usage: 
--
-- > do hs <- ifConfM haskell compSomeMethods (return [])
-- >    return rClass ... (vcat $ defaultMethods ++ hs)
ifConfM :: (c -> Bool) -> Gen c a -> Gen c a -> Gen c a
ifConfM f e d = do cond <- askConf f
                   if cond then e else d

-- | Helper fonction that iters over the fields of
-- a constructor and combines them.
iterOverFields 
  :: (FieldId -> SortId -> Gen c a) -- ^ the function to iter
  -> ([a] -> b)                   -- ^ the combinator
  -> CtorId                       -- ^ the constructor
  -> Gen c b
iterOverFields f g c = do fis  <- askSt (fieldsOf c)
                          fis' <- mapM (uncurry f) fis
                          return $ g fis'

-- | Helper fonction for 'compEmptyGettersOfSort' and 
-- 'compEmptySettersOfSort'. Iters the first argument on the fields of the
-- subject sort and combines them with the second one. Duplicate fields are
-- merged.
iterOverSortFields 
  :: (SortId -> FieldId -> SortId -> Gen c a) 
     -- ^ gets codomain, field name and field sort
  -> ([a] -> b) -- ^ combinator of results
  -> SortId     -- ^ subject sort
  -> Gen c b
iterOverSortFields f g s = do cs <- askSt (sCtorsOf s)
                              fs <- (nub . concat) `fmap` mapM combine cs
                              ms <- mapM (\(co,(fi,ty)) -> f co fi ty) fs
                              return $ g ms
   where combine c = do fis <- askSt (fieldsOf c)
                        co  <- askSt (codomainOf c)
                        return $ map ((,) co) fis


-- | Returns the package prefix ended by a dot.
--
-- As an example, returns @aa.bb.cc.foo@ for the module @Foo@,
-- provided the user toggled @-p aa.bb.cc@.
packagePrefix :: IsConfig c => Gen c Doc
packagePrefix = do m <- map toLower `fmap` askSt modName
                   go (pretty m) `fmap` askConf package
  where go dm Nothing  = dm
        go dm (Just l) = hcat . intersperse dot $ map text l ++ [dm]

-- | Given a sort @S@, returns @module.types.S@
qualifiedSort :: IsConfig c => SortId -> Gen c Doc
qualifiedSort s 
  | isBuiltin s = return $ pretty s
  | otherwise   = do p <- packagePrefix
                     return $ p <> dot <> text "types" <> dot <> pretty s

-- | Given a constructor @C@ of codomain @S@, returns
-- @module.types.s.C@
qualifiedCtor :: IsConfig c => CtorId -> Gen c Doc
qualifiedCtor c = 
  do p <- packagePrefix
     lows <- lowerId `fmap` askSt (codomainOf c)
     return $ p <> dot <> text "types" <> dot <> 
              pretty lows <> dot <> pretty c

-- | Generates @mAbstractType@ for the current module @m@.
abstractType :: Gen c String
abstractType = do mn <- askSt modName
                  return $ mn ++ "AbstractType"

-- | Generates @m.mAbstractType@ for the current module @m@.
qualifiedAbstractType :: IsConfig c => Gen c Doc
qualifiedAbstractType = do at <- abstractType
                           pr <- packagePrefix 
                           return $ pr <> dot <> text at

-- | Given a sort @S@ in the module @m@, generates @m.strategy.s@
qualifiedStratPrefix :: IsConfig c => SortId -> Gen c Doc
qualifiedStratPrefix s = do
  m <- packagePrefix
  return $ m <> dot <> text "strategy" <> dot <> pretty (lowerId s)
