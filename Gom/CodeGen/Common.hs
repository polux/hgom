{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gom.CodeGen.Common (
  -- * Pure functions
  lower,
  renderBuiltin,
  -- * The @Gen@ monad
  -- ** Definition
  Gen(),
  runGen,
  -- ** Basic access
  askSt,
  askConf,
  ifConf,
  ifConfM,
  iterOverFields,
  iterOverSortFields,
  -- ** Impure functions
  packagePrefix,
  qualifiedSort,
  qualifiedCtor,
  abstractType,
  qualifiedAbstractType
) where

import Gom.Sig
import Gom.SymbolTable
import Gom.Java
import Gom.Constants
import Gom.Config

import Text.PrettyPrint.Leijen
import Control.Monad.Reader
import Data.Char(toLower)
import Data.List(nub, intersperse)

-- | Turns a 'String' into lowercase.
lower :: String -> String
lower = map toLower

-- | @renderBuiltin s f b@ generates what is necessary to put
-- the representation of @f@ (field of sort @s@) in the buffer @b@.
renderBuiltin :: SortId -> FieldId -> Doc -> Doc
renderBuiltin s f b 
  | isString s = text "renderString" <> parens (b <> comma <> pretty f)
  | isChar   s = rMethodCall b (text "append") [fMinus0]
  | otherwise  = rMethodCall b (text "append") [pretty f]
  where fMinus0 = text "(int)" <> pretty f <> text " - (int)'0'"

-- | A computation inside a context containing a read-only symbol table.
newtype Gen a = Gen (Reader (SymbolTable,Config) a)
  deriving (Monad, MonadReader (SymbolTable,Config))

-- | Run the Gen monad
runGen ::  Gen a -> SymbolTable -> Config -> a
runGen (Gen comp) st c = runReader comp (st,c)

-- | Query symbol table.
askSt :: (SymbolTable -> a) -> Gen a
askSt f = asks (f . fst)

-- | Query configuration.
askConf :: (Config -> a) -> Gen a
askConf f = asks (f . snd)

-- | @ifConf f e d@ is @return e@ if @f config@ holds, else @return d@ .
--
-- Example usage: 
--
-- > do hs <- ifConf haskell [rMethod ...] []
-- >    return rClass ... (vcat $ defaultMethods ++ hs)
ifConf :: (Config -> Bool) -> a -> a -> Gen a
ifConf f e d = ifConfM f (return e) (return d) 

-- | @ifConfM f e d@ is @e@ if @f config@ holds, else @d@ .
--
-- Example usage: 
--
-- > do hs <- ifConfM haskell compSomeMethods (return [])
-- >    return rClass ... (vcat $ defaultMethods ++ hs)
ifConfM :: (Config -> Bool) -> Gen a -> Gen a -> Gen a
ifConfM f e d = do cond <- askConf f
                   if cond then e else d

-- | Helper fonction that iters over the fields of
-- a constructor and combines them.
iterOverFields 
  :: (FieldId -> SortId -> Gen a) -- ^ the function to iter
  -> ([a] -> b)                   -- ^ the combinator
  -> CtorId                       -- ^ the constructor
  -> Gen b
iterOverFields f g c = do fis  <- askSt (fieldsOf c)
                          fis' <- mapM (uncurry f) fis
                          return $ g fis'

-- | Helper fonction for 'compEmptyGettersOfSort' and 
-- 'compEmptySettersOfSort'. Iters the first argument on the fields of the
-- subject sort and combines them with the second one. Duplicate fields are
-- merged.
iterOverSortFields 
  :: (SortId -> FieldId -> SortId -> Gen a) -- ^ gets codomain, field name and field sort
  -> ([a] -> b) -- ^ combinator of results
  -> SortId     -- ^ subject sort
  -> Gen b
iterOverSortFields f g s = do cs <- askSt (sCtorsOf s)
                              fs <- (nub . concat) `liftM` mapM combine cs
                              ms <- mapM (\(co,(fi,ty)) -> f co fi ty) fs
                              return $ g ms
   where combine c = do fis <- askSt (fieldsOf c)
                        co  <- askSt (codomainOf c)
                        return $ map ((,) co) fis


-- | Returns the package prefix ended by a dot.
--
-- As an example, returns @aa.bb.cc.foo@ for the module @Foo@,
-- provided the user toggled @-p aa.bb.cc@.
packagePrefix :: Gen Doc
packagePrefix = do m <- lower `liftM` askSt modName
                   go (pretty m) `liftM` askConf package
  where go dm Nothing  = dm
        go dm (Just l) = hcat . intersperse dot $ map text l ++ [dm]

-- | Given a sort @S@, returns @module.types.S@
qualifiedSort :: SortId -> Gen Doc
qualifiedSort s 
  | isBuiltin s = return $ pretty s
  | otherwise   = do p <- packagePrefix
                     return $ p <> dot <> text "types" <> dot <> pretty s

-- | Given a constructor @C@ of codomain @S@, returns
-- @module.types.s.C@
qualifiedCtor :: CtorId -> Gen Doc
qualifiedCtor c = 
  do p <- packagePrefix
     lows <- lowerSortId `liftM` askSt (codomainOf c)
     return $ p <> dot <> text "types" <> dot <> 
              pretty lows <> dot <> pretty c

-- | Generates @mAbstractType@ for the current module @m@.
abstractType :: Gen String
abstractType = do mn <- askSt modName
                  return $ mn ++ "AbstractType"

-- | Generates @m.mAbstractType@ for the current module @m@.
qualifiedAbstractType :: Gen Doc
qualifiedAbstractType = do at <- abstractType
                           pr <- packagePrefix 
                           return $ pr <> dot <> text at

