------------------------------------------------------------------
-- |
-- Module      : Gom.Sig
-- Copyright   : (c) Paul Brauner 2009
--               (c) Emilie Balland 2009
--               (c) INRIA 2009
-- Licence     : GPL (see COPYING)
--
-- Maintainer  : paul.brauner@inria.fr
-- Stability   : provisional
-- Portability : non-portable (requires generalized newtype deriving)
--
-- This module exposes 'Module', the datatype representing gom modules
-- after parsing, and several helper functions.
--------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gom.Sig (
  SortId(),
  FieldId(),
  CtorId(),
  Module(..),
  SortDef(..),
  Ctor(..),
  -- * Building identifiers
  makeSortId,
  makeFieldId,
  makeCtorId,
  prependEmpty,
  prependCons,
  prependHead,
  prependTail,
  lowerSortId,
  -- * Recovering information in a module
  simpleFields,
  definedSorts,
  constructorNames,
  vconstructorNames
) where

import Data.Char(toLower)
import Text.PrettyPrint.Leijen
import Test.QuickCheck

-- | Sort name (e.g. @Expr@) identifier.
newtype SortId     = SortId String 
  deriving (Ord,Eq)
-- | Field name (e.g. @x@) identifier.
newtype FieldId    = FieldId String 
  deriving (Ord,Eq)
-- | Constructor name (e.g. @f@) identifier.
newtype CtorId     = CtorId String 
  deriving (Ord,Eq)

instance Show SortId  where show (SortId s)    = s
instance Show FieldId where show (FieldId s)   = s
instance Show CtorId  where show (CtorId s)    = s

instance Pretty SortId  where pretty = text . show
instance Pretty FieldId where pretty = text . show
instance Pretty CtorId  where pretty = text . show

-- | Represents a gom module.
data Module = Module {
  moduleName :: String,    -- ^ module name
  imports    :: [SortId],  -- ^ list of imported sorts
  sortDefs   :: [SortDef]  -- ^ sort definitions
} deriving (Eq)

-- | Represents a sort definition, 
-- e.g. @List = nil() | cons(x:int, xs:List)@.
data SortDef = SortDef {
  sortName :: SortId,      -- ^ sort name (e.g. @List@)
  ctors    :: [Ctor]       -- ^ constructors
                           -- (e.g. @nil()@ and @cons(x:int, xs:List)@)
} deriving (Eq)

-- | Represents a constructor definition.
data Ctor = 
    -- | non-variadic constructor, e.g. @f(x:int,y:T)@
    Simple { 
      ctorName :: CtorId,          -- ^ constructor name (e.g. @f@)
      fields :: [(FieldId,SortId)] -- ^ constructor fields (e.g. @x:int@ and @y:T@)
    }
    -- | variadic constructor, e.g. @plus(Expr*)@
  | Variadic { 
      ctorName :: CtorId, -- ^ constructor name (e.g. @plus@)
      field :: SortId     -- ^ sort of the only field (e.g. @Expr@)
    }
 deriving (Eq)

makeSortId :: String -> SortId
makeSortId = SortId

makeFieldId :: String -> FieldId
makeFieldId = FieldId

makeCtorId :: String -> CtorId
makeCtorId = CtorId

-- | Turns @C@ into @EmptyC@.
prependEmpty :: CtorId -> CtorId
prependEmpty (CtorId s) = CtorId ("Empty" ++ s)

-- | Turns @C@ into @ConsC@.
prependCons :: CtorId -> CtorId
prependCons (CtorId s) = CtorId ("Cons" ++ s)

-- | Turns @C@ into @HeadC@.
prependHead :: CtorId -> FieldId
prependHead (CtorId s) = FieldId ("Head" ++ s)

-- | Turns @C@ into @TailC@.
prependTail :: CtorId -> FieldId
prependTail (CtorId s) = FieldId ("Tail" ++ s)

-- | Turns the id into lowercase
lowerSortId :: SortId -> SortId
lowerSortId (SortId s) = SortId (map toLower s)

-- | @simpleFields def@ is the list of fields of non-variadic constructors of
-- @def@, along with their sorts.
simpleFields :: SortDef -> [(FieldId, SortId)]
simpleFields def = concatMap getfields $ ctors def
  where getfields (Simple _ f) = f
        getfields _            = []

-- | @vconstructorNames m@ returns the list of all the variadic constructors
-- declared in @m@.
--
-- As an example, for the following signature
--
-- > T = f(A*) | g(x:B)
-- > U = h(C*)
--
-- it returns @A@ and @C@.
vconstructorNames :: Module -> [CtorId]
vconstructorNames = concatMap (concatMap getvtors . ctors) . sortDefs
  where getvtors (Variadic f _) = [f]
        getvtors _              = []

-- | @definedSorts m@ returns the list of sort names defined by @m@,
-- i.e. the sorts that appear in the left-hand sides of definitions.
definedSorts :: Module -> [SortId]
definedSorts sig = imports sig ++ map sortName (sortDefs sig)

-- | @constructorNames m@ returns the list of all the constructors declared
-- in @m@.
constructorNames :: Module -> [CtorId]
constructorNames = map ctorName . concatMap ctors . sortDefs

-- QuickCheck --

instance Arbitrary SortId  where arbitrary = SortId  `fmap` genId
instance Arbitrary FieldId where arbitrary = FieldId `fmap` genId
instance Arbitrary CtorId  where arbitrary = CtorId  `fmap` genId

genId :: Gen [Char]
genId = listOf1 $ oneof [choose ('a','z'), choose ('A','Z')]

allDiff :: (Eq t) => [t] -> Bool
allDiff []     = True
allDiff (x:xs) = not (x `elem` xs) && allDiff xs

instance Arbitrary Module where
  arbitrary = do
    modul <- genId
    sorts <- arbitrary `suchThat` allDiff
    -- We need at least one constructor per sort
    cidss <- listOf (listOf1 arbitrary) `suchThat` (allDiff . concat)
    let mix = zip sorts cidss
    defs  <- mapM (genSortDef (map fst mix)) mix
    return $ Module modul [] defs
  shrink (Module m i d) = do
    d' <- shrink d
    return $ Module m i d'

genTypedFields ::  (Arbitrary a, Eq a) => [a1] -> Gen [(a, a1)]
genTypedFields sorts = do
  flds <- listOf1 arbitrary `suchThat` allDiff
  doms <- listOf1 (elements sorts)
  return $ zip flds doms

instance Arbitrary SortDef where
  shrink (SortDef s l) = do
    l' <- shrink l
    return $ SortDef s l' 

genSortDef ::  [SortId] -> (SortId, [CtorId]) -> Gen SortDef
genSortDef sorts (sid,cids) = do
  flds   <- genTypedFields sorts
  ctrs   <- mapM (genCtor sorts flds) cids
  return $ SortDef sid ctrs

instance Arbitrary Ctor where
  shrink (Simple c l) = do l' <- shrink l
                           return $ Simple c l' 
  shrink x            = return x 

genCtor :: [SortId] -> [(FieldId, SortId)] -> CtorId -> Gen Ctor
genCtor sorts flds cname =
  oneof [genSCtor flds cname, genVCtor sorts cname]

genSCtor :: [(FieldId, SortId)] -> CtorId -> Gen Ctor
genSCtor flds cname = do
  fis <- listOf (elements flds) `suchThat` allDiff
  return $ Simple cname fis

genVCtor :: [SortId] -> CtorId -> Gen Ctor
genVCtor sorts cname = do
  sort <- elements sorts
  return $ Variadic cname sort
