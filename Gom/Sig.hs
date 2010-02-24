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
  ClassId(),
  FieldId(),
  CtorId(),
  Module(..),
  SortDef(..),
  Ctor(..),
  GomId(..),
  -- * Building identifiers
  makeSortId,
  makeClassId,
  makeFieldId,
  makeCtorId,
  prependEmpty,
  prependCons,
  prependHead,
  prependTail,
  lowerId,
  -- * Recovering information in a module
  definedSorts,
  exportedSorts,
  constructorNames,
  vconstructorNames,
  simpleFieldsNames,
  simpleFieldsOf
) where

import Data.Char(toLower)
import Text.PrettyPrint.Leijen

-- | Sort name (e.g. @Expr@) identifier.
newtype SortId     = SortId String 
  deriving (Ord,Eq)
-- | Java Class name (e.g. @p1.p2.c@) identifier.
newtype ClassId     = ClassId [String] 
  deriving (Ord,Eq)
-- | Field name (e.g. @x@) identifier.
newtype FieldId    = FieldId String 
  deriving (Ord,Eq)
-- | Constructor name (e.g. @f@) identifier.
newtype CtorId     = CtorId String 
  deriving (Ord,Eq)

instance Show SortId  where show (SortId s)    = s
instance Show ClassId  where show (ClassId s)  = show ((hcat . punctuate dot) (map text s))
instance Show FieldId where show (FieldId s)   = s
instance Show CtorId  where show (CtorId s)    = s

instance Pretty SortId  where pretty = text . show
instance Pretty ClassId  where pretty = text . show
instance Pretty FieldId where pretty = text . show
instance Pretty CtorId  where pretty = text . show

class GomId a where
  idStr :: a -> String

instance GomId SortId where idStr (SortId x)   = x
instance GomId FieldId where idStr (FieldId x)   = x
instance GomId CtorId where idStr (CtorId x)   = x

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
  concreteSortName :: Maybe ClassId,      -- ^ sort name (e.g. @List@)
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

makeClassId :: [String] -> ClassId
makeClassId = ClassId

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
lowerId :: SortId -> SortId
lowerId (SortId x) = SortId (map toLower x)

-- | @simpleFields def@ is the list of fields of non-variadic 
-- constructors of @def@, along with their sorts.
simpleFieldsOf :: SortDef -> [(FieldId, SortId)]
simpleFieldsOf def = ctors def >>= getfields
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
vconstructorNames m = sortDefs m >>= ctors >>= getvtors
  where getvtors (Variadic f _) = [f]
        getvtors _              = []

-- | @exportedSorts m@ returns the list of sort names exported by @m@,
-- i.e. the sorts that appear in the left-hand sides of definitions
-- and the imported sorts.
exportedSorts :: Module -> [SortId]
exportedSorts sig = imports sig ++ definedSorts sig

-- | @definedSorts m@ returns the list of sort names defined by @m@,
-- i.e. the sorts that appear in the left-hand sides of definitions.
definedSorts :: Module -> [SortId]
definedSorts sig = map sortName (sortDefs sig)

-- | @constructorNames m@ returns the list of all the constructors declared
-- in @m@ (variadic or not).
constructorNames :: Module -> [CtorId]
constructorNames = map ctorName . concatMap ctors . sortDefs

-- | All declared simple fields, duplicates removed
simpleFieldsNames :: Module -> [FieldId]
simpleFieldsNames m = sortDefs m >>= ctors >>= getfields
  where getfields (Simple _ fs) = map fst fs
        getfields _             = []

