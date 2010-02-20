------------------------------------------------------------------
-- |
-- Module      : Gom.SymbolTable
-- Copyright   : (c) Paul Brauner 2009
--               (c) Emilie Balland 2009
--               (c) INRIA 2009
-- Licence     : GPL (see COPYING)
--
-- Maintainer  : paul.brauner@inria.fr
-- Stability   : provisional
-- Portability : non-portable (requires generalized newtype deriving)
--
-- An efficient representation of a gom module.
--------------------------------------------------------------------

module Gom.SymbolTable (
  SymbolTable (),
  -- * Constructing tables
  emptySt, ast2st,
  -- * Consulting tables
  definedSortsIds, simpleConstructorsIds, variadicConstructorsIds,
  modName, importedSorts, sCtorsOf, vCtorsOf, fieldsOf, fieldOf, codomainOf,
  isGenerated, importsString,
  -- * Modifying tables
  -- ** Modifying mappings
  -- | These functions allow to change the constructor names (resp. fields)
  -- associated to a sort (resp. constructor) in a 'SymbolTable'.
  insertSctors, insertVctors, insertSfields, insertVfield,
  addToSctors, addToVctors,
  -- ** Adding whole definitions
  -- | These functions perform higher-level modifications of the table than
  -- 'insertSctors', 'addToSctors', etc.
  -- They take AST bits of "Gom.Sig" as arguments.
  addCtor,
  -- ** Table completion
  completeVariadics,
  -- * Consitency Checks (to be quickChecked)
  propCodomConsistent
) where

import Gom.Sig
import Control.Monad.State
import qualified Data.Map as M
import Data.Either(partitionEithers)
import Data.List(foldl',nub,sort)

import Test.QuickCheck

-- | A private datatype implemented by maps from sorts to constructors, from
-- constructors to codomains, etc.
data SymbolTable = 
  SymbolTable {
    -- | name of the module encoded by the symbol table
    mName :: String,
    -- | sorts imported by the module
    imported :: [SortId],
    -- | non-variadic constructors associated to a sort
    sctors :: M.Map SortId [CtorId],
    -- | variadic constructors associated to a sort
    vctors :: M.Map SortId [CtorId],
    -- | fields associated to a non-variadic constructor
    sfields :: M.Map CtorId [(FieldId,SortId)],
    -- | sort of the unique field of a variadic constructor
    vfield :: M.Map CtorId SortId,
    -- | codomain of the constructor (redundant information)
    codom :: M.Map CtorId SortId,
    -- | maps generated constructors to original ones
    baseCtor :: M.Map CtorId CtorId
  } deriving (Show)

-- | Name of the module encoded by the symbol table.
modName :: SymbolTable -> String
modName = mName

-- | Sorts imported by the module.
importedSorts :: SymbolTable -> [SortId]
importedSorts = imported

-- | Non-variadic constructors associated to a sort.
sCtorsOf :: SortId -> SymbolTable -> [CtorId]
sCtorsOf s st = 
  case s `M.lookup` sctors st of
    Just l  -> l
    Nothing -> error $ "sort" ++ show s ++ "not declared"

-- | Variadic constructors associated to a sort.
vCtorsOf :: SortId -> SymbolTable -> [CtorId]
vCtorsOf s st = 
  case s `M.lookup` vctors st of
    Just l  -> l
    Nothing -> error $ "sort" ++ show s ++ "not declared" 

-- | Fields associated to a non-variadic constructor.
fieldsOf :: CtorId -> SymbolTable -> [(FieldId,SortId)]
fieldsOf c st = 
  case c `M.lookup` sfields st of
    Just l  -> l
    Nothing -> error $ "non-variadic constructor" ++ show c ++ "not declared" 

-- | Sort of the unique field of a variadic constructor.
fieldOf :: CtorId -> SymbolTable -> SortId
fieldOf c st =
  case c `M.lookup` vfield st of
    Just s  -> s
    Nothing -> error $ "variadic constructor" ++ show c ++ "not declared" 

-- | Codomain of the constructor.
codomainOf :: CtorId -> SymbolTable -> SortId
codomainOf c st =
  case c `M.lookup` codom st of
    Just s  -> s
    Nothing -> error $ "constructor" ++ show c ++ "not declared" 

-- | If the field has been generated (e.g. @ConsC@) returns @'Just' C@,
-- @'Nothing'@ otherwise.
isGenerated :: CtorId -> SymbolTable -> Maybe CtorId
isGenerated c = M.lookup c . baseCtor

-- | Returns @True@ if @String@ is imported.
importsString :: SymbolTable -> Bool
importsString st = makeSortId "String" `elem` importedSorts st

-- | @emptySt m is@ is an empty symbol table (no sorts nor constructors) that
-- encodes a module of name @m@ which imports sorts @is@.
emptySt :: String -> [SortId] -> SymbolTable
emptySt m i = SymbolTable m i M.empty M.empty M.empty M.empty M.empty M.empty

-- | The ids of the sorts present in st.
definedSortsIds :: SymbolTable -> [SortId]
definedSortsIds st = nub $ M.keys (sctors st) ++ M.keys (vctors st)

-- | The ids of the non-variadic constructors present in st.
simpleConstructorsIds :: SymbolTable -> [CtorId]
simpleConstructorsIds = concat  . M.elems . sctors

-- | The ids of the variadic constructors present in st.
variadicConstructorsIds :: SymbolTable -> [CtorId]
variadicConstructorsIds = concat . M.elems . vctors

-- | private : breaks redundancy consistency, updates only @codom@
updateCodom :: CtorId -> SortId -> SymbolTable -> SymbolTable
updateCodom c s st = st { codom = M.insert c s (codom st) }

-- | @insertSctors s cs st@ inserts, or replaces, the mapping @(s,cs)@ in 
-- @'sctors' st@.
insertSctors :: SortId -> [CtorId] -> SymbolTable -> SymbolTable
insertSctors s cs st = update $ st { sctors = M.insert s cs (sctors st) }
  where update mst = foldl' (\t c -> updateCodom c s t) mst cs

-- | @insertVctors s cs st@ inserts, or replaces, the mapping @(s,cs)@ in 
-- @'vctors' st@.
insertVctors :: SortId -> [CtorId] -> SymbolTable -> SymbolTable
insertVctors s cs st = update $ st { vctors = M.insert s cs (vctors st) }
  where update mst = foldl' (\t c -> updateCodom c s t) mst cs

-- | @insertSfields c fs st@ inserts, or replaces, the mapping @(c,fs)@ in 
-- @'sfields' st@.
insertSfields :: CtorId -> [(FieldId,SortId)] -> SymbolTable -> SymbolTable
insertSfields c fs st = st { sfields = M.insert c fs (sfields st) }

-- | @insertVfield c s st@ inserts, or replaces, the mapping @(c,s)@ in 
-- @'vfield' st@.
insertVfield :: CtorId -> SortId -> SymbolTable -> SymbolTable
insertVfield c s st = st { vfield = M.insert c s (vfield st) }

-- | @addToSctors s cs st@ appends @cs@ to the non-variadic constructors
-- already associated to @s@ in @st@. If there are no non-variadic constructors
-- associated to @s@, an entry is created.
addToSctors :: SortId -> [CtorId] -> SymbolTable -> SymbolTable
addToSctors s cs st = update $ st { sctors = M.alter (Just . maybe cs (++cs)) s (sctors st) }
  where update mst = foldl' (\t c -> updateCodom c s t) mst cs

-- | @addToVctors s cs st@ appends @cs@ to the variadic constructors already
-- associated to @s@ in @st@.  If there are no variadic constructors associated
-- to @s@, an entry is created.
addToVctors :: SortId -> [CtorId] -> SymbolTable -> SymbolTable
addToVctors s cs st = update $ st { vctors = M.alter (Just . maybe cs (++cs)) s (vctors st) }
  where update mst = foldl' (\t c -> updateCodom c s t) mst cs

-- | Converts a 'Module' into a 'SymbolTable', provided no
-- error has been detected during the checking phase.
ast2st :: Module -> SymbolTable
ast2st (Module m i defs) = execState (conv defs) (emptySt m i)

  where conv :: [SortDef] -> State SymbolTable ()
        conv = mapM_ convdef 

        convdef :: SortDef -> State SymbolTable ()
        convdef (SortDef n cs) = do 
           (ss,vs) <- partitionEithers `liftM` mapM convctor cs
           modify $ insertSctors n ss
           modify $ insertVctors n vs

        convctor :: Ctor -> State SymbolTable (Either CtorId CtorId)
        convctor (Simple n fis) = do modify $ insertSfields n fis
                                     return (Left n)
        convctor (Variadic n s) = do modify $ insertVfield n s
                                     return (Right n)

-- | @addCtor s c st@ updates the mappings of @st@ in order
-- to reflect the addition of @c@ to the constructors already
-- associated to @s@.
addCtor :: SortId -> Ctor -> SymbolTable -> SymbolTable
addCtor s (Simple n f)   = insertSfields n f . addToSctors s [n]
addCtor s (Variadic n t) = insertVfield n t . addToVctors s [n]

-- | Same as @addCtor@ but for several constructors.
addCtors :: SortId -> [Ctor] -> SymbolTable -> SymbolTable
addCtors s cs st = foldl' (flip $ addCtor s) st cs

-- | @tonil c@ constructs a @Nilc@ constructor.
toNil :: CtorId -> Ctor
toNil ci = Simple (prependEmpty ci) []

-- | @tocons c co dom@ constructs a @Consc@ constructor of domain @dom@ and
-- codomain @c@.
toCons :: CtorId -> SortId -> SortId -> Ctor
toCons ci co dom = 
  Simple (prependCons ci) [(prependHead ci,dom),(prependTail ci,co)]

-- | @insertGeneratedCtor gc bc st@ inserts mapping
-- @(gc,bc)@  from generated constructor
-- to the orginal one (e.g. @ConsC@ and @C@ in @st@.
insertGeneratedCtor :: CtorId -> CtorId -> SymbolTable -> SymbolTable
insertGeneratedCtor gc bc st = st { baseCtor = M.insert gc bc (baseCtor st) }

-- | @insertGeneratedCtors [gc1,..,gcn] bc st@ adds mappings
-- @(gc1,bc)@ .. @(gcn,bc)@ from generated constructors
-- to the orginal ones (e.g. @ConsC@ and @EmptyC@ to @C@ in @st@.
insertGeneratedCtors :: [CtorId] -> CtorId -> SymbolTable -> SymbolTable
insertGeneratedCtors gcs bc st = 
  foldl' (\st' gc -> insertGeneratedCtor gc bc st') st gcs

-- | For each variadic constructor @V(T*)@ of codomain @S@,
-- adds two constuctors @EmptyV()@ and @ConsV(HeadV:T, TailV:S)@
-- to @S@.
completeVariadics :: SymbolTable -> SymbolTable
completeVariadics st = foldl' add st res
  where res = map conv vcs
        vcs = variadicConstructorsIds st
        conv ci = let co  = codomainOf ci st
                      dom = fieldOf ci st
                  in (co, ci, [toNil ci, toCons ci co dom])
        add st' (co,ci,l) = 
          (insertGeneratedCtors (map ctorName l) ci . addCtors co l) st'

-- | generates an arbitrary Symboltable from an arbitrary signature
instance Arbitrary SymbolTable where
  arbitrary = do
    sig <- arbitrary
    return $ ast2st sig 

-- | checks that redundant information about constructor domains is consistent
-- with the rest of the table
propCodomConsistent :: SymbolTable -> Bool
propCodomConsistent st = sort assocs == sort (M.toList $ codom st)
  where assocs = toAssoc (sctors st) ++ toAssoc (vctors st)
        toAssoc amap = [(c,s) | (s,l) <- M.toList amap, c <- l]


