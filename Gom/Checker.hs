-- | Checker to be run before the construction of a symbol table.

module Gom.Checker (
  NameConsistencyError(),
  UndefSortError(),
  MultipleCtorDecl(),
  MultipleSortDecl(),
  MultipleFieldsError(),
  GeneratedConstructorsClash(),
  -- * Individual features checking
  checkMultipleSortDecl,
  checkMultipleCtorDecl,
  checkDuplicateFields,
  checkNameConsistency,
  checkUndefSorts,
  checkGenClashes,
  -- * All-in-one check
  checkEverything
) where

import Gom.Sig
import Gom.Pretty()

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe(catMaybes)
import Text.PrettyPrint.Leijen

newtype NameConsistencyError = NCE [(SortId,[(FieldId,[SortId])])]

instance Pretty NameConsistencyError where
  pretty (NCE e) = vsep $ map f e
    where f (s,al) = hang 2 (text "In the definition of sort" <+> text (show s) <> text ":" <$> vsep (map g al))
          g (x,l)  = text "field" <+> text (show x) <+> text "has many sorts:" <+> 
                     hsep (punctuate (text ",") (map pretty l))

newtype UndefSortError = USE [(SortId,[(CtorId,[SortId])])]

instance Pretty UndefSortError where
  pretty (USE e) = vsep $ map f e
    where f (s,l)  = nest 2 $ text "In the definition of sort" <+> text (show s) <> text ":" </> vsep (map g l)
          g (c,ss) = nest 2 $ text "in constructor" <+> text (show c) <> text ":" </> 
                              text "the following sorts are not defined:" <+>  
                              hsep (punctuate (text ",") (map pretty ss))

newtype MultipleFieldsError = MFE [(SortId,[(CtorId,[(FieldId,Int)])])]

instance Pretty MultipleFieldsError where
  pretty (MFE e) = vsep $ map f e
    where f (s,l)  = nest 2 $ text "In the definition of sort" <+> text (show s) <> text ":" </> vsep (map g l)
          g (c,fs) = nest 2 $ text "in constructor" <+> text (show c) <> text ":" </> vsep (map h fs)
          h (fi,n) = text "there are" <+> int n <+> text "fields named" <+> text (show fi)

newtype MultipleCtorDecl = MCD [(CtorId,Int)]

instance Pretty MultipleCtorDecl where
  pretty (MCD l) = vsep $ map f l
    where f (c,n) = text "Constructor" <+> text (show c) 
                    <+> text "is declared" <+> int n <+> text "times."

newtype MultipleSortDecl = MSD [(SortId,Int)]

instance Pretty MultipleSortDecl where
  pretty (MSD l) = vsep $ map f l
    where f (c,n) = text "Sort" <+> text (show c)
                    <+> text "is declared" <+> int n <+> text "times."

newtype GeneratedConstructorsClash = GCG [(CtorId,[CtorId])]

instance Pretty GeneratedConstructorsClash where
  pretty (GCG l) = vsep $ map f l
    where f (c,cs) = text "Variadic constructor" <+> text (show c) <+>
                     nest 2 (text "clashes with:" 
                             </> (hsep . punctuate (text ",")) (map pretty cs))

differentSortsForAName :: SortDef -> [(FieldId, [SortId])]
differentSortsForAName def = convert . M.filter f . populate M.empty $ simpleFields def
  where populate m []         = m
        populate m ((x,t):fs) = let ts = S.singleton t
                                    m' = M.insertWith S.union x ts m
                                in populate m' fs
        f s = S.size s > 1
        convert = M.toAscList . M.map S.toList

-- | helper function to pack checkers results into Maybes
pack :: ([t] -> a) -> [t] -> Maybe a
pack _ [] = Nothing
pack c l  = Just (c l)

-- | Checks that each field name in all the constructors of a sort is assigned
-- the same type everywhere.
-- 
-- As an example, for the following signature
--
-- > Expr = Lit(x:int) 
-- >      | Str(x:String) 
-- >      | ...
--
-- the function will report that @x@ is assigned both @int@ and
-- @String@ sorts.
checkNameConsistency :: Module -> Maybe NameConsistencyError
checkNameConsistency = pack NCE . catMaybes . map check . sortDefs 
  where check def = case differentSortsForAName def of
          [] -> Nothing
          al -> Just (sortName def, al)

-- | Checks that all sorts mentionned in the constructors are either defined or
-- imported.
--
-- As an example, in the following module
--
-- > module M
-- > imports String
-- > abstract syntax
-- >
-- > Expr = Lit(x:int) 
-- >      | Str(y:String) 
-- > 
-- > List = nil() | (x:A,xs:List)
--
-- the function will report that @int@ and @A@ are not defined. 
checkUndefSorts :: Module -> Maybe UndefSortError
checkUndefSorts sig = pack USE . catMaybes $ map f (sortDefs sig)
  where def = definedSorts sig
        f (SortDef n ctrs) = 
          case catMaybes $ map g ctrs of
            [] -> Nothing
            cs -> Just (n,cs)
        g c = case h c of
            [] -> Nothing
            ss -> Just (ctorName c, ss)
        h (Simple _ fis) = L.nub $ concatMap (i . snd) fis
        h (Variadic _ s) = i s
        i x | x `notElem` def = [x]
            | otherwise       = []

count :: (Ord a) => [a] -> [(a, Int)]
count = catMaybes . map multi . L.group . L.sort
  where multi [_] = Nothing
        multi cs  = Just (head cs, length cs)

-- | Checks, for each constructor defined in the signature, 
-- that all of its fields have different names.
--
-- As an example, for the following constructor
--
-- > Plus(e:Expr,e:Expr)
--
-- the function will report that @Plus@ contains two fields named @e@. 
checkDuplicateFields :: Module -> Maybe MultipleFieldsError
checkDuplicateFields sig = pack MFE . catMaybes $ map f (sortDefs sig)
  where f (SortDef n ctrs) = 
          case catMaybes $ map g ctrs of
            [] -> Nothing
            cs -> Just (n,cs)
        g c = case h c of
            [] -> Nothing
            ss -> Just (ctorName c, ss)
        h (Simple _ fis) = count $ map fst fis 
        h (Variadic _ _) = []

-- | Checks that a constructor is not declared twice in the module
--
-- As an exemple, for the following signature
--
-- > IntList = nil() | cons(x:int,y:IntList)
-- > StrList = nil() | cons(x:Str,y:StrList)
--
-- the function will report that both @nil@ and @cons@ are declared twice.
checkMultipleCtorDecl :: Module -> Maybe MultipleCtorDecl
checkMultipleCtorDecl sig = 
  pack MCD . count $ constructorNames sig

-- | Checks that a sort is not defined twice in the module
--
-- As an exemple, for the following signature
--
-- > List = inil() | icons(x:int,y:IntList)
-- > List = snil() | scons(x:Str,y:StrList)
--
-- the function will report that @List@ is defined twice.
checkMultipleSortDecl :: Module -> Maybe MultipleSortDecl
checkMultipleSortDecl sig = 
  pack MSD . count $ definedSorts sig

-- | Checks that constructor names that will be generated for
-- every variadic constructor don't collide with user-declared
-- constructors.
--
-- As an exemple, for the following signature
--
-- > List = list(int*)
-- > T    = Emptylist(...) | Conslist(...)
--
-- the function will report that @list@ clashes with both @Emptylist@ and
-- @Conslist@.
checkGenClashes :: Module -> Maybe GeneratedConstructorsClash
checkGenClashes m = pack GCG . catMaybes $ map check vcs
  where vcs = vconstructorNames m
        cs  = constructorNames  m
        check vc = 
          let l = L.nub $ filter (`elem` [prependEmpty vc, prependCons vc]) cs
          in if null l then Nothing else Just (vc,l)
           

checkers :: [Module -> Maybe Doc]
checkers = [w checkMultipleSortDecl,
            w checkMultipleCtorDecl,
            w checkDuplicateFields,
            w checkNameConsistency,
            w checkUndefSorts,
            w checkGenClashes]

  where w f x = f x >>= return . pretty

-- | Reports, in this order, the results of:
--
--    - 'checkMultipleSortDecl'
--
--    - 'checkMultipleCtorDecl'
--
--    - 'checkDuplicateFields'
--
--    - 'checkNameConsistency'
--
--    - 'checkUndefSorts'
--
--    - 'checkGenClashes'
checkEverything :: Module -> Maybe Doc
checkEverything m = ret . catMaybes $ map ($ m) checkers  
  where ret [] = Nothing
        ret l  = Just $ foldr1 dbreak l
        dbreak x y = (x <> linebreak <> linebreak <> y)
