------------------------------------------------------------------
-- |
-- Module      : Common.Checker
-- Copyright   : (c) Paul Brauner 2009
--               (c) Emilie Balland 2009
--               (c) INRIA 2009
-- Licence     : GPL (see COPYING)
--
-- Maintainer  : paul.brauner@inria.fr
-- Stability   : provisional
-- Portability : non-portable (requires generalized newtype deriving)
--
-- Checker to be run before the construction of a symbol table.
--------------------------------------------------------------------

module Common.Checker (
  NameConsistencyError(),
  UndefSortError(),
  MultipleCtorDecl(),
  MultipleSortDecl(),
  MultipleFieldsError(),
  GeneratedConstructorsClash(),
  -- * Individual features checking
  checkJavaKeywordClash,
  checkCtorModuleClash,
  checkMultipleSortDecl,
  checkMultipleCtorDecl,
  checkDuplicateFields,
  checkNameConsistency,
  checkUndefSorts,
  checkGenClashes,
  -- * All-in-one check
  checkEverything
) where

import Common.Sig
import Common.Pretty()

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe(mapMaybe)
import Text.PrettyPrint.Leijen
import Data.Char(toLower)

newtype NameConsistencyError = NCE [(SortId,[(FieldId,[SortId])])]

instance Pretty NameConsistencyError where
  pretty (NCE e) = vsep $ map f e
    where f (s,al) = hang 2 (text "In the definition of sort" <+> 
                     text (show s) <> text ":" <$> vsep (map g al))
          g (x,l)  = text "field" <+> text (show x) <+> 
                     text "has many sorts:" <+>
                     hsep (punctuate (text ",") (map pretty l))

newtype UndefSortError = USE [(SortId,[(CtorId,[SortId])])]

instance Pretty UndefSortError where
  pretty (USE e) = vsep $ map f e
    where f (s,l)  = nest 2 $ text "In the definition of sort" <+> 
                              text (show s) <> text ":" </> vsep (map g l)
          g (c,ss) = nest 2 $ text "in constructor" <+> text (show c) <> 
                              text ":" </> 
                              text "the following sorts are not defined:" <+>
                              hsep (punctuate (text ",") (map pretty ss))

newtype MultipleFieldsError = MFE [(SortId,[(CtorId,[(FieldId,Int)])])]

instance Pretty MultipleFieldsError where
  pretty (MFE e) = vsep $ map f e
    where f (s,l)  = nest 2 $ text "In the definition of sort" <+> 
                              text (show s) <> text ":" </> vsep (map g l)
          g (c,fs) = nest 2 $ text "in constructor" <+> text (show c) <> 
                              text ":" </> vsep (map h fs)
          h (fi,n) = text "there are" <+> int n <+> 
                     text "fields named" <+> text (show fi)

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
                     nest 2 (text "clashes with:" </>
                            (hsep . punctuate (text ",")) (map pretty cs))

data JavaKeywordClash = JKC [String] [SortId] [CtorId] [FieldId]

instance Pretty JavaKeywordClash where
  pretty (JKC m s c f) = vcat [p "Module name"       m,
                               p "Sort name"         s, 
                               p "Constructors name" c, 
                               p "Field name"        f]
    where p mes = vcat . map (er mes)
          er mes x = text mes <+> dquotes (pretty x) <+> 
                     text "clashes with java keywords."

data CtorModuleClash = CMC [CtorId]

instance Pretty CtorModuleClash where
  pretty (CMC cs) = vcat (map f cs)
    where f c = text "Constructor" <+> pretty c <+> 
                text "clashes with module's name."

-- | helper function to pack checkers results into Maybes
pack :: ([t] -> a) -> [t] -> Maybe a
pack _ [] = Nothing
pack c l  = Just (c l)

-- | Auxiliary function for 'checkNameConsistency'
differentSortsForAName :: SortDef -> [(FieldId, [SortId])]
differentSortsForAName = 
  convert . M.filter f . populate M.empty . simpleFieldsOf
  where populate m []         = m
        populate m ((x,t):fs) = let ts = S.singleton t
                                    m' = M.insertWith S.union x ts m
                                in populate m' fs
        f s = S.size s > 1
        convert = M.toAscList . M.map S.toList

-- | Checks that each field name in all the constructors of a sort 
-- is assigned the same type everywhere.
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
checkNameConsistency = pack NCE . mapMaybe check . sortDefs 
  where check def = case differentSortsForAName def of
          [] -> Nothing
          al -> Just (sortName def, al)

-- | Checks that all sorts mentionned in the constructors are either 
-- defined or imported.
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
checkUndefSorts sig = pack USE $ mapMaybe f (sortDefs sig)
  where def = exportedSorts sig
        f (SortDef n _ ctrs) = 
          case mapMaybe g ctrs of
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
count = mapMaybe multi . L.group . L.sort
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
checkDuplicateFields sig = pack MFE $ mapMaybe f (sortDefs sig)
  where f (SortDef n _ ctrs) = 
          case mapMaybe g ctrs of
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
checkMultipleCtorDecl = pack MCD . count . constructorNames

-- | Checks that a sort is not defined twice in the module
--
-- As an exemple, for the following signature
--
-- > List = inil() | icons(x:int,y:IntList)
-- > List = snil() | scons(x:Str,y:StrList)
--
-- the function will report that @List@ is defined twice.
checkMultipleSortDecl :: Module -> Maybe MultipleSortDecl
checkMultipleSortDecl = pack MSD . count . exportedSorts

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
checkGenClashes m = pack GCG $ mapMaybe check vcs
  where vcs = vconstructorNames m
        cs  = constructorNames  m
        check vc = 
          let l = L.nub $ filter (`elem` [prependEmpty vc, prependCons vc]) cs
          in if null l then Nothing else Just (vc,l)
           
javaKeywords :: S.Set String
javaKeywords = S.fromList
  ["abstract","continue","for","new","switch","assert","default","goto",
   "package","synchronized","do","if","private","this","break",
   "implements","protected","throw","else","import",
   "public","throws","case","enum","instanceof","return","transient",
   "catch","extends","try","final","interface",
   "static","void","class","finally","strictfp","volatile","const",
   "native","super","while",
   "boolean","double","byte","int","short","long","float","char"]

-- | Looks for module, sorts, ctors and fields names that would clash
-- with java keywords.
checkJavaKeywordClash :: Module -> Maybe JavaKeywordClash
checkJavaKeywordClash m = pack4 (filter checkMod [moduleName m])
                                (filter checkSort $ definedSorts m)
                                (filter checkCtor $ constructorNames m)
                                (filter checkFld $ simpleFieldsNames m)
  where pack4 [] [] [] [] = Nothing
        pack4 l1 l2 l3 l4 = Just $ JKC l1 l2 l3 l4
        isJkw      = (`S.member` javaKeywords)
        checkMod   = isJkw . map toLower
        checkSort  = isJkw . map toLower . idStr
        checkCtor  = isJkw . idStr 
        checkFld   = isJkw . idStr

-- | Looks for clashes between lowercase module name and constructor names
checkCtorModuleClash :: Module -> Maybe CtorModuleClash
checkCtorModuleClash m = pack CMC clashes
  where lowmn   = map toLower (moduleName m)
        clashes = filter ((== lowmn) . idStr) (constructorNames m)

checkers :: [Module -> Maybe Doc]
checkers = [w checkJavaKeywordClash,
            w checkCtorModuleClash,
            w checkMultipleSortDecl,
            w checkMultipleCtorDecl,
            w checkDuplicateFields,
            w checkNameConsistency,
            w checkUndefSorts,
            w checkGenClashes]
  where w check x = pretty `fmap` check x

-- | Reports, in this order, the results of:
--    - 'checkJavaKeywordClash'
--
--    - 'checkCtorModuleClash'
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
checkEverything m = ret $ mapMaybe ($ m) checkers  
  where ret [] = Nothing
        ret l  = Just $ foldr1 dbreak l
        dbreak x y = x <> linebreak <> linebreak <> y
