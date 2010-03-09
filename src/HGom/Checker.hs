------------------------------------------------------------------
-- |
-- Module      : HGom.Checker
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

module HGom.Checker (
  checkEverything
) where

import Common.Sig
import Common.Pretty()
import Common.Checkers

import Text.PrettyPrint.Leijen
import Data.Maybe(mapMaybe)

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
