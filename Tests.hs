------------------------------------------------------------------
-- |
-- Module      : Tests
-- Copyright   : (c) Paul Brauner 2009
--               (c) Emilie Balland 2009
--               (c) INRIA 2009
-- Licence     : GPL (see COPYING)
--
-- Maintainer  : paul.brauner@inria.fr
-- Stability   : provisional
-- Portability : non-portable (requires generalized newtype deriving)
--
-- Run all QuickCheck tests
--------------------------------------------------------------------

import Test.QuickCheck

import Gom.SymbolTable
import Gom.Parser
import Gom.Pretty
import Gom.Sig

propParsePretty :: Module -> Bool
propParsePretty m = parseModule (show m) == m

-- | list of @(test_name, test_io_action)@
tests ::  [([Char], IO ())]
tests = [("codom_consistent", quickCheck propCodomConsistent)
        ,("parse/pretty    ",     quickCheck propParsePretty)]

-- | run quickCheck tests 
main = mapM_ (\(s,a) -> putStr (s ++ ": ") >> a) tests
