------------------------------------------------------------------
-- |
-- Module      : Gom.UnitTests
-- Copyright   : (c) Paul Brauner 2009
--               (c) Emilie Balland 2009
--               (c) INRIA 2009
-- Licence     : GPL (see COPYING)
--
-- Maintainer  : paul.brauner@inria.fr
-- Stability   : provisional
-- Portability : non-portable (requires generalized newtype deriving)
--
-- Run all unit tests
--------------------------------------------------------------------

module Gom.UnitTests(testSuite) where

import Test.Framework (Test,testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified Gom.SymbolTable
import Gom.Pretty ()
import Gom.Parser
import Gom.Sig

-- | checks that @parse . pretty = id@
propParsePretty :: Module -> Bool
propParsePretty m = parseModule (show m) == m

-- | cross modules tests
localTestSuite :: Test
localTestSuite = testGroup "cross module:" 
  [testProperty "parse . pretty = id" propParsePretty]

-- | all tests
testSuite :: [Test]
testSuite = [localTestSuite,Gom.SymbolTable.testSuite]

