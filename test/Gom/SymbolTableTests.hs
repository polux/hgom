module Gom.SymbolTableTests where

import Gom.Pretty ()
import Gom.Random ()
import Gom.Sig(Module())
import Gom.SymbolTable 
  ( SymbolTable()
  , ast2st
  , propCodomConsistent
  , propBaseConsistent
  , propDomainsConsistent
  , propCtorsAllDiff
  , propFieldsSortConsistent)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- | test suite for the module
testSuite :: Test
testSuite = testGroup "symbol table consistency after completion" 
  [ testProperty "codomain consistency"   $ go propCodomConsistent
  , testProperty "base ctors consistency" $ go propBaseConsistent
  , testProperty "domains consistency"    $ go propDomainsConsistent
  , testProperty "no ctor duplicates"     $ go propCtorsAllDiff
  , testProperty "same fields same sorts" $ go propFieldsSortConsistent]

  where go :: (SymbolTable -> Bool) -> Module -> Bool
        go f = f . ast2st
