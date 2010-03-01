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

module Gom.UnitTests (testSuite) where

import Gom.Pretty ()
import Gom.Random ()
import Gom.Sig
import Gom.Parser
import Gom.Checker

-- imported test suites
import qualified Gom.SymbolTable

-- for t*.gom testing
import Test.Framework (Test,testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit.Base((@?=))
import System.FilePath((</>))

-- generated by cabal
import Paths_hgom (getDataFileName)

-- for generated parser tesing
import System.Cmd(rawSystem)
import Data.Char(toLower)
import System.Directory
import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.Exit

-- for generated java testing
import System.FilePath.Glob
import Data.List(intercalate)
import Control.Monad(when,liftM2)
import Data.Maybe(isNothing)

-- | models at which step of the chain a module failed
data FailsDuring = Parsing | Checking | Never deriving (Show,Eq)

-- | checks how far in the chain a module gets without 
-- raising a failure of some sort
failsDuring :: String -> FailsDuring
failsDuring s = 
  case parseModule s of
    Left  _ -> Parsing
    Right m -> case checkEverything m of
      Nothing -> Never
      Just _  -> Checking

-- | run failsDuring on some relative filePath
fileFailsDuring :: FilePath -> IO FailsDuring
fileFailsDuring f = do af <- getDataFileName f
                       failsDuring `fmap` readFile af

-- | regression suite
regressionSuite :: Test
regressionSuite = testGroup "regression tests" $

  map cook [("t1.gom",  Never   ), ("t2.gom",  Never   ),
            ("t3.gom",  Checking), ("t4.gom",  Parsing ),
            ("t5.gom",  Parsing ), ("t6.gom",  Parsing ),
            ("t7.gom",  Parsing ), ("t8.gom",  Parsing ),
            ("t9.gom",  Parsing ), ("t10.gom", Checking),
            ("t11.gom", Never   ), ("t12.gom", Never   ),
            ("t13.gom", Checking), ("t14.gom", Never   )]

  where cook (s,f) = testCase (msg s f) (test s f)
        test s f = fileFailsDuring (prefix s) >>= (@?= f)
        prefix s = "test" </> "data" </> s
        msg s Never    = s ++ " is valid"
        msg s Parsing  = s ++ " fails parsing"
        msg s Checking = s ++ " fails checking"

-- | checks that @parse . pretty = id@
propParsePretty :: Module -> Bool
propParsePretty m = 
  case parseModule (show m) of
    Left  _  -> False
    Right m' -> m == m'

getTempDir :: IO FilePath
getTempDir = do
  tmp <- getTemporaryDirectory
  getTemp 0 tmp
  where getTemp :: Integer -> FilePath -> IO FilePath
        getTemp n tmp = do
          b <- doesDirectoryExist htmp
          if b then getTemp (n+1) tmp
               else do createDirectory htmp
                       return htmp
          where htmp = tmp </> "hgom-tmp" ++ show n
    

-- | @doInTempDir a@ creates a directory, change working directory 
-- to it, perform @a@ and comes back to original directory
doInTempDir :: IO Bool -> IO Bool
doInTempDir a = do
  tmp <- getTempDir
  cur <- getCurrentDirectory
  setCurrentDirectory tmp
  r <- a
  setCurrentDirectory cur
  when r $ removeDirectoryRecursive tmp
  return r

-- | True iff the module passes the check phase
checks :: Module -> Bool
checks m = isNothing (checkEverything m)

-- | test that the generated parser is correct w.r.t. 
-- the generated pretty printer (@fromString(x.toString()) == x@)
propGenParsePretty :: Property
propGenParsePretty = monadicIO $ do
  sig <- pick (arbitrary `suchThat` liftM2 (&&) hasSort checks)
  case sig of
    Module m _ (SortDef s _ _:_) ->
      let pack = map toLower m 
      in assert =<< (run . doInTempDir $ do 
        writeFile "Test.gom" $ show sig
        _ <- rawSystem "hgom" ["-r","Test.gom"]
        writeFile "Test.java" $ template pack (show s)
        cp <- getDataFileName $ "test" </> "data" </> "tom-runtime-full.jar:"
        st <- rawSystem "javac" ["-cp",cp,"Test.java"]
        let res = (st == ExitSuccess)
        return res)
    _ -> error "never happens"

  where hasSort = not . null . sortDefs
        template pack s = unlines
          ["import " ++ pack ++ ".types.*;",
           "public class Test {",
           "  public static void main(String[] args) {",
           "    for(int i=0; i<10; i++) {",
           "      " ++ s ++ " t = " ++ s ++ ".makeRandom(i);",
           "      if (!" ++ s ++ ".fromString(t.toString()).equals(t))",
           "        System.exit(1);",
           "    }",
           "  }",
           "}"]

-- | @testChecker opts@ generates a random module that passes the checker
-- phase, and return True iff javac successes on the generated files
testChecker :: [String] -> Property
testChecker opts = monadicIO $ do
  sig <- pick (arbitrary `suchThat` checks)
  case sig of 
    Module m _ _ ->
      let pack = map toLower m
      in assert =<< (run . doInTempDir $ do
        writeFile "Test.gom" $ show sig
        _ <- rawSystem "hgom" ("Test.gom":opts)
        jfs <- globDir1 (compile $ "**" </> "*.java") pack
        cp <- getDataFileName $ "test" </> "data" </> "tom-runtime-full.jar:"
        st <- rawSystem "javac" $ ["-cp",cp]++jfs
        let res = (st == ExitSuccess)
        return res)

-- | cross modules quickcheck tests
crossModuleSuite :: Test
crossModuleSuite = testGroup "cross module properties" 
  [check flags3, check flags2, check flags1,
   testProperty "parse . pretty = id" propParsePretty,
   testProperty "generated parse . generated pretty = id" propGenParsePretty]
  where check fs = testProperty (mes fs) (testChecker fs)
        flags1 = ["-r","-d","-s","-h"]
        flags2 = ["--noSharing"]
        flags3 = ["-j"]
        mes fs = "check ok => compilable java (" ++ intercalate " " fs ++ ")"

-- | all tests
testSuite :: [Test]
testSuite = [regressionSuite,
             Gom.SymbolTable.testSuite,
             crossModuleSuite]
