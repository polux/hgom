------------------------------------------------------------------
-- |
-- Module      : HGom.UnitTests
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

module HGom.UnitTests (testSuite) where

import Common.Pretty ()
import Common.Random ()
import Common.Sig
import Common.Parser
import Common.Checker

-- imported test suites
import qualified Common.SymbolTable

-- for t*.gom testing
import Test.Framework (Test,testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit.Base((@?=))
import System.FilePath((</>))

-- generated by cabal
import Paths_hgom (getDataFileName)

-- for generated parser tesing
import System.Process(rawSystem)
import Data.Char(toLower)
import System.Directory
import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.Exit

-- for generated java testing
import System.FilePath.Glob
import Data.List(intercalate)
import Control.Monad(liftM2)
import Data.Maybe(isNothing)
import System.Process(readProcessWithExitCode)

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
  concat [map (cook Never   ) (files 1 1 6),
          map (cook Checking) (files 2 0 9), 
          map (cook Parsing ) (files 3 1 7)]
  where files :: Int -> Int -> Int -> [FilePath]
        files n l h = map fname [l..h]
          where fname i = "test" </> "data" </> 
                          "t" ++ show n ++ 
                          "_" ++ show i ++ ".gom"
        cook f s = testCase (msg s f) (test f s)
        test f s = fileFailsDuring s >>= (@?= f)
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
getTempDir = getTemporaryDirectory >>= getTemp 0
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
  if r then removeDirectoryRecursive tmp
       else putStrLn $ "guilty files kept in " ++ tmp
  return r

-- | True iff the module passes the check phase
checks :: Module -> Bool
checks = isNothing . checkEverything

-- | classpath needed for tests
dataClassPath :: IO String
dataClassPath = do
  j1 <- getDataFileName $ "test" </> "data" </> "tom_runtime.jar"
  j2 <- getDataFileName $ "test" </> "data" </> "shared-objects.jar"
  return $ j1 ++ ":" ++ j2 ++ ":"

-- | runs javac with args, returns exit status code
javac :: [String] -> IO ExitCode
javac args = do 
  cp <- dataClassPath
  (st,_,_) <- readProcessWithExitCode "javac" (["-cp",cp] ++ args) ""
  return st

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
        st <- javac ["Test.java"]
        return $ st == ExitSuccess)
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
        st <- javac jfs
        return $ st == ExitSuccess)

-- | cross modules quickcheck tests
crossModuleSuite :: Test
crossModuleSuite = testGroup "cross module properties" 
  [testProperty "parse . pretty = id" propParsePretty,
   check flags1, check flags2, 
   testProperty "generated parse . generated pretty = id" propGenParsePretty]
  where check fs = testProperty (mes fs) (testChecker fs)
        flags1 = ["-r","-d","-s","-h","-c","same"]
        flags2 = ["--noSharing"]
        mes fs = "check ok => compilable java (" ++ intercalate " " fs ++ ")"

-- | all tests
testSuite :: [Test]
testSuite = [regressionSuite,
             Common.SymbolTable.testSuite,
             crossModuleSuite]
