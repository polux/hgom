------------------------------------------------------------------
-- |
-- Module      : Main
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

module Main where

import Gom.Pretty ()
import Gom.Random ()
import Gom.Sig
import Gom.Parser
import Gom.Checker

-- imported test suites
import qualified Gom.SymbolTableTests

-- for t*.gom testing
import Test.Framework (Test,testGroup,defaultMainWithArgs)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit.Base((@?=))
import System.FilePath((</>))

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
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

-- for the main function
import System.Environment (getArgs)

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
fileFailsDuring f = failsDuring `fmap` readFile f

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


-- | Compiles the provided java files and returns True if the compilation was
-- successful. If the compilation fails, dumps javac's stderr on stderr.
compileJava :: String -> [FilePath] -> IO Bool
compileJava _         [] = return True
compileJava classpath files = do
  let args = ["-cp", classpath] ++ files
  (st,_,err) <- readProcessWithExitCode "javac" args ""
  if (st /= ExitSuccess)
    then hPutStrLn stderr $ printf
           "Error while running javac %s:\n%s"
           (unwords args)
           err
    else return ()
  return (st == ExitSuccess)

-- | test that the generated parser is correct w.r.t. 
-- the generated pretty printer (@fromString(x.toString()) == x@)
propGenParsePretty :: String -> Property
propGenParsePretty classpath = monadicIO $ do
  sig <- pick (arbitrary `suchThat` liftM2 (&&) hasSort checks)
  case sig of
    Module m _ (SortDef s _ _:_) ->
      let pack = map toLower m 
      in assert =<< (run . doInTempDir $ do 
        writeFile "Test.gom" $ show sig
        _ <- rawSystem "hgom" ["-r","Test.gom"]
        writeFile "Test.java" $ template pack (show s)
        compileJava classpath ["Test.java"])
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
testChecker :: String -> [String] -> Property
testChecker classpath opts = monadicIO $ do
  sig <- pick (arbitrary `suchThat` checks)
  case sig of 
    Module m _ _ ->
      let pack = map toLower m
      in assert =<< (run . doInTempDir $ do
        writeFile "Test.gom" $ show sig
        _ <- rawSystem "hgom" ("Test.gom":opts)
        jfs <- globDir1 (compile $ "**" </> "*.java") pack
        compileJava classpath jfs)

-- | cross modules quickcheck tests
crossModuleSuite :: String -> Test
crossModuleSuite classpath = testGroup "cross module properties" 
  [ testProperty "parse . pretty = id" propParsePretty
  , check flags1, check flags2, check flags3
  , testProperty 
      "generated parse . generated pretty = id" 
       (propGenParsePretty classpath)
  ]
  where check fs = testProperty (mes fs) (testChecker classpath fs)
        flags1 = ["-r","-d","-s","-h","-c","same"]
        flags2 = ["--noSharing"]
        flags3 = ["-j"]
        mes fs = "check ok => compilable java (" ++ intercalate " " fs ++ ")"

-- | all tests
testSuite :: String -> [Test]
testSuite classpath = 
  [ regressionSuite
  , Gom.SymbolTableTests.testSuite
  , crossModuleSuite classpath
  ]

-- | Classpath of the tom_runtime and shared-objects jars. Assumes that
-- tests are run from the toplevel directory (which stack and cabal do).
dataClassPath :: IO String
dataClassPath = do
  root <- getCurrentDirectory
  let j1 = root </> "test" </> "data" </> "tom_runtime.jar"
  let j2 = root </> "test" </> "data" </> "shared-objects.jar"
  return $ j1 ++ ":" ++ j2 ++ ":"

main :: IO ()
main = do
  args <- getArgs
  classpath <- dataClassPath
  defaultMainWithArgs (testSuite classpath) args
