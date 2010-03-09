------------------------------------------------------------------
-- |
-- Module      : OOMappings.UnitTests
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

module OOMappings.UnitTests (testSuite) where

import Common.Pretty ()
import Common.Random ()
import Common.Sig
import OOMappings.Checker

-- for t*.gom testing
import Test.Framework (Test,testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
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
import Data.Maybe(isNothing)
import System.Process(readProcessWithExitCode)

-- | models at which step of the chain a module failed
data FailsDuring = Parsing | Checking | Never deriving (Show,Eq)

getTempDir :: IO FilePath
getTempDir = getTemporaryDirectory >>= getTemp 0
  where getTemp :: Integer -> FilePath -> IO FilePath
        getTemp n tmp = do
          b <- doesDirectoryExist htmp
          if b then getTemp (n+1) tmp
               else do createDirectory htmp
                       return htmp
          where htmp = tmp </> "oom-tmp" ++ show n

-- | True iff the module passes the check phase
checks :: Module -> Bool
checks = isNothing . checkEverything

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
        _ <- rawSystem "oomappings" ("Test.gom":opts)
        jfs <- globDir1 (compile $ "**" </> "*.java") pack
        st <- javac jfs
        return $ st == ExitSuccess)

-- | cross modules quickcheck tests
crossModuleSuite :: Test
crossModuleSuite = testGroup "cross module properties" [check flags1] 
  where check fs = testProperty (mes fs) (testChecker fs)
        flags1 = []
        mes fs = "check ok => compilable java (" ++ intercalate " " fs ++ ")"

-- | all tests
testSuite :: [Test]
testSuite = [crossModuleSuite]
