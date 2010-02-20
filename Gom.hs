------------------------------------------------------------------
-- |
-- Module      : Main
-- Copyright   : (c) Paul Brauner 2009
--               (c) Emilie Balland 2009
--               (c) INRIA 2009
-- Licence     : GPL (see COPYING)
--
-- Maintainer  : paul.brauner\@inria.fr
-- Stability   : provisional
-- Portability : non-portable (requires GeneralizedNewtypeDeriving)
--
-- Entry point of hgom. 
--------------------------------------------------------------------

{-# LANGUAGE PatternGuards #-}

module Main (main) where

import Gom.Sig
import Gom.Parser
import Gom.Pretty ()
import Gom.Checker
import Gom.SymbolTable
import Gom.CodeGen
import Gom.Config
import Gom.FileGen

import System.Environment (getArgs)
import Text.PrettyPrint.Leijen (pretty)
import Test.Framework (defaultMainWithOpts)
import Test.Framework.Runners.Options 
import Test.Framework.Options 

import qualified Gom.UnitTests as T

hgomVersion :: String
hgomVersion = "Version 0.4 - January 2009"

main ::  IO ()
main = do args <- getArgs 
          case gomOpts args of 
            Left err -> paramsError err
            Right (c,n) -> go1 c n

-- | options for the test framework
opts :: Int -> RunnerOptions' Maybe
opts n = RunnerOptions Nothing (Just topts) Nothing
  where topts = TestOptions Nothing (Just n) Nothing Nothing

go1 :: Config -> [String] -> IO ()
go1 c n | help c              = putStrLn usage
        | version c           = putStrLn hgomVersion
        | Just k <- utests c  = defaultMainWithOpts T.testSuite (opts k)
        | otherwise           = case n of
            [f] -> go2 f c 
            []  -> paramsError "No input file specified.\n"
            _   -> paramsError "Too many input files specified.\n"

go2 :: String -> Config -> IO ()
go2 f c = go2' =<< parseModule `fmap` readFile f
  where go2' sig | prprint c = print $ pretty sig
                 | checker c = case checkEverything sig of
                     Nothing -> chain sig c
                     Just d  -> error (f ++ " contains errors:\n" ++ show d)
                 | otherwise = chain sig c

chain :: Module -> Config -> IO ()
chain m conf = generateFileHierarchy (compact conf) . 
               flip st2java conf . 
               completeVariadics . 
               ast2st $ m
