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
-- Portability : non-portable (requires GeneralizedNewtypeDeriving)
--
-- Entry point of hgom. 
--------------------------------------------------------------------

{-# LANGUAGE PatternGuards #-}

module Main (main,entryPoint) where

import Gom.Sig
import Gom.Parser
import Gom.Pretty ()
import Gom.Checker
import Gom.SymbolTable
import Gom.CodeGen
import Gom.OOMappingGen
import Gom.Config
import Gom.FileGen

import System.Environment (getArgs)
import Text.PrettyPrint.Leijen (pretty)

#if TEST
import qualified Gom.UnitTests as T
import Test.Framework (defaultMainWithOpts)
import Test.Framework.Runners.Options 
import Test.Framework.Options 
#endif

hgomVersion :: String
hgomVersion = "Version 0.4 - January 2009"

-- | @main = getArgs >>= entryPoint@
main ::  IO ()
main = getArgs >>= entryPoint

-- | entry poiny of hgom, separated from @main@ in order 
-- to make it callable by other programs
entryPoint :: [String] -> IO ()
entryPoint args =
  case gomOpts args of 
    Left err    -> paramsError err
    Right (c,n) -> go1 c n

#if TEST
-- | options for the test framework
opts :: Int -> RunnerOptions' Maybe
opts n = RunnerOptions Nothing (Just topts) Nothing
  where topts = TestOptions Nothing (Just n) Nothing Nothing
#endif

-- | before parsing: checks all \"... and exit\" functions 
-- that don't require parsing
go1 :: Config -> [String] -> IO ()
go1 c n | help c              = putStrLn usage
        | version c           = putStrLn hgomVersion
#if TEST
        | Just k <- utests c  = defaultMainWithOpts T.testSuite (opts k)
#endif
        | otherwise           = case n of
            [f] -> go2 f c 
            []  -> paramsError "No input file specified.\n"
            _   -> paramsError "Too many input files specified.\n"

-- | parsing: checks that all went well and calls @go3@
go2 :: FilePath -> Config -> IO ()
go2 f c = do em <- parseModule `fmap` readFile f
             case em of Left err -> error ("parse error at " ++ show err)
                        Right m  -> go3 f c m

-- | after parsing:
--
--    * checks all \"... and exit\" functions that require parsing
--
--    * checks well-formedness
--
--    * runs compilation chain
go3 :: FilePath -> Config -> Module -> IO ()
go3 f c sig | prprint c = print $ pretty sig
            | checker c = case checkEverything sig of
                Nothing -> chain sig c
                Just d  -> error (f ++ " contains errors:\n" ++ show d)
            | otherwise = chain sig c

-- | compilation chain
chain :: Module -> Config -> IO ()
chain m conf = generateFileHierarchy (compact conf) .  flip (if oomapping conf then st2oomapping else st2java) conf .  (if oomapping conf then id else completeVariadics) .  ast2st $ m
