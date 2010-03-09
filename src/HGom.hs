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

import Common.Sig
import Common.Parser
import Common.Pretty ()
import Common.SymbolTable
import Common.FileGen
import HGom.Checker
import HGom.CodeGen
import HGom.Config

import System.Environment (getArgs)
import Text.PrettyPrint.Leijen (pretty)

#if TEST
import qualified HGom.UnitTests as T
import Test.Framework (defaultMainWithArgs)
#endif

hgomVersion :: String
hgomVersion = "Version 0.6 - March 2009"

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

-- | before parsing: checks all \"... and exit\" functions 
-- that don't require parsing
go1 :: Config -> [String] -> IO ()
go1 c n | help c              = putStrLn usage
        | version c           = putStrLn hgomVersion
#if TEST
        | Just as <- utests c = defaultMainWithArgs T.testSuite as
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
                Nothing -> gomain
                Just d  -> error (f ++ " contains errors:\n" ++ show d)
            | otherwise = gomain
  where gomain = chain c sig


-- | compilation chain for hgom
chain :: Config -> Module -> IO ()
chain conf = generateFileHierarchy (compact conf) . 
             flip st2java conf . 
             completeVariadics .  
             ast2st
