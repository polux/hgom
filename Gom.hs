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

module Main (main) where

import Gom.Sig
import Gom.Parser
import Gom.Pretty ()
import Gom.Checker
import Gom.SymbolTable
import Gom.CodeGen
import Gom.RenderCode
import Gom.Config

import System.Environment(getArgs)
import Text.PrettyPrint.Leijen (pretty)
import Control.Monad(liftM)

hgomVersion :: String
hgomVersion = "Version 0.3.1 - December 2009"

main ::  IO ()
main = do args <- getArgs 
          case gomOpts args of 
            Left err -> paramsError err
            Right (c,n) ->
              if help c 
                then putStrLn usage
                else if version c
                  then putStrLn hgomVersion
                  else case n of
                    [f] -> go f c 
                    []  -> paramsError "No input file specified.\n"
                    _   -> paramsError "Too many input files specified.\n"

go :: String -> Config -> IO ()
go f c = do sig <- parseModule `liftM` readFile f
            if prprint c 
              then print $ pretty sig
              else if checker c 
                then case checkEverything sig of
                  Nothing -> chain sig c
                  Just d  -> error (f ++ " contains errors:\n" ++ show d)
                else chain sig c

  where chain :: Module -> Config -> IO ()
        chain m conf = generateFileHierarchy (compact conf) . 
                       flip st2java conf . 
                       completeVariadics . 
                       ast2st $ m
