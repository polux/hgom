module Main (main) where

import Gom.Sig
import Gom.Parser
import Gom.Pretty ()
import Gom.Checker
import Gom.SymbolTable
import Gom.CodeGen
import Gom.Java
import Gom.Config

import System.Environment(getArgs)
import Text.PrettyPrint.Leijen (pretty)
import Control.Monad(liftM)

main ::  IO ()
main = do (c,n) <- getArgs >>= gomOpts
          if help c 
            then putStrLn usage
            else if version c
              then putStrLn "Version 0.3.1 - December 2009 - Copyrigth (c) INRIA"
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
