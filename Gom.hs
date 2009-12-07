module Main (main) where

import Gom.Sig
import Gom.Parser
import Gom.Pretty ()
import Gom.Checker
import Gom.SymbolTable
import Gom.CodeGen
import Gom.Java

import System.Environment(getArgs)
import Text.PrettyPrint.Leijen (pretty)
import Control.Monad(liftM)
import System.Console.GetOpt
    
data Flag = FHelp
          | FVersion 
          | FPackage String 
       deriving (Eq,Show)

options :: [OptDescr Flag]
options =
  [Option []    ["help"]    (NoArg FHelp)                   "show this message",
   Option ['V'] ["version"] (NoArg FVersion)                "show version number",
   Option ['p'] ["package"] (ReqArg FPackage "packageName") "specify package name"]

usage :: String
usage = "Usage: hgom [OPTION...] file"
 
compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv = 
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo usage options))

printUsage :: String -> IO ()
printUsage h = putStrLn (usageInfo h options)

main ::  IO ()
main = do (flags,n) <- getArgs >>= compilerOpts
          if FHelp `elem` flags 
           then printUsage usage
           else if FVersion `elem` flags
            then putStrLn "version 0.1 - december 2009 - copyrigth INRIA"
            else case n of
             [f] -> go f
             []  -> printUsage "No input file specified.\n"
             _   -> printUsage "Too many input files specified.\n"

go :: String -> IO ()
go f = do sig <- parseModule `liftM` readFile f
          print $ pretty sig
          putStrLn ""
          case checkEverything sig of
            Nothing -> chain sig
            Just d  -> print $ d
  where chain :: Module -> IO ()
        chain = generateFileHierarchy . st2java . completeVariadics . ast2st
