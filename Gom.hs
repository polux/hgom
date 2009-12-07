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
--import System.Console.CmdArgs
import System.Console.GetOpt
    
data Flag = FHelp
          | FVersion 
          | FPackage String 
       deriving (Eq,Show)

{-
data Config = Config {
    version :: Bool,
    packagePrefix :: String,
    inputFile :: String
  }

m = mode $ Config {
      version = def &= text "show version number",
-}    
  
options :: [OptDescr Flag]
options =
  [Option []    ["help"]    (NoArg FHelp)                   "show this message",
   Option ['V'] ["version"] (NoArg FVersion)                "show version number",
   Option ['p'] ["package"] (ReqArg FPackage "packageName") "specify package name"]
    
compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv = 
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: hgom [OPTION...] file"

usage :: String -> IO ()
usage h = putStrLn (usageInfo h options)

main ::  IO ()
main = do (flags,n) <- getArgs >>= compilerOpts
          if FHelp `elem` flags 
           then usage ""
           else if FVersion `elem` flags
            then putStrLn "0.1"
            else case n of
             [f] -> go f
             []  -> usage "No input file specified.\n"
             _   -> usage "Too many input files specified.\n"

go :: String -> IO ()
go f = do sig <- parseModule `liftM` readFile f
          print $ pretty sig
          putStrLn ""
          case checkEverything sig of
            Nothing -> chain sig
            Just d  -> print $ d
  where chain :: Module -> IO ()
        chain = generateFileHierarchy . st2java . completeVariadics . ast2st
