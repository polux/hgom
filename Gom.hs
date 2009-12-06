module Main (main) where

import Gom.Sig
import Gom.Parser
import Gom.Pretty ()
import Gom.Checker
import Gom.SymbolTable
import Gom.CodeGen
import Gom.Java

import System.Environment(getArgs)
import Text.PrettyPrint.Leijen
import Control.Monad(liftM)

main ::  IO ()
main = do [f] <- getArgs
          sig <- parseModule `liftM` readFile f
          print $ pretty sig
          putStrLn ""
          case checkEverything sig of
            Nothing -> chain sig
            Just d  -> print $ d

chain :: Module -> IO ()
chain = generateFileHierarchy . st2java . completeVariadics . ast2st
