module Gom.CodeGen where

import Gom.SymbolTable
import Gom.Java
import Gom.Constants
import Gom.Config

import Control.Monad.Reader

import Gom.CodeGen.Common
import Gom.CodeGen.Abstract
import Gom.CodeGen.Mappings
import Gom.CodeGen.Strategies
import Gom.CodeGen.Sorts

-- | Compiles a symbol table into a Java hierarchy
st2java :: SymbolTable -> Config -> FileHierarchy
st2java = runGen compSt

-- | Generates the whole file hierarchy of the \"global\" symbol table.
compSt :: Gen FileHierarchy
compSt = do mn <- lower `liftM` askSt modName
            ds <- askSt definedSortsIds
            hs <- mapM compSort ds
            ac <- compAbstract
            tf <- compTomFile
            pr <- askConf package
            cs <- mapM compStrategy ds
            pc <- ifP $ Class "Parser" absParser
            lc <- ifP $ Class "Lexer" absLexer
            ps <- ifC $ Package "strategy" cs
            let always = [ac,tf,Package "types" (concat hs)] 
            return . wrap pr $ Package mn (always ++ pc ++ lc ++ ps)
  where -- wraps the package in the user-provided prefix hierarchy (-p option)
        wrap Nothing  h = h
        wrap (Just l) h = foldr w h l
        w p h = Package p [h]
        ifC x = do congrval <- askConf congr
                   case congrval of
                     NoCongr -> return []
                     _ ->       return [x]
        ifP x = ifConf parsers [x] []
         




