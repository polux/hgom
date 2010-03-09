-------------------------------------------------------------------
-- |
-- Module      : HGom.CodeGen
-- Copyright   : (c) Paul Brauner 2009
--               (c) Emilie Balland 2009
--               (c) INRIA 2009
-- Licence     : GPL (see COPYING)
--
-- Maintainer  : paul.brauner@inria.fr
-- Stability   : provisional
-- Portability : non-portable (requires generalized newtype deriving)
--
-- Generation of a file hierarchy from a symbol table.
-------------------------------------------------------------------- 

module HGom.CodeGen (st2java) where

import Data.Char(toLower)

import Common.SymbolTable
import Common.FileGen
import Common.CodeGen
import HGom.CodeGen.HGenMonad
import HGom.CodeGen.Abstract
import HGom.CodeGen.Mappings
import HGom.CodeGen.Strategies
import HGom.CodeGen.Sorts
import HGom.Config

-- | Compiles a symbol table into a Java hierarchy
st2java :: SymbolTable -> Config -> FileHierarchy
st2java =  runGen  compSt

-- | Generates the whole file hierarchy of the \"global\" symbol table.
compSt :: HGen FileHierarchy
compSt = do mn <- map toLower `fmap` askSt modName
            ds <- askSt definedSortsIds
            hs <- mapM compSort ds
            ac <- compAbstract
            tfs <- compTomFiles
            pr <- askConf package
            cs <- mapM compStrategy ds
            pc <- ifP $ Class "Parser" absParser
            lc <- ifP $ Class "Lexer" absLexer
            ps <- ifC $ Package "strategy" cs
            return . wrap pr . Package mn $
              ac : (Package "types" (concat hs)) : concat [tfs, pc, lc, ps]
  where -- wraps the package in the user-provided prefix hierarchy (-p option)
        wrap Nothing  h = h
        wrap (Just l) h = foldr w h l where w p r = Package p [r]
        ifC x = do congrval <- askConf congr
                   return $ case congrval of
                     NoCongr -> []
                     _       -> [x]
        ifP x = ifConf parsers [x] []
