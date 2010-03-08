-------------------------------------------------------------------
-- |
-- Module      : Gom.Common.FileGen
-- Copyright   : (c) Paul Brauner 2009
--               (c) Emilie Balland 2009
--               (c) INRIA 2009
-- Licence     : GPL (see COPYING)
--
-- Maintainer  : paul.brauner@inria.fr
-- Stability   : provisional
-- Portability : non-portable (requires generalized newtype deriving)
--
-- Exposes 'FileHierarchy', a datatype representing a file hierarchy made of
-- packages, java files and tom files as well as an impure function for
-- actually generating the hierarchy.
--------------------------------------------------------------------

module Gom.Common.FileGen (
  FileHierarchy(..),
  generateFileHierarchy
) where

import Text.PrettyPrint.Leijen
import Data.List(intercalate)
import System.FilePath hiding ((</>))
import System.Directory
import System.IO

-- | Represents a hierarchy of Java packages, Java classes and .tom files.
-- Constructors and field names speak for themselves.
data FileHierarchy
  = Package { 
    packageName    :: String,
    packageContent :: [FileHierarchy]
  }
  | Class {
    className    :: String,
    classContent :: Doc -- ^ without @package ...@ at the begining
  }
  | Tom {
    fileName    :: String, -- ^ without the extension
    fileContent :: Doc 
  }

-- | Actually generates files on disk. If the first argument is
-- true, generates compact code.
generateFileHierarchy :: Bool -> FileHierarchy -> IO ()
generateFileHierarchy c h = do cur <- getCurrentDirectory
                               generateHierarchyIn c cur [] h

-- | @generateHierarchyIn compact dir [pack]@ generates the files in
-- directory @dir@ in package @pack@. Adds proper @package ...@ on top of java
-- files. If @compatc@ is true, the generated code is more compact.
--
-- GenMonad usage : @generateFileHierarchyIn compact dir []@
generateHierarchyIn :: Bool -> FilePath -> [String] -> FileHierarchy -> IO ()
generateHierarchyIn cp dir pac h = go h
  where go (Package n hs) = let ndir = dir `combine` n
                                npac = pac ++ [n]       
                            in do createDirectoryIfMissing False ndir
                                  mapM_ (generateHierarchyIn cp ndir npac) hs
        go (Class n b)    = let fn = dir `combine` (n `addExtension` "java")
                                fb = rFullClass pac b
                            in renderInFile fn fb
        go (Tom n b)      = let fn = dir `combine` (n `addExtension` "tom")
                            in renderInFile fn b
        renderInFile n b  = do hdl <- openFile n WriteMode
                               displayIO hdl (rdr b) 
                               hClose hdl
        rdr = if cp then renderCompact else renderPretty 0.6 80 

-- | Adds package name at the top of a class declaration.
rFullClass
  :: [String] -- ^ package
  -> Doc      -- ^ class content
  -> Doc
rFullClass [] bd = bd
rFullClass pk bd = text "package" <+> text (intercalate "." pk) <>
                   semi <> linebreak <$> bd

