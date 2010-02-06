------------------------------------------------------------------
-- |
-- Module      : Gom.CodeGen.Builtins
-- Copyright   : (c) Paul Brauner 2009
--               (c) Emilie Balland 2009
--               (c) INRIA 2009
-- Licence     : GPL (see COPYING)
--
-- Maintainer  : paul.brauner@inria.fr
-- Stability   : provisional
-- Portability : non-portable (requires generalized newtype deriving)
--
-- Builtins handling.
--------------------------------------------------------------------

module Gom.CodeGen.Builtins (
  builtins,
  isBuiltin,
  isBoolean,
  isInt,
  isChar,
  isDouble,
  isFloat,
  isLong,
  isString,
  qualifiedBuiltin,
  builtinImport,
  renderBuiltin
) where

import Data.Maybe(fromMaybe)
import Text.PrettyPrint.Leijen

import Gom.CodeGen.Helpers
import Gom.Sig

-- | List of supported java builtins
builtins :: [SortId]
builtins = map makeSortId 
  ["boolean","int","char","double","float","long","String"]

-- | Check if some sort is a builtin.
isBuiltin :: SortId -> Bool
isBuiltin = (`elem` builtins)
 
-- | Check if some sort is a java bool
isBoolean :: SortId -> Bool
-- | Check if some sort is a java int
isInt :: SortId -> Bool
-- | Check if some sort is a java char
isChar :: SortId -> Bool
-- | Check if some sort is a java double
isDouble :: SortId -> Bool
-- | Check if some sort is a java float
isFloat :: SortId -> Bool
-- | Check if some sort is a java long
isLong :: SortId -> Bool
-- | Check if some sort is a java String
isString :: SortId -> Bool

[isBoolean,isInt,isChar,isDouble,isFloat,isLong,isString] = map (==) builtins

qbuiltins :: [(SortId,Doc)]
qbuiltins = zip builtins (map text qts)
  where qts = ["java.lang.Boolean",
               "java.lang.Integer",
               "java.lang.Character",
               "java.lang.Double",
               "java.lang.Float",
               "java.lang.Long"] 

-- | Returns the qualified java type for builtin boxing.
qualifiedBuiltin :: SortId -> Doc
qualifiedBuiltin s = fromMaybe (pretty s) (s `lookup` qbuiltins)

ibuiltins :: [(SortId,Doc)]
ibuiltins = zip builtins (map text toms)
  where toms = ["boolean.tom","int.tom","char.tom","double.tom","float.tom","long.tom","string.tom"] 

-- | Returns the right .tom filename associated to a builtin.
builtinImport :: SortId -> Doc
builtinImport s = fromMaybe (pretty s) (s `lookup` ibuiltins)

-- | @renderBuiltin s f b@ generates what is necessary to put
-- the representation of @f@ (field of sort @s@) in the buffer @b@.
renderBuiltin :: SortId -> FieldId -> Doc -> Doc
renderBuiltin s f b 
  | isString s = text "renderString" <> parens (b <> comma <> pretty f)
  | isChar   s = text "renderChar"   <> parens (b <> comma <> pretty f)
  | otherwise  = rMethodCall b (text "append") [pretty f]

