------------------------------------------------------------------
-- |
-- Module      : Gom.CodeGen.Common.Builtins
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

module Gom.CodeGen.Common.Builtins (
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

import Gom.CodeGen.Common.Helpers
import Gom.Sig

-- | List of supported java builtins
builtins :: [String]
builtins = ["boolean","int","char","double","float","long","String"]

-- | Check if some sort is a builtin.
isBuiltin :: SortId -> Bool
isBuiltin = (`elem` builtins) . idStr
 
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

[isBoolean,isInt,isChar,isDouble,isFloat,isLong,isString] = 
  map (\b x -> idStr x == b) builtins

qbuiltins :: [(String,Doc)]
qbuiltins = zip builtins (map text qts)
  where qts = ["java.lang.Boolean",
               "java.lang.Integer",
               "java.lang.Character",
               "java.lang.Double",
               "java.lang.Float",
               "java.lang.Long"] 

-- | Returns the qualified java type for builtin boxing.
qualifiedBuiltin :: SortId -> Doc
qualifiedBuiltin s = fromMaybe (pretty s) (idStr s `lookup` qbuiltins)

ibuiltins :: [(String,Doc)]
ibuiltins = zip builtins (map text toms)
  where toms = ["boolean.tom","int.tom","char.tom",
                "double.tom","float.tom","long.tom","string.tom"] 

-- | Returns the right .tom filename associated to a builtin.
builtinImport :: SortId -> Doc
builtinImport s = fromMaybe (pretty s) (idStr s `lookup` ibuiltins)

-- | @renderBuiltin s f b@ generates what is necessary to put
-- the representation of @f@ (field of sort @s@) in the buffer @b@.
renderBuiltin :: SortId -> Doc -> Doc -> Doc
renderBuiltin s f b 
  | isString s = text "renderString" <> parens (b <> comma <> f)
  | isChar   s = text "renderChar"   <> parens (b <> comma <> f)
  | otherwise  = rMethodCall b (text "append") [f]

