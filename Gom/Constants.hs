module Gom.Constants (
  toStringBody,
  abstractToStringBuilder,
  builtins,
  isBuiltin,
  qualifiedBuiltin
) where

import Text.PrettyPrint.Leijen
import Gom.Java
import Gom.Sig

-- | Full text of the toString method of moduleAbstractType.
toStringBody :: Doc
toStringBody =
 text "public String toString()" <+>
 (ibraces . rBody . map text)
    ["java.lang.StringBuilder buf = new java.lang.StringBuilder()",
     "toStringBuilder(buf)",
     "return buf.toString()"]

-- | Full prototype of the abstract method of moduleAbstractType.
abstractToStringBuilder :: Doc
abstractToStringBuilder = 
  text "public abstract void toStringBuilder(java.lang.StringBuilder buf);"

-- | List of supported java builtins
builtins :: [SortId]
builtins = map makeSortId ["int","String"]

-- | Check if some sort is a builtin.
isBuiltin :: SortId -> Bool
isBuiltin s = s `elem` builtins

qbuiltins :: [(SortId,Doc)]
qbuiltins = 
  zip builtins (map text qts)
  where qts = ["java.lang.Integer",
               "java.lang.Char"] 

-- | Returns the qualified java type for builtin boxing.
qualifiedBuiltin :: SortId -> Doc
qualifiedBuiltin s = maybe (pretty s) id (s `lookup` qbuiltins)
