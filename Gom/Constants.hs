module Gom.Constants where

import Text.PrettyPrint.Leijen
import Gom.Java
import Gom.Sig

toStringBody :: Doc
toStringBody =
 text "public String toString()" <+>
 (ibraces . rBody . map text)
    ["java.lang.StringBuilder buf = new java.lang.StringBuilder()",
     "toStringBuilder(buf)",
     "return buf.toString()"]

abstractToStringBuilder :: Doc
abstractToStringBuilder = 
  text "public abstract void toStringBuilder(java.lang.StringBuilder buf);"

builtins :: [SortId]
builtins = map makeSortId ["int","String"]

isBuiltin :: SortId -> Bool
isBuiltin s = s `elem` builtins
