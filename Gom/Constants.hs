module Gom.Constants (
  toStringBody,
  abstractToStringBuilder,
  toHaskellBody,
  abstractToHaskellBuilder,
  abstractSymbolName,
  abstractSharing,
  builtins,
  isBuiltin,
  isString,
  qualifiedBuiltin,
  builtinImport,
  renderStringMethod,
) where

import Data.Maybe(fromMaybe)
import Text.PrettyPrint.Leijen
import Gom.Java
import Gom.Sig

-- | Full text of the toString method of @moduleAbstractType@.
toStringBody :: Doc
toStringBody =
 text "public String toString()" <+>
 (ibraces . rBody . map text)
    ["java.lang.StringBuilder buf = new java.lang.StringBuilder()",
     "toStringBuilder(buf)",
     "return buf.toString()"]

-- | Full prototype of the abstract method of @moduleAbstractType@.
abstractToStringBuilder :: Doc
abstractToStringBuilder = 
  text "public abstract void toStringBuilder(java.lang.StringBuilder buf);"

-- | Full text of the toHaskell method of @moduleAbstractType@.
toHaskellBody :: Doc
toHaskellBody =
 text "public String toHaskell()" <+>
 (ibraces . rBody . map text)
    ["java.lang.StringBuilder buf = new java.lang.StringBuilder()",
     "toHaskellBuilder(buf)",
     "return buf.toString()"]

-- | Full prototype of the abstract method of @moduleAbstractType@.
abstractToHaskellBuilder :: Doc
abstractToHaskellBuilder = 
  text "public abstract void toHaskellBuilder(java.lang.StringBuilder buf);"

-- | Full prototype of the abstract methode of @moduleAbstractType@.
abstractSymbolName :: Doc
abstractSymbolName =
  text "public abstract String symbolName();"

-- | List of supported java builtins
builtins :: [SortId]
builtins = map makeSortId ["int","char","String"]

-- | Check if some sort is a builtin.
isBuiltin :: SortId -> Bool
isBuiltin = (`elem` builtins)

-- | Check if some sort is a java String
isString :: SortId -> Bool
isString = (== makeSortId "String")

qbuiltins :: [(SortId,Doc)]
qbuiltins = zip builtins (map text qts)
  where qts = ["java.lang.Integer",
               "java.lang.Character"] 

-- | Returns the qualified java type for builtin boxing.
qualifiedBuiltin :: SortId -> Doc
qualifiedBuiltin s = fromMaybe (pretty s) (s `lookup` qbuiltins)

ibuiltins :: [(SortId,Doc)]
ibuiltins = zip builtins (map text toms)
  where toms = ["int.tom","char.tom","string.tom"] 

-- | Returns the right .tom filename associated to a builtin.
builtinImport :: SortId -> Doc
builtinImport s = fromMaybe (pretty s) (s `lookup` ibuiltins)

renderStringMethod :: Doc
renderStringMethod = vcat $ map text
  ["public void renderString(java.lang.StringBuilder buf, String x) {",
   "  buf.append('\"');",
   "  for (int i = 0; i < x.length(); i++) {",
   "    char c = x.charAt(i);",
   "    switch (c) {",
   "      case '\\n':",
   "        buf.append('\\\\');",
   "        buf.append('n');",
   "        break;",
   "      case '\\t':",
   "        buf.append('\\\\');",
   "        buf.append('t');",
   "        break;",
   "      case '\\b':",
   "        buf.append('\\\\');",
   "        buf.append('b');",
   "        break;",
   "      case '\\r':",
   "        buf.append('\\\\');",
   "        buf.append('r');",
   "        break;",
   "      case '\\f':",
   "        buf.append('\\\\');",
   "        buf.append('f');",
   "        break;",
   "      case '\\\\':",
   "        buf.append('\\\\');",
   "        buf.append('\\\\');",
   "        break;",
   "      case '\\'':",
   "        buf.append('\\\\');",
   "        buf.append('\\'');",
   "        break;",
   "      case '\\\"':",
   "        buf.append('\\\\');",
   "        buf.append('\\\"');",
   "        break;",
   "      case '!':",
   "      case '@':",
   "      case '#':",
   "      case '$':",
   "      case '%':",
   "      case '^':",
   "      case '&':",
   "      case '*':",
   "      case '(':",
   "      case ')':",
   "      case '-':",
   "      case '_':",
   "      case '+':",
   "      case '=':",
   "      case '|':",
   "      case '~':",
   "      case '{':",
   "      case '}':",
   "      case '[':",
   "      case ']':",
   "      case ';':",
   "      case ':':",
   "      case '<':",
   "      case '>':",
   "      case ',':",
   "      case '.':",
   "      case '?':",
   "      case ' ':",
   "      case '/':",
   "        buf.append(c);",
   "        break;",
   "  ",
   "      default:",
   "        if (java.lang.Character.isLetterOrDigit(c)) {",
   "          buf.append(c);",
   "        } else {",
   "          buf.append('\\\\');",
   "          buf.append((char) ('0' + c / 64));",
   "          c = (char) (c % 64);",
   "          buf.append((char) ('0' + c / 8));",
   "          c = (char) (c % 8);",
   "          buf.append((char) ('0' + c));",
   "        }",
   "    }",
   "  }",
   "  buf.append('\"');",
   "}"]

-- | declaration of sharing related methods and fields for the
-- modNameAbstractType class
abstractSharing :: Doc
abstractSharing = vcat $ map text
  ["protected static final shared.SharedObjectFactory factory =",
   "   shared.SingletonSharedObjectFactory.getInstance();",
   "private int uniqueID;",
   "public int getUniqueIdentifier() {",
   "  return uniqueID;",
   "}",
   "public void setUniqueIdentifier(int uniqueID) {",
   "  this.uniqueID = uniqueID;",
   "}"]
