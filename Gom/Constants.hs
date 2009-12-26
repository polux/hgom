module Gom.Constants (
  toStringBody,
  abstractToStringBuilder,
  toHaskellBody,
  abstractToHaskellBuilder,
  abstractSymbolName,
  abstractSharing,
  absParser,
  absLexer,
  hashCodeMethod,
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

-- | declaration of the @hashCode@ method which returns the private 
-- @hashCode@ member of a constructor
hashCodeMethod :: Doc
hashCodeMethod = vcat $ map text 
  ["public final int hashCode() {",
   "  return hashCode;",
   "}"]

-- | The parser class generated in package @mod@.
absParser :: Doc
absParser = vcat $ map text 
  ["public class Parser {",
   "  private Lexer lex;",
   "  private Lexer.Token cur;",
   "  public Parser(String s) {",
   "    this.lex = new Lexer(new java.io.StringReader(s));",
   "    next();",
   "  }",
   "  final void next() {",
   "    try { cur = lex.yylex(); }",
   "    catch (java.io.IOException e) {",
   "     throw new RuntimeException();",
   "    }",
   "  }",
   "  public final String parseString() {",
   "    if (cur.type == Lexer.TokenType.STRING) {",
   "      String res = cur.string;",
   "      next();",
   "      return res;",
   "    }",
   "    else throw new RuntimeException();",
   "  }",
   "  public final int parseint() {",
   "    if (cur.type == Lexer.TokenType.INTEGRAL) {",
   "      int res = Integer.valueOf(cur.string);",
   "      next();",
   "      return res;",
   "    }",
   "    else throw new RuntimeException();",
   "  }",
   "  public final long parselong() {",
   "    if (cur.type == Lexer.TokenType.INTEGRAL) {",
   "      long res = Long.valueOf(cur.string);",
   "      next();",
   "      return res;",
   "    }",
   "    else throw new RuntimeException();",
   "  }",
   "  public final String parseId() {",
   "    if (cur.type == Lexer.TokenType.IDENTIFIER) {",
   "      String res = cur.string;",
   "      next();",
   "      return res;",
   "    }",
   "    else throw new RuntimeException();",
   "  }",
   "  public final void parseLpar() { ",
   "    if (cur.type == Lexer.TokenType.LPAR) {",
   "      next();",
   "      return;",
   "    }",
   "    else throw new RuntimeException();",
   "  }",
   "  public final void parseRpar() {",
   "    if (cur.type == Lexer.TokenType.RPAR) {",
   "      next();",
   "      return;",
   "    }",
   "    else throw new RuntimeException();",
   "  }",
   "  public final void parseComma() {",
   "    if (cur.type == Lexer.TokenType.COMMA) {",
   "      next();",
   "      return;",
   "    }",
   "    else throw new RuntimeException();",
   "  }",
   "}"]

-- | The lexer class generated in package @mod@.
absLexer :: Doc
absLexer = vcat $ map text 
  ["/**",
   " * This class is a scanner generated by ",
   " * <a href=\"http://www.jflex.de/\">JFlex</a> 1.4.2",
   " */",
   "public class Lexer {",
   "  public static final int YYEOF = -1;",
   "  private static final int ZZ_BUFFERSIZE = 16384;",
   "  public static final int STRING = 2;",
   "  public static final int YYINITIAL = 0;",
   "  private static final int ZZ_LEXSTATE[] = { ",
   "     0,  0,  1, 1",
   "  };",
   "  private static final String ZZ_CMAP_PACKED = ",
   "    \"\\11\\5\\1\\3\\1\\2\\1\\0\\1\\3\\1\\1\\16\\5\\4\\0\\1\\3\\1\\0\"+",
   "    \"\\1\\13\\1\\0\\1\\4\\3\\0\\1\\10\\1\\11\\2\\0\\1\\12\\3\\0\\1\\6\"+",
   "    \"\\11\\7\\7\\0\\32\\4\\1\\0\\1\\14\\2\\0\\1\\4\\1\\0\\15\\4\\1\\16\"+",
   "    \"\\3\\4\\1\\17\\1\\4\\1\\15\\6\\4\\4\\0\\41\\5\\2\\0\\4\\4\\4\\0\"+",
   "    \"\\1\\4\\2\\0\\1\\5\\7\\0\\1\\4\\4\\0\\1\\4\\5\\0\\27\\4\\1\\0\"+",
   "    \"\\37\\4\\1\\0\\u013f\\4\\31\\0\\162\\4\\4\\0\\14\\4\\16\\0\\5\\4\\11\\0\"+",
   "    \"\\1\\4\\21\\0\\130\\5\\5\\0\\23\\5\\12\\0\\1\\4\\13\\0\\1\\4\\1\\0\"+",
   "    \"\\3\\4\\1\\0\\1\\4\\1\\0\\24\\4\\1\\0\\54\\4\\1\\0\\46\\4\\1\\0\"+",
   "    \"\\5\\4\\4\\0\\202\\4\\1\\0\\4\\5\\3\\0\\105\\4\\1\\0\\46\\4\\2\\0\"+",
   "    \"\\2\\4\\6\\0\\20\\4\\41\\0\\46\\4\\2\\0\\1\\4\\7\\0\\47\\4\\11\\0\"+",
   "    \"\\21\\5\\1\\0\\27\\5\\1\\0\\3\\5\\1\\0\\1\\5\\1\\0\\2\\5\\1\\0\"+",
   "    \"\\1\\5\\13\\0\\33\\4\\5\\0\\3\\4\\15\\0\\4\\5\\14\\0\\6\\5\\13\\0\"+",
   "    \"\\32\\4\\5\\0\\13\\4\\16\\5\\7\\0\\12\\5\\4\\0\\2\\4\\1\\5\\143\\4\"+",
   "    \"\\1\\0\\1\\4\\10\\5\\1\\0\\6\\5\\2\\4\\2\\5\\1\\0\\4\\5\\2\\4\"+",
   "    \"\\12\\5\\3\\4\\2\\0\\1\\4\\17\\0\\1\\5\\1\\4\\1\\5\\36\\4\\33\\5\"+",
   "    \"\\2\\0\\3\\4\\60\\0\\46\\4\\13\\5\\1\\4\\u014f\\0\\3\\5\\66\\4\\2\\0\"+",
   "    \"\\1\\5\\1\\4\\20\\5\\2\\0\\1\\4\\4\\5\\3\\0\\12\\4\\2\\5\\2\\0\"+",
   "    \"\\12\\5\\21\\0\\3\\5\\1\\0\\10\\4\\2\\0\\2\\4\\2\\0\\26\\4\\1\\0\"+",
   "    \"\\7\\4\\1\\0\\1\\4\\3\\0\\4\\4\\2\\0\\1\\5\\1\\4\\7\\5\\2\\0\"+",
   "    \"\\2\\5\\2\\0\\3\\5\\11\\0\\1\\5\\4\\0\\2\\4\\1\\0\\3\\4\\2\\5\"+",
   "    \"\\2\\0\\12\\5\\4\\4\\15\\0\\3\\5\\1\\0\\6\\4\\4\\0\\2\\4\\2\\0\"+",
   "    \"\\26\\4\\1\\0\\7\\4\\1\\0\\2\\4\\1\\0\\2\\4\\1\\0\\2\\4\\2\\0\"+",
   "    \"\\1\\5\\1\\0\\5\\5\\4\\0\\2\\5\\2\\0\\3\\5\\13\\0\\4\\4\\1\\0\"+",
   "    \"\\1\\4\\7\\0\\14\\5\\3\\4\\14\\0\\3\\5\\1\\0\\11\\4\\1\\0\\3\\4\"+",
   "    \"\\1\\0\\26\\4\\1\\0\\7\\4\\1\\0\\2\\4\\1\\0\\5\\4\\2\\0\\1\\5\"+",
   "    \"\\1\\4\\10\\5\\1\\0\\3\\5\\1\\0\\3\\5\\2\\0\\1\\4\\17\\0\\2\\4\"+",
   "    \"\\2\\5\\2\\0\\12\\5\\1\\0\\1\\4\\17\\0\\3\\5\\1\\0\\10\\4\\2\\0\"+",
   "    \"\\2\\4\\2\\0\\26\\4\\1\\0\\7\\4\\1\\0\\2\\4\\1\\0\\5\\4\\2\\0\"+",
   "    \"\\1\\5\\1\\4\\6\\5\\3\\0\\2\\5\\2\\0\\3\\5\\10\\0\\2\\5\\4\\0\"+",
   "    \"\\2\\4\\1\\0\\3\\4\\4\\0\\12\\5\\1\\0\\1\\4\\20\\0\\1\\5\\1\\4\"+",
   "    \"\\1\\0\\6\\4\\3\\0\\3\\4\\1\\0\\4\\4\\3\\0\\2\\4\\1\\0\\1\\4\"+",
   "    \"\\1\\0\\2\\4\\3\\0\\2\\4\\3\\0\\3\\4\\3\\0\\10\\4\\1\\0\\3\\4\"+",
   "    \"\\4\\0\\5\\5\\3\\0\\3\\5\\1\\0\\4\\5\\11\\0\\1\\5\\17\\0\\11\\5\"+",
   "    \"\\11\\0\\1\\4\\7\\0\\3\\5\\1\\0\\10\\4\\1\\0\\3\\4\\1\\0\\27\\4\"+",
   "    \"\\1\\0\\12\\4\\1\\0\\5\\4\\4\\0\\7\\5\\1\\0\\3\\5\\1\\0\\4\\5\"+",
   "    \"\\7\\0\\2\\5\\11\\0\\2\\4\\4\\0\\12\\5\\22\\0\\2\\5\\1\\0\\10\\4\"+",
   "    \"\\1\\0\\3\\4\\1\\0\\27\\4\\1\\0\\12\\4\\1\\0\\5\\4\\2\\0\\1\\5\"+",
   "    \"\\1\\4\\7\\5\\1\\0\\3\\5\\1\\0\\4\\5\\7\\0\\2\\5\\7\\0\\1\\4\"+",
   "    \"\\1\\0\\2\\4\\4\\0\\12\\5\\22\\0\\2\\5\\1\\0\\10\\4\\1\\0\\3\\4\"+",
   "    \"\\1\\0\\27\\4\\1\\0\\20\\4\\4\\0\\6\\5\\2\\0\\3\\5\\1\\0\\4\\5\"+",
   "    \"\\11\\0\\1\\5\\10\\0\\2\\4\\4\\0\\12\\5\\22\\0\\2\\5\\1\\0\\22\\4\"+",
   "    \"\\3\\0\\30\\4\\1\\0\\11\\4\\1\\0\\1\\4\\2\\0\\7\\4\\3\\0\\1\\5\"+",
   "    \"\\4\\0\\6\\5\\1\\0\\1\\5\\1\\0\\10\\5\\22\\0\\2\\5\\15\\0\\60\\4\"+",
   "    \"\\1\\5\\2\\4\\7\\5\\4\\0\\10\\4\\10\\5\\1\\0\\12\\5\\47\\0\\2\\4\"+",
   "    \"\\1\\0\\1\\4\\2\\0\\2\\4\\1\\0\\1\\4\\2\\0\\1\\4\\6\\0\\4\\4\"+",
   "    \"\\1\\0\\7\\4\\1\\0\\3\\4\\1\\0\\1\\4\\1\\0\\1\\4\\2\\0\\2\\4\"+",
   "    \"\\1\\0\\4\\4\\1\\5\\2\\4\\6\\5\\1\\0\\2\\5\\1\\4\\2\\0\\5\\4\"+",
   "    \"\\1\\0\\1\\4\\1\\0\\6\\5\\2\\0\\12\\5\\2\\0\\2\\4\\42\\0\\1\\4\"+",
   "    \"\\27\\0\\2\\5\\6\\0\\12\\5\\13\\0\\1\\5\\1\\0\\1\\5\\1\\0\\1\\5\"+",
   "    \"\\4\\0\\2\\5\\10\\4\\1\\0\\42\\4\\6\\0\\24\\5\\1\\0\\2\\5\\4\\4\"+",
   "    \"\\4\\0\\10\\5\\1\\0\\44\\5\\11\\0\\1\\5\\71\\0\\42\\4\\1\\0\\5\\4\"+",
   "    \"\\1\\0\\2\\4\\1\\0\\7\\5\\3\\0\\4\\5\\6\\0\\12\\5\\6\\0\\6\\4\"+",
   "    \"\\4\\5\\106\\0\\46\\4\\12\\0\\51\\4\\7\\0\\132\\4\\5\\0\\104\\4\\5\\0\"+",
   "    \"\\122\\4\\6\\0\\7\\4\\1\\0\\77\\4\\1\\0\\1\\4\\1\\0\\4\\4\\2\\0\"+",
   "    \"\\7\\4\\1\\0\\1\\4\\1\\0\\4\\4\\2\\0\\47\\4\\1\\0\\1\\4\\1\\0\"+",
   "    \"\\4\\4\\2\\0\\37\\4\\1\\0\\1\\4\\1\\0\\4\\4\\2\\0\\7\\4\\1\\0\"+",
   "    \"\\1\\4\\1\\0\\4\\4\\2\\0\\7\\4\\1\\0\\7\\4\\1\\0\\27\\4\\1\\0\"+",
   "    \"\\37\\4\\1\\0\\1\\4\\1\\0\\4\\4\\2\\0\\7\\4\\1\\0\\47\\4\\1\\0\"+",
   "    \"\\23\\4\\16\\0\\11\\5\\56\\0\\125\\4\\14\\0\\u026c\\4\\2\\0\\10\\4\\12\\0\"+",
   "    \"\\32\\4\\5\\0\\113\\4\\3\\0\\3\\4\\17\\0\\15\\4\\1\\0\\4\\4\\3\\5\"+",
   "    \"\\13\\0\\22\\4\\3\\5\\13\\0\\22\\4\\2\\5\\14\\0\\15\\4\\1\\0\\3\\4\"+",
   "    \"\\1\\0\\2\\5\\14\\0\\64\\4\\40\\5\\3\\0\\1\\4\\3\\0\\2\\4\\1\\5\"+",
   "    \"\\2\\0\\12\\5\\41\\0\\3\\5\\2\\0\\12\\5\\6\\0\\130\\4\\10\\0\\51\\4\"+",
   "    \"\\1\\5\\126\\0\\35\\4\\3\\0\\14\\5\\4\\0\\14\\5\\12\\0\\12\\5\\36\\4\"+",
   "    \"\\2\\0\\5\\4\\u038b\\0\\154\\4\\224\\0\\234\\4\\4\\0\\132\\4\\6\\0\\26\\4\"+",
   "    \"\\2\\0\\6\\4\\2\\0\\46\\4\\2\\0\\6\\4\\2\\0\\10\\4\\1\\0\\1\\4\"+",
   "    \"\\1\\0\\1\\4\\1\\0\\1\\4\\1\\0\\37\\4\\2\\0\\65\\4\\1\\0\\7\\4\"+",
   "    \"\\1\\0\\1\\4\\3\\0\\3\\4\\1\\0\\7\\4\\3\\0\\4\\4\\2\\0\\6\\4\"+",
   "    \"\\4\\0\\15\\4\\5\\0\\3\\4\\1\\0\\7\\4\\17\\0\\4\\5\\32\\0\\5\\5\"+",
   "    \"\\20\\0\\2\\4\\23\\0\\1\\4\\13\\0\\4\\5\\6\\0\\6\\5\\1\\0\\1\\4\"+",
   "    \"\\15\\0\\1\\4\\40\\0\\22\\4\\36\\0\\15\\5\\4\\0\\1\\5\\3\\0\\6\\5\"+",
   "    \"\\27\\0\\1\\4\\4\\0\\1\\4\\2\\0\\12\\4\\1\\0\\1\\4\\3\\0\\5\\4\"+",
   "    \"\\6\\0\\1\\4\\1\\0\\1\\4\\1\\0\\1\\4\\1\\0\\4\\4\\1\\0\\3\\4\"+",
   "    \"\\1\\0\\7\\4\\3\\0\\3\\4\\5\\0\\5\\4\\26\\0\\44\\4\\u0e81\\0\\3\\4\"+",
   "    \"\\31\\0\\11\\4\\6\\5\\1\\0\\5\\4\\2\\0\\5\\4\\4\\0\\126\\4\\2\\0\"+",
   "    \"\\2\\5\\2\\0\\3\\4\\1\\0\\137\\4\\5\\0\\50\\4\\4\\0\\136\\4\\21\\0\"+",
   "    \"\\30\\4\\70\\0\\20\\4\\u0200\\0\\u19b6\\4\\112\\0\\u51a6\\4\\132\\0\\u048d\\4\\u0773\\0\"+",
   "    \"\\u2ba4\\4\\u215c\\0\\u012e\\4\\2\\0\\73\\4\\225\\0\\7\\4\\14\\0\\5\\4\\5\\0\"+",
   "    \"\\1\\4\\1\\5\\12\\4\\1\\0\\15\\4\\1\\0\\5\\4\\1\\0\\1\\4\\1\\0\"+",
   "    \"\\2\\4\\1\\0\\2\\4\\1\\0\\154\\4\\41\\0\\u016b\\4\\22\\0\\100\\4\\2\\0\"+",
   "    \"\\66\\4\\50\\0\\15\\4\\3\\0\\20\\5\\20\\0\\4\\5\\17\\0\\2\\4\\30\\0\"+",
   "    \"\\3\\4\\31\\0\\1\\4\\6\\0\\5\\4\\1\\0\\207\\4\\2\\0\\1\\5\\4\\0\"+",
   "    \"\\1\\4\\13\\0\\12\\5\\7\\0\\32\\4\\4\\0\\1\\4\\1\\0\\32\\4\\12\\0\"+",
   "    \"\\132\\4\\3\\0\\6\\4\\2\\0\\6\\4\\2\\0\\6\\4\\2\\0\\3\\4\\3\\0\"+",
   "    \"\\2\\4\\3\\0\\2\\4\\22\\0\\3\\5\\4\\0\";",
   "  private static final char [] ZZ_CMAP = zzUnpackCMap(ZZ_CMAP_PACKED);",
   "  private static final int [] ZZ_ACTION = zzUnpackAction();",
   "  private static final String ZZ_ACTION_PACKED_0 =",
   "    \"\\2\\0\\1\\1\\2\\2\\1\\3\\2\\4\\1\\5\\1\\6\\1\\7\"+",
   "    \"\\1\\10\\1\\11\\1\\12\\1\\13\\1\\14\\1\\15\\1\\16\\1\\17\";",
   "  private static int [] zzUnpackAction() {",
   "    int [] result = new int[19];",
   "    int offset = 0;",
   "    offset = zzUnpackAction(ZZ_ACTION_PACKED_0, offset, result);",
   "    return result;",
   "  }",
   "  private static int zzUnpackAction(String packed, int offset, int [] result) {",
   "    int i = 0;",
   "    int j = offset;",
   "    int l = packed.length();",
   "    while (i < l) {",
   "      int count = packed.charAt(i++);",
   "      int value = packed.charAt(i++);",
   "      do result[j++] = value; while (--count > 0);",
   "    }",
   "    return j;",
   "  }",
   "  private static final int [] ZZ_ROWMAP = zzUnpackRowMap();",
   "  private static final String ZZ_ROWMAP_PACKED_0 =",
   "    \"\\0\\0\\0\\20\\0\\40\\0\\60\\0\\40\\0\\100\\0\\40\\0\\120\"+",
   "    \"\\0\\40\\0\\40\\0\\40\\0\\40\\0\\140\\0\\40\\0\\160\\0\\40\"+",
   "    \"\\0\\40\\0\\40\\0\\40\";",
   "  private static int [] zzUnpackRowMap() {",
   "    int [] result = new int[19];",
   "    int offset = 0;",
   "    offset = zzUnpackRowMap(ZZ_ROWMAP_PACKED_0, offset, result);",
   "    return result;",
   "  }",
   "  private static int zzUnpackRowMap(String packed, int offset, int [] result) {",
   "    int i = 0;",
   "    int j = offset;",
   "    int l = packed.length();",
   "    while (i < l) {",
   "      int high = packed.charAt(i++) << 16;",
   "      result[j++] = high | packed.charAt(i++);",
   "    }",
   "    return j;",
   "  }",
   "  private static final int [] ZZ_TRANS = zzUnpackTrans();",
   "  private static final String ZZ_TRANS_PACKED_0 =",
   "    \"\\1\\3\\1\\4\\2\\5\\1\\6\\1\\3\\1\\7\\1\\10\\1\\11\"+",
   "    \"\\1\\12\\1\\13\\1\\14\\1\\3\\3\\6\\1\\15\\1\\3\\1\\0\"+",
   "    \"\\10\\15\\1\\16\\1\\17\\3\\15\\22\\0\\1\\5\\21\\0\\4\\6\"+",
   "    \"\\5\\0\\3\\6\\6\\0\\2\\10\\10\\0\\1\\15\\2\\0\\10\\15\"+",
   "    \"\\2\\0\\3\\15\\13\\0\\1\\20\\1\\0\\1\\21\\1\\22\\1\\23\";",
   "  private static int [] zzUnpackTrans() {",
   "    int [] result = new int[128];",
   "    int offset = 0;",
   "    offset = zzUnpackTrans(ZZ_TRANS_PACKED_0, offset, result);",
   "    return result;",
   "  }",
   "  private static int zzUnpackTrans(String packed, int offset, int [] result) {",
   "    int i = 0;",
   "    int j = offset;",
   "    int l = packed.length();",
   "    while (i < l) {",
   "      int count = packed.charAt(i++);",
   "      int value = packed.charAt(i++);",
   "      value--;",
   "      do result[j++] = value; while (--count > 0);",
   "    }",
   "    return j;",
   "  }",
   "  private static final int ZZ_UNKNOWN_ERROR = 0;",
   "  private static final int ZZ_NO_MATCH = 1;",
   "  private static final int ZZ_PUSHBACK_2BIG = 2;",
   "  private static final String ZZ_ERROR_MSG[] = {",
   "    \"Unkown internal scanner error\",",
   "    \"Error: could not match input\",",
   "    \"Error: pushback value was too large\"",
   "  };",
   "  private static final int [] ZZ_ATTRIBUTE = zzUnpackAttribute();",
   "  private static final String ZZ_ATTRIBUTE_PACKED_0 =",
   "    \"\\2\\0\\1\\11\\1\\1\\1\\11\\1\\1\\1\\11\\1\\1\\4\\11\"+",
   "    \"\\1\\1\\1\\11\\1\\1\\4\\11\";",
   "  private static int [] zzUnpackAttribute() {",
   "    int [] result = new int[19];",
   "    int offset = 0;",
   "    offset = zzUnpackAttribute(ZZ_ATTRIBUTE_PACKED_0, offset, result);",
   "    return result;",
   "  }",
   "  private static int zzUnpackAttribute(String packed, int offset, int [] result) {",
   "    int i = 0;",
   "    int j = offset;",
   "    int l = packed.length();",
   "    while (i < l) {",
   "      int count = packed.charAt(i++);",
   "      int value = packed.charAt(i++);",
   "      do result[j++] = value; while (--count > 0);",
   "    }",
   "    return j;",
   "  }",
   "  private java.io.Reader zzReader;",
   "  private int zzState;",
   "  private int zzLexicalState = YYINITIAL;",
   "  private char zzBuffer[] = new char[ZZ_BUFFERSIZE];",
   "  private int zzMarkedPos;",
   "  private int zzCurrentPos;",
   "  private int zzStartRead;",
   "  private int zzEndRead;",
   "  private int yyline;",
   "  private int yychar;",
   "  private int yycolumn;",
   "  private boolean zzAtBOL = true;",
   "  private boolean zzAtEOF;",
   "  StringBuffer string = new StringBuffer();",
   "  public enum TokenType { IDENTIFIER, INTEGRAL, STRING, LPAR, RPAR, COMMA } ;",
   "  public class Token { ",
   "    public Token(TokenType t, String s) { type = t; string = s; }",
   "    public TokenType type; ",
   "    public String string; ",
   "  } ",
   "  public Lexer(java.io.Reader in) {",
   "    this.zzReader = in;",
   "  }",
   "  public Lexer(java.io.InputStream in) {",
   "    this(new java.io.InputStreamReader(in));",
   "  }",
   "  private static char [] zzUnpackCMap(String packed) {",
   "    char [] map = new char[0x10000];",
   "    int i = 0;",
   "    int j = 0;",
   "    while (i < 1692) {",
   "      int  count = packed.charAt(i++);",
   "      char value = packed.charAt(i++);",
   "      do map[j++] = value; while (--count > 0);",
   "    }",
   "    return map;",
   "  }",
   "  private boolean zzRefill() throws java.io.IOException {",
   "    if (zzStartRead > 0) {",
   "      System.arraycopy(zzBuffer, zzStartRead,",
   "                       zzBuffer, 0,",
   "                       zzEndRead-zzStartRead);",
   "      zzEndRead-= zzStartRead;",
   "      zzCurrentPos-= zzStartRead;",
   "      zzMarkedPos-= zzStartRead;",
   "      zzStartRead = 0;",
   "    }",
   "    if (zzCurrentPos >= zzBuffer.length) {",
   "      char newBuffer[] = new char[zzCurrentPos*2];",
   "      System.arraycopy(zzBuffer, 0, newBuffer, 0, zzBuffer.length);",
   "      zzBuffer = newBuffer;",
   "    }",
   "    int numRead = zzReader.read(zzBuffer, zzEndRead,",
   "                                            zzBuffer.length-zzEndRead);",
   "    if (numRead > 0) {",
   "      zzEndRead+= numRead;",
   "      return false;",
   "    }",
   "    if (numRead == 0) {",
   "      int c = zzReader.read();",
   "      if (c == -1) {",
   "        return true;",
   "      } else {",
   "        zzBuffer[zzEndRead++] = (char) c;",
   "        return false;",
   "      }     ",
   "    }",
   "    return true;",
   "  }",
   "  public final void yyclose() throws java.io.IOException {",
   "    zzAtEOF = true;",
   "    zzEndRead = zzStartRead;",
   "    if (zzReader != null)",
   "      zzReader.close();",
   "  }",
   "  public final void yyreset(java.io.Reader reader) {",
   "    zzReader = reader;",
   "    zzAtBOL  = true;",
   "    zzAtEOF  = false;",
   "    zzEndRead = zzStartRead = 0;",
   "    zzCurrentPos = zzMarkedPos = 0;",
   "    yyline = yychar = yycolumn = 0;",
   "    zzLexicalState = YYINITIAL;",
   "  }",
   "  public final int yystate() {",
   "    return zzLexicalState;",
   "  }",
   "  public final void yybegin(int newState) {",
   "    zzLexicalState = newState;",
   "  }",
   "  public final String yytext() {",
   "    return new String( zzBuffer, zzStartRead, zzMarkedPos-zzStartRead );",
   "  }",
   "  public final char yycharat(int pos) {",
   "    return zzBuffer[zzStartRead+pos];",
   "  }",
   "  public final int yylength() {",
   "    return zzMarkedPos-zzStartRead;",
   "  }",
   "  private void zzScanError(int errorCode) {",
   "    String message;",
   "    try {",
   "      message = ZZ_ERROR_MSG[errorCode];",
   "    }",
   "    catch (ArrayIndexOutOfBoundsException e) {",
   "      message = ZZ_ERROR_MSG[ZZ_UNKNOWN_ERROR];",
   "    }",
   "",
   "    throw new Error(message);",
   "  } ",
   "  public void yypushback(int number)  {",
   "    if ( number > yylength() )",
   "      zzScanError(ZZ_PUSHBACK_2BIG);",
   "",
   "    zzMarkedPos -= number;",
   "  }",
   "  public Token yylex() throws java.io.IOException {",
   "    int zzInput;",
   "    int zzAction;",
   "    int zzCurrentPosL;",
   "    int zzMarkedPosL;",
   "    int zzEndReadL = zzEndRead;",
   "    char [] zzBufferL = zzBuffer;",
   "    char [] zzCMapL = ZZ_CMAP;",
   "    int [] zzTransL = ZZ_TRANS;",
   "    int [] zzRowMapL = ZZ_ROWMAP;",
   "    int [] zzAttrL = ZZ_ATTRIBUTE;",
   "    while (true) {",
   "      zzMarkedPosL = zzMarkedPos;",
   "      zzAction = -1;",
   "      zzCurrentPosL = zzCurrentPos = zzStartRead = zzMarkedPosL;",
   "      zzState = ZZ_LEXSTATE[zzLexicalState];",
   "      zzForAction: {",
   "        while (true) {",
   "          if (zzCurrentPosL < zzEndReadL)",
   "            zzInput = zzBufferL[zzCurrentPosL++];",
   "          else if (zzAtEOF) {",
   "            zzInput = YYEOF;",
   "            break zzForAction;",
   "          }",
   "          else {",
   "            zzCurrentPos  = zzCurrentPosL;",
   "            zzMarkedPos   = zzMarkedPosL;",
   "            boolean eof = zzRefill();",
   "            zzCurrentPosL  = zzCurrentPos;",
   "            zzMarkedPosL   = zzMarkedPos;",
   "            zzBufferL      = zzBuffer;",
   "            zzEndReadL     = zzEndRead;",
   "            if (eof) {",
   "              zzInput = YYEOF;",
   "              break zzForAction;",
   "            }",
   "            else {",
   "              zzInput = zzBufferL[zzCurrentPosL++];",
   "            }",
   "          }",
   "          int zzNext = zzTransL[ zzRowMapL[zzState] + zzCMapL[zzInput] ];",
   "          if (zzNext == -1) break zzForAction;",
   "          zzState = zzNext;",
   "          int zzAttributes = zzAttrL[zzState];",
   "          if ( (zzAttributes & 1) == 1 ) {",
   "            zzAction = zzState;",
   "            zzMarkedPosL = zzCurrentPosL;",
   "            if ( (zzAttributes & 8) == 8 ) break zzForAction;",
   "          }",
   "        }",
   "      }",
   "      zzMarkedPos = zzMarkedPosL;",
   "      switch (zzAction < 0 ? zzAction : ZZ_ACTION[zzAction]) {",
   "        case 5: ",
   "          { return new Token(TokenType.LPAR,\"(\");",
   "          }",
   "        case 16: break;",
   "        case 7: ",
   "          { return new Token(TokenType.COMMA,\",\");",
   "          }",
   "        case 17: break;",
   "        case 13: ",
   "          { string.append('\\t');",
   "          }",
   "        case 18: break;",
   "        case 6: ",
   "          { return new Token(TokenType.RPAR,\")\");",
   "          }",
   "        case 19: break;",
   "        case 3: ",
   "          { return new Token(TokenType.IDENTIFIER,yytext());",
   "          }",
   "        case 20: break;",
   "        case 1: ",
   "          { throw new Error(\"Illegal character <\"+ yytext()+\">\");",
   "          }",
   "        case 21: break;",
   "        case 2: ",
   "          { /* ignore */",
   "          }",
   "        case 22: break;",
   "        case 11: ",
   "          { string.append('\\\\');",
   "          }",
   "        case 23: break;",
   "        case 12: ",
   "          { string.append('\\\"');",
   "          }",
   "        case 24: break;",
   "        case 10: ",
   "          { yybegin(YYINITIAL); ",
   "            return new Token(TokenType.STRING,string.toString());",
   "          }",
   "        case 25: break;",
   "        case 4: ",
   "          { return new Token(TokenType.INTEGRAL,yytext());",
   "          }",
   "        case 26: break;",
   "        case 15: ",
   "          { string.append('\\r');",
   "          }",
   "        case 27: break;",
   "        case 14: ",
   "          { string.append('\\n');",
   "          }",
   "        case 28: break;",
   "        case 9: ",
   "          { string.append( yytext() );",
   "          }",
   "        case 29: break;",
   "        case 8: ",
   "          { string.setLength(0); yybegin(STRING);",
   "          }",
   "        case 30: break;",
   "        default: ",
   "          if (zzInput == YYEOF && zzStartRead == zzCurrentPos) {",
   "            zzAtEOF = true;",
   "            return null;",
   "          } ",
   "          else {",
   "            zzScanError(ZZ_NO_MATCH);",
   "          }",
   "      }",
   "    }",
   "  }",
   "}"]


