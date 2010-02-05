-------------------------------------------------------------------
-- |
-- Module      : Gom.Java
-- Copyright   : (c) Paul Brauner 2009
--               (c) Emilie Balland 2009
--               (c) INRIA 2009
-- Licence     : GPL (see COPYING)
--
-- Maintainer  : paul.brauner@inria.fr
-- Stability   : provisional
-- Portability : non-portable (requires generalized newtype deriving)
--
-- Some utility functions for generating
-- java and tom pieces of type 'Doc'.
--------------------------------------------------------------------

module Gom.Java (
  -- * File hierarchies
  FileHierarchy(..),
  generateFileHierarchy,
  generateHierarchyIn,
  -- * Java pretty-printing
  -- ** Generic
  ibraces, sbraces, encloseCommas, rBody,
  -- ** Comments
  rComment,
  -- ** Java keywords
  abstract, public, protected, private, this,
  jreturn, throw, new, void, instanceof, jif,
  while, jelse, jtrue, jfalse, final, static,
  -- ** Java Types
  jint, stringBuilder, jboolean,
  jObject, jVisitable, jShared, jSharedId,
  jVisitableArray, jSCombinator, 
  jIntrospector,
  -- ** Classes
  rClass, rFullClass,
  -- ** Methods
  rMethodDef, rMethodCall, rWrapBuiltin, rConstructorCall,
  -- ** Control structures
  rIfThen, rIfThenElse, rWhile, rSwitch,
  -- * Tom pretty-printing
  -- ** Mappings
  rOp, rOpList, rTypeterm,
  rIsFsym, rMake, rGetSlot,
  -- ** Other
  inline
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
    classContent :: Doc
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
-- Common usage : @generateFileHierarchyIn compact dir []@
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

-- | Encloses a document into { } and indents the body.
ibraces :: Doc -> Doc
ibraces d = group $ nest 2 (lbrace <$> d) <$> rbrace

-- | Encloses a document into { }
sbraces :: Doc -> Doc
sbraces d = lbrace <+> d <+> rbrace

abstract,public,protected,private :: Doc
this,jreturn,throw,new,void,final,static :: Doc
instanceof,jif,while,jelse,jtrue,jfalse :: Doc

abstract   = text "abstract"
public     = text "public"
protected  = text "protected"
private    = text "private"
this       = text "this"
jreturn    = text "return"
throw      = text "throw"
new        = text "new"
void       = text "void"
instanceof = text "instanceof"
jif        = text "if"
while      = text "while"
jelse      = text "else"
jtrue      = text "true"
jfalse     = text "false"
final      = text "final"
static     = text "static"

jint,stringBuilder,jboolean,jObject, jIntrospector :: Doc
jVisitable,jShared,jSharedId,jVisitableArray :: Doc
jSCombinator :: Doc
jint            = text "int"
jboolean        = text "boolean"
stringBuilder   = text "java.lang.StringBuilder"
jObject         = text "Object"
jVisitable      = text "tom.library.sl.Visitable"
jIntrospector   = text "tom.library.sl.Introspector"
jSCombinator    = text "tom.library.sl.AbstractStrategyCombinator"
jShared         = text "shared.SharedObject"
jSharedId       = text "shared.SharedObjectWithID"
jVisitableArray = jVisitable <> text "[]"

-- | Renders the list enclosed in parenthesis and
-- separated by commas.
encloseCommas :: [Doc] -> Doc
encloseCommas [] = text "()"
encloseCommas l = parens $ nest 2 (softbreak <> fillSep (punctuate comma l))

-- | Renders the list punctuated by semicolons
rBody :: [Doc] -> Doc
rBody [] = empty
rBody l = align $ sep (punctuate semi l) <> semi

-- | Renders @modifier class name { body }@.
rClass
 :: Doc   -- ^ modifier (public, private..)
 -> Doc   -- ^ class name
 -> Maybe Doc   -- ^ extends
 -> [Doc] -- ^ implements
 -> Doc   -- ^ body
 -> Doc
rClass md cn ex im body = 
  md <+> text "class" <+> cn <+> r1 ex <+> r2 im <+> ibraces body
  where r1 Nothing  = empty
        r1 (Just d) = text "extends" <+> d
        r2 [] = empty
        r2 l  = text "implements" <+> hsep (punctuate comma l)

-- | Adds package name at the top of a class declaration.
rFullClass
  :: [String] -- ^ package
  -> Doc      -- ^ class content
  -> Doc
rFullClass [] bd = bd
rFullClass pk bd = text "package" <+> text (intercalate "." pk) <>
                   semi <> linebreak <$> bd

-- | Renders @modifier type name(arg_1,...,arg_n) { body }@.
rMethodDef
 :: Doc   -- ^ modifier
 -> Doc   -- ^ return type
 -> Doc   -- ^ method name
 -> [Doc] -- ^ arguments
 -> Doc   -- ^ body
 -> Doc
rMethodDef md ty mn args body = 
  md <+> ty <+> mn <> encloseCommas args <+> ibraces body

-- | Renders @object.method(arg_1,...,arg_n)@.
rMethodCall
 :: Doc   -- ^ object
 -> Doc   -- ^ method name
 -> [Doc] -- ^ arguments
 -> Doc
rMethodCall o f args = 
  o <> dot <> f <> encloseCommas args

-- | Renders @new className(arg_1,...,arg_n)@.
rConstructorCall
 :: Doc   -- ^ class name
 -> [Doc] -- ^ arguments
 -> Doc
rConstructorCall c args = 
  new <+> c <> encloseCommas args

-- | @rWrapBuiltin qt@ renders
--
-- > tom.library.sl.VisitableBuiltin<qt>
rWrapBuiltin
  :: Doc -- ^ qualified type
  -> Doc
rWrapBuiltin qt =
  text "tom.library.sl.VisitableBuiltin" <> angles qt

-- | Renders the string between @\/* \*/@ on one or several lines.
rComment :: String -> Doc
rComment s = text "/*" <+> fillSep (map text $ words s) <+> text "*/"

-- | Renders @if(cond) { then body }@.
rIfThen 
  :: Doc -- ^ condition
  -> Doc -- ^ then body
  -> Doc
rIfThen c b = 
  group $ jif <+> parens c <+> ibraces b

-- | Renders @if(cond) { then body } else { else body }@.
rIfThenElse 
  :: Doc -- ^ condition
  -> Doc -- ^ then body
  -> Doc -- ^ else body
  -> Doc
rIfThenElse c b1 b2 = 
  group $ jif <+> parens c <+> ibraces b1 <+> jelse <+> ibraces b2

-- | Renders @while(cond) { body }@
rWhile
  :: Doc -- ^ cond
  -> Doc -- ^ body
  -> Doc
rWhile c b = group $ while <+> parens c <+> ibraces b

-- | @rSwitch s [(l1,r1),..,(ln,rn)] d@ renders
--
-- > switch (s) {
-- >   l1: r1
-- >   ...
-- >   l2: r2
-- >   default: d
-- > }
rSwitch
 :: Doc -- ^ subject
 -> [(Doc,Doc)] -- ^ lhs, rhs
 -> Maybe Doc -- ^ default case
 -> Doc
rSwitch s l d = text "switch" <+> parens s <+> ibraces body
  where body    = vcat (map f l) <$> maybe empty ((text "default:" <+>) . align) d
        f (x,y) = text "case" <+> x <> colon <+> align y

-- | @rTypeterm s qs shr@ renders
--
-- > %typeterm s {
-- >   implement { qs }
-- >   is_sort(t) { ($t instanceof qs) }
-- >   equals(t1,t2) { eq }
-- > }
--
-- where @eq = $t1 == $t2@ if @shr@,
-- @$t1.equals($t2)@ otherwise.
rTypeterm
  :: Doc -- ^ sort
  -> Doc -- ^ qualified sort
  -> Bool -- ^ sharing ?
  -> Doc
rTypeterm s qs deep =
  text "%typeterm" <+> s <+> ibraces (vcat
    [text "implement" <+> sbraces qs,
     text "is_sort(t) { ($t instanceof" <+> qs <> text ") }",
     text "equals(t1,t2) { (" <> eq <> text ") }"])
  where eq = text $ if deep then "$t1 == $t2"
                            else "$t1.equals($t2)"

-- | Renders @is_fsym(t) { ($t instanceof sort) }@.
rIsFsym
  :: Doc -- ^ sort 
  -> Doc
rIsFsym s = 
  text "is_fsym(t) { ($t instanceof" <+> s <> text ") }"

-- | Renders @make(arg1,..,argn) { (m.types.co.make($arg1,..,$argn)) }@.
rMake
 :: Doc   -- ^ qualified constructor
 -> [Doc] -- ^ arguments
 -> Doc
rMake qc as =
  text "make" <> args <+> (sbraces . parens) (qc <> text ".make" <> iargs)
  where gen   = parens . hcat . punctuate comma
        args  = gen as
        iargs = gen (map inline as)


-- | Renders @get_slot(slot,t) { $t.getslot() }@.
rGetSlot
 :: Doc -- ^ slot
 -> Doc
rGetSlot x =
  text "get_slot(" <> x <> text ",t) { $t.get" <> x <> text "() }"

-- | Renders @%op sort c(field1,..,field2) { body }@.
rOp :: Doc   -- ^ sort
    -> Doc   -- ^ constructor
    -> [(Doc,Doc)] -- ^ fields
    -> Doc   -- ^ body
    -> Doc
rOp s c fs b = 
   text "%op" <+> s <+> c <> 
   (parens . hsep . punctuate comma $ map f fs) <+> ibraces b
  where f (x,t) = x <> colon <> t

-- | @rOpList c co dom cons empty@ renders
--
-- > %oplist co VC(dom*) {
-- >   is_fsym(t) { (($t instanceof cons) || ($t instanceof empty)) }
-- >   make_empty() { empty.make() }
-- >   make_insert(e,l) { cons.make($e,$l) }
-- >   get_head(l) { $l.getHeadc() }
-- >   get_tail(l) { $l.getTailc() }
-- >   is_empty(l) { $l.isEmptyc() }
-- > }
rOpList 
  :: Doc -- ^ the constructor
  -> Doc -- ^ codomain of the constructor
  -> Doc -- ^ domain of the constructor
  -> Doc -- ^ qualified Cons
  -> Doc -- ^ qualified Empty
  -> Doc
rOpList c co dom consc emptyc =
  text "%oplist" <+> co <+> c <> parens (dom <> text "*") <+> 
   ibraces (vcat
     [text "is_fsym(t) { (($t instanceof" <+> consc <> text ") || ($t instanceof" <+> emptyc <> text ")) }",
      text "make_empty() {" <+> emptyc <> text ".make() }",
      text "make_insert(e,l) {" <+> consc <> text ".make($e,$l) }",
      text "get_head(l) { $l.getHead" <> c <> text "() }",
      text "get_tail(l) { $l.getTail" <> c <> text "() }",
      text "is_empty(l) { $l.isEmpty" <> c <> text "() }"])

-- | @inline d@ renders @$d@.
inline :: Doc -> Doc
inline = (text "$" <>)
