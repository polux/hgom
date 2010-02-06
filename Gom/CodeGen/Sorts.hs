------------------------------------------------------------------
-- |
-- Module      : Gom.CodeGen.Sorts
-- Copyright   : (c) Paul Brauner 2009
--               (c) Emilie Balland 2009
--               (c) INRIA 2009
-- Licence     : GPL (see COPYING)
--
-- Maintainer  : paul.brauner@inria.fr
-- Stability   : provisional
-- Portability : non-portable (requires generalized newtype deriving)
--------------------------------------------------------------------

module Gom.CodeGen.Sorts where

import Gom.Sig
import Gom.Config
import Gom.FileGen
import Gom.SymbolTable
import Gom.CodeGen.Common
import Gom.CodeGen.Constructors

import Text.PrettyPrint.Leijen
import Control.Monad(foldM,liftM)

-- | Given a sort @Sort@, generates 
--  an abstract class @Sort.java@
--  and a package @sort@ containing:
--
--     - an abstract  class @VC.java@ for every variadic constructor @VC@ of
--     @Sort@
--
--     - a concrete class @C.java@ for every non-variadic constructor @C@ of
--     @Sort@
compSort :: SortId -> Gen [FileHierarchy]
compSort s = do ac    <- compAbstractSort s
                vctrs <- askSt (vCtorsOf s)
                ctrs  <- askSt (sCtorsOf s)
                avs   <- mapM compAbstractVariadic vctrs
                ccs   <- mapM compConstructor ctrs
                return [ac, Package (show $ lowerSortId s) (avs ++ ccs)] 

-- | Given a sort @S@, generates an abstract class @S.java@.
compAbstractSort :: SortId -> Gen FileHierarchy
compAbstractSort s = do eg <- compEmptyGettersOfSort s
                        es <- compEmptySettersOfSort s
                        ei <- compEmptyIsX s
                        fs <- ifP $ compFromStringSort s
                        pa <- ifP $ compParseSort s
                        ri <- ifR $ compMakeRandomSortInit s
                        ra <- ifR $ compMakeRandomSort s
                        cl <- wrap $ vcat [eg,es,ei,pa,fs,ri,ra]
                        return $ Class (show s) cl
  where ifP = flip (ifConfM parsers) (return empty)
        ifR = flip (ifConfM random) (return empty)
        wrap body = do qat <- qualifiedAbstractType
                       return $ rClass (public <+> abstract) 
                                       (pretty s) (Just qat)  
                                       [] body

-- | @hasNotError s f@ renders 
-- @throw new UnsupportedOperationException(\"this s has no f.\")@
hasNotError :: SortId -> FieldId -> Doc
hasNotError s f = throw <+> new <+> text "UnsupportedOperationException" <>
                  parens (dquotes $ text "This" <+> pretty s <+> 
                                    text "has no" <+> pretty f <> dot)

-- | Given a sort S, generates the methods: 
--
-- > public m.types.T getx() {
-- >   throw new UnsupportedOperationException("This S has no x");
-- > }
--
-- for each of its fields @x:T@
compEmptyGettersOfSort :: SortId -> Gen Doc
compEmptyGettersOfSort = iterOverSortFields getter vcat
  where getter co f t = do qt <- qualifiedSort t
                           let fun = text "get" <> pretty f
                           let b = rBody [hasNotError co f]
                           return $ rMethodDef public qt fun [] b 

-- | Given a sort S, generates the methods: 
--
-- > public void setx(m.types.T x) {
-- >   throw new UnsupportedOperationException("This S has no x");
-- > }
--
-- for each of its fields @x:T@
compEmptySettersOfSort :: SortId -> Gen Doc
compEmptySettersOfSort = iterOverSortFields setter vcat
  where setter co f t = do qt <- qualifiedSort t
                           let fun = text "set" <> pretty f
                           let a = [pretty qt <+> pretty f]
                           let b = rBody [hasNotError co f]
                           return $ rMethodDef public void fun a b 

-- | Given a sort S, generates the methods: 
--
-- > public void isx() {
-- >   return false;
-- > }
--
-- for each of its fields @x@
compEmptyIsX :: SortId -> Gen Doc
compEmptyIsX s = do cs  <- askSt (sCtorsOf s) 
                    return . vcat $ map isx cs
  where isx f = let fun = text "is" <> pretty f
                    b   = rBody [jreturn <+> jfalse]
                in rMethodDef public jboolean fun [] b 

-- | Given a sort @T = f1(...) | ... | fn(...)@, generates
--
-- > static public sig.types.T parse(mod.Parser par) {
-- >   String id = par.parseId();
-- >   if (id.equals("f1")) return mod.types.t.f1.parse(par);
-- >   else ....
-- >   else if(id.equals("fn")) return mod.types.t.fn.parse(par);
-- >   else throw new RuntimeException();
-- > }
compParseSort :: SortId -> Gen Doc
compParseSort s = do
  qs  <- qualifiedSort s
  scs <- askSt (sCtorsOf s)
  vcs <- askSt (vCtorsOf s)
  let cs = scs ++ vcs
  qcs <- mapM qualifiedCtor cs
  pr  <- packagePrefix
  let calls = foldr ifsym post (zip cs qcs)
  return $ rMethodDef (static <+> public) qs (text "parse")
           [pars pr <+> arg] (vcat [pre,calls])
  where pars pr  = pr <> dot <> text "Parser"
        arg      = text "par"
        pre      = text "String id = par.parseId();"
        post     = text "throw new RuntimeException();"
        cond c   = rMethodCall (text "id") (text "equals") [dquotes $ pretty c]
        rcall qc = rMethodCall (pretty qc) (text "parseArgs") [arg]
        ifsym (c,qc) = rIfThenElse (cond c) (jreturn <+> rcall qc <> semi)

-- | Given a sort @S@, generates
--
-- > public static mod.types.S fromString(String s) {
-- >   return mod.types.S.parse(new mod.Parser(s));
-- > }
compFromStringSort :: SortId -> Gen Doc
compFromStringSort s = do
  qs <- qualifiedSort s
  pr <- packagePrefix
  let pa = pr <> dot <> text "Parser"
  return $ vcat
    [text "public static" <+> qs <+> text "fromString(String s) {",
     text "  return" <+> qs <> text ".parse(new" <+> pa <> text "(s));",
     text "}"]

-- | Given a sort @T = f1(...) | ... | fn(...)@, generates
--
-- > static public sig.types.T 
-- > makeRandom(java.util.Random rand, int depth) {
-- >   if (depth <= 0) {
-- >     switch(rand.nextInt(m)) {
-- >       case 0: return sig.types.t.g1.make();
-- >       ...
-- >       case m-1: return sig.types.t.gn.make();
-- >     }
-- >   } else {
-- >     switch(rand.nextInt(n)) {
-- >       case 0: return sig.types.t.f1.makeRandom(rand,maxdepth);
-- >       ...
-- >       case n-1: return sig.types.t.fn.makeRandom(rand,maxdepth);
-- >       default: throw new RuntimeException("never happens");
-- >     }
-- >   }
-- > }
--
-- Where @g1 ... gm@ are the constructors with arity 0 among @f1 ... fn@.
compMakeRandomSort :: SortId -> Gen Doc
compMakeRandomSort s = do
  qs         <- qualifiedSort s
  cs         <- askSt (sCtorsOf s)
  (zcs,nzcs) <- partitionM isConst cs
  qzcs       <- mapM qualifiedCtor zcs
  qnzcs      <- mapM qualifiedCtor nzcs
  let rcalls1 = map rcall1 qzcs 
  let rcalls2 = map rcall2 qnzcs
  return $ rMethodDef (static <+> public) qs (text "makeRandom")
                      [text "java.util.Random rand", text "int depth"] 
                      (pack rcalls1 (rcalls1 ++ rcalls2))
  where isConst c  = null `liftM` askSt (fieldsOf c) 
        rcall1 qc  = jreturn <+> pretty qc <> text ".make();"
        rcall2 qc  = jreturn <+> pretty qc <> text ".makeRandom(rand,depth-1);"
        ints       = map int [0..]
        dflt       = text "throw new RuntimeException();"
        nextint n  = rMethodCall (text "rand") (text "nextInt") [int (max n 1)]
        pack c1 c2 = rIfThen (text "depth <= 0") 
                       (rSwitch (nextint $ length c1) (zip ints c1) Nothing)
                     <$> 
                     rSwitch (nextint $ length c2) (zip ints c2) (Just dflt)

-- | generates
--
-- > static public sig.types.T makeRandom(int depth) {
-- >   sig.types.T.makeRandom(new java.util.Random(), depth);
-- > }
compMakeRandomSortInit :: SortId -> Gen Doc
compMakeRandomSortInit s = do
  qs <- qualifiedSort s
  return $ rMethodDef (static <+> public) qs (text "makeRandom") 
                      [text "int depth"] (body qs)
  where body qs = jreturn <+> qs <> 
                  text ".makeRandom(new java.util.Random(), depth);"

-- | Monadic version of Data.List.partition
partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM p xs = foldM f ([],[]) xs
  where f (a,b) x = do test <- p x
                       return $ if test then (x:a,b) else (a,x:b)
