------------------------------------------------------------------
-- |
-- Module      : Gom.CodeGen.Constructors
-- Copyright   : (c) Paul Brauner 2009
--               (c) Emilie Balland 2009
--               (c) INRIA 2009
-- Licence     : GPL (see COPYING)
--
-- Maintainer  : paul.brauner@inria.fr
-- Stability   : provisional
-- Portability : non-portable (requires generalized newtype deriving)
--------------------------------------------------------------------

module Gom.CodeGen.Constructors where

import Gom.Sig
import Gom.Config
import Gom.FileGen
import Gom.SymbolTable
import Gom.CodeGen.Common

import Text.PrettyPrint.Leijen
import Control.Monad.Reader
import Data.List(intersperse)

-- | Given a variadic constructor @VC@, generates an abstract class @VC.java@.
compAbstractVariadic :: CtorId -> Gen FileHierarchy
compAbstractVariadic vc = do cl <- body
                             return $ Class (show vc) cl
  where ifP = flip (ifConfM parsers) (return empty)
        body = do co  <- askSt (codomainOf vc)
                  qto <- qualifiedSort co
                  tsb <- compToStringBuilderVariadic vc
                  pa  <- ifP $ compParseVariadic vc
                  pat <- ifP $ compParseVariadicTail vc
                  return $ rClass (public <+> abstract) (pretty vc)
                                  (Just qto) [] (vcat [tsb,pa,pat])
 
-- | Given a variadic constructor @List(T*)@, generates
--
-- >  public void toStringBuilder(java.lang.StringBuilder buf) {
-- >   buffer.append("List(");
-- >   if(this instanceof mod.types.codom.ConsList) {
-- >     mod.types.List cur = this;
-- >     while(cur instanceof mod.types.codom.ConsList) {
-- >       mod.types.T elem = cur.getHeadList();
-- >       cur = cur.getTailList();
-- >       elem.toStringBuilder(buf);
-- >       if(cur instanceof mod.types.codom.ConsList) {
-- >         buf.append(",");
-- >       }
-- >     }
-- >     if(!(cur instanceof mod.types.codom.EmptyList)) {
-- >       buf.append(",");
-- >       cur.toStringBuilder(buf);
-- >     }
-- >   }
-- >   buf.append(")");
-- > }
compToStringBuilderVariadic :: CtorId -> Gen Doc
compToStringBuilderVariadic vc = do
  qcons <- qualifiedCtor cons
  qnil  <- qualifiedCtor nil
  co    <- askSt (codomainOf vc)
  dom   <- askSt (fieldOf vc) 
  qco   <- pretty `liftM` qualifiedSort co
  qdom  <- pretty `liftM` qualifiedSort dom
  let mid = middle qco qcons qnil qdom (isBuiltin dom)
  return $ rMethodDef public void toSB
                      [stringBuilder <+> buf]
                      (rBody [pre,mid,post])
  where bapp arg = rMethodCall buf (text "append") [arg]
        cons = prependCons vc
        nil  = prependEmpty vc
        pre  = bapp $ dquotes (pretty vc <> lparen)
        post = bapp $ dquotes rparen 
        comm = bapp $ dquotes comma
        cur  = text "cur"
        elm  = text "elem"
        getH = text "getHead" <> pretty vc
        getT = text "getTail" <> pretty vc
        toSB = text "toStringBuilder"
        buf  = text "buf"
        jneg = (text "!" <>) . parens
        middle qco qc qn dom btin = 
          rIfThen (this <+> instanceof <+> qc) $ rBody
            [qco <+> cur <+> equals <+> this,
             rWhile (cur <+> instanceof <+> qc) $ rBody
               [dom <+> elm <+> equals <+> rMethodCall cur getH [],
                cur <+> equals <+> rMethodCall cur getT [],
                if btin then bapp elm else rMethodCall elm toSB [buf],
                rIfThen (cur <+> instanceof <+> qc) (rBody [comm])],
             rIfThen (jneg $ cur <+> instanceof <+> qn) $ rBody 
               [comm, rMethodCall cur toSB [buf]]]

-- | Given a sort @s@, @parseRecCall arg s@ generates
--
--  * @arg.parses()@ if @s@ is a builtin, 
--
--  * @mod.types.s.parse(arg)@ otherwise
parseRecCall :: Doc -> SortId -> Gen Doc
parseRecCall arg s = do
  qs <- qualifiedSort s
  return $ if isBuiltin s 
    then rMethodCall arg (text "parse" <> pretty s) []
    else rMethodCall qs (text "parse") [arg]

-- | Given a constructor @C(x1:T1,...,xn:Tn)@, 
-- of codomain @Co@, generates
--
-- > final static mod.types.Co parseArgs(mod.Parser par) {
-- >   par.parseLpar();
-- >   mod.types.T1 x1 = mod.types.T1.parse(par);
-- >   par.parseComma();
-- >   ...
-- >   par.parseComma();
-- >   mod.types.Tn xn = mod.types.Tn.parse(par);
-- >   par.parseRpar();
-- >   return mod.types.Co.C.make(x1,...,xn);
-- > }
compParseConstructor :: CtorId -> Gen Doc
compParseConstructor c = do
  pr   <- packagePrefix
  co   <- askSt (codomainOf c)
  qco  <- qualifiedSort co
  recs <- iterOverFields call (intersperse pcomm) c
  post <- postM
  return $ rMethodDef (final <+> static <+> public)
                      qco (text "parseArgs")
                      [pars pr <+> arg] (rBody (pre:recs++post))
  where pars pr  = pr <> dot <> text "Parser"
        arg      = text "par"
        pcomm    = rMethodCall arg (text "parseComma") []
        call f s = do 
          qs  <- qualifiedSort s
          rec <- parseRecCall arg s
          return $ qs <+> pretty f <+> equals <+> rec
        pre      = rMethodCall arg (text "parseLpar") []
        postM    = do qc <- qualifiedCtor c
                      fis <- map (pretty . fst) `liftM` askSt (fieldsOf c)
                      return [rMethodCall arg (text "parseRpar") [], 
                              jreturn <+> rMethodCall qc (text "make") fis]

-- | Given a variadic constructor @VC(T*)@, 
-- of codomain @Co@, generates
--
-- > static public mod.types.Co parseArgs(mod.Parser par) {
-- >   par.parseLpar();
-- >   if (par.isRpar()) {
-- >     par.parseRpar();
-- >     return mod.types.vc.EmptyVC.make();
-- >   } else {
-- >     mod.types.T  head =  mod.types.T.parse(par)
-- >     mod.types.Co tail = mod.types.vc.VC.parseTail(par);
-- >     par.parseRpar();
-- >     return mod.types.vc.ConsVC.make(head,tail);
-- >   }
-- > }
compParseVariadic :: CtorId -> Gen Doc
compParseVariadic vc = do
  pr    <- packagePrefix
  co    <- askSt (codomainOf vc)
  qco   <- qualifiedSort co
  qvc   <- qualifiedCtor vc
  qcons <- qualifiedCtor (prependCons vc)
  qnil  <- qualifiedCtor (prependEmpty vc)
  phead <- parseHead
  return $ rMethodDef 
    (static <+> public) qco (text "parseArgs") [pars pr <+> arg]
    (vcat [text "par.parseLpar();",
           rIfThenElse (text "par.isRpar()")
             (rBody [text "par.parseRpar()", makeNil qnil])
             (rBody [phead, parseTail qco qvc,
                     text "par.parseRpar()", makeCons qcons])])
  where pars pr      = pr <> dot <> text "Parser"
        arg          = text "par"
        parseHead    = do dom  <- askSt (fieldOf vc)
                          qdom <- qualifiedSort dom
                          rec  <- parseRecCall arg dom
                          return $ qdom <+> text "head" <+> equals <+> rec
        retMake q as = jreturn <+> rMethodCall q (text "make") as
        makeCons qc  = retMake qc [text "head", text "tail"]
        makeNil  qn  = retMake qn []
        parseTail qco qvc = qco <+> text "tail" <+> equals <+> 
                            rMethodCall qvc (text "parseTail") [arg]

-- | Given a variadic constructor @VC(T*)@, 
-- of codomain @Co@, generates
--
-- > static public mod.types.Co parseTail(mod.Parser par) {
-- >   if (par.isComma()) {
-- >     par.parseComma();
-- >     mod.types.T  head = mod.types.T.parse(par)
-- >     mod.types.Co tail = mod.types.vc.VC.parseTail(par);
-- >     return mod.types.vc.ConsVC.make(head,tail);
-- >   } else {
-- >     return mod.types.vc.EmptyVC.make();
-- >   }
-- > }
compParseVariadicTail :: CtorId -> Gen Doc
compParseVariadicTail vc = do
  pr    <- packagePrefix
  co    <- askSt (codomainOf vc)
  qco   <- qualifiedSort co
  qvc   <- qualifiedCtor vc
  qcons <- qualifiedCtor (prependCons vc)
  qnil  <- qualifiedCtor (prependEmpty vc)
  phead <- parseHead
  return $ rMethodDef 
    (static <+> public) qco (text "parseTail") [pars pr <+> arg]
    (rIfThenElse (text "par.isComma()")
       (rBody [text "par.parseComma()", phead,
               parseTail qco qvc ,makeCons qcons])
       (makeNil qnil <> semi))
  where pars pr      = pr <> dot <> text "Parser"
        arg          = text "par"
        parseHead    = do dom  <- askSt (fieldOf vc)
                          qdom <- qualifiedSort dom
                          rec  <- parseRecCall arg dom
                          return $ qdom <+> text "head" <+> equals <+> rec
        retMake q as = jreturn <+> rMethodCall q (text "make") as
        makeCons qc  = retMake qc [text "head", text "tail"]
        makeNil  qn  = retMake qn []
        parseTail qco qvc = qco <+> text "tail" <+> equals <+> 
                            rMethodCall qvc (text "parseTail") [arg]

-- | Given a non-variadic constructor @C@, generates a concrete class @C.java@.
compConstructor :: CtorId -> Gen FileHierarchy
compConstructor c = do mem  <- compMembersOfConstructor c
                       smem <- ifSh $ compSharingMembers c
                       ctor <- compCtorOfConstructor c
                       mak  <- compMakeOfConstructor c
                       get  <- compGettersOfConstructor c
                       set  <- compSettersOfConstructor c
                       tos  <- ifNG $ compToStringBuilder c
                       toh  <- ifConfM haskell (compToHaskellBuilder c) rempty
                       eqs  <- ifConfM sharing (compEquiv c) (compEquals c)
                       hac  <- ifConf sharing hashCodeMethod empty
                       dup  <- ifSh $ compDuplicate c
                       ini  <- ifSh $ compInit c
                       inh  <- ifSh $ compInitHash c
                       haf  <- ifSh $ compHashFun c
                       gcc  <- ifV  $ compGetChildCount c
                       gca  <- ifV  $ compGetChildAt c
                       gcs  <- ifV  $ compGetChildren c
                       sca  <- ifV  $ compSetChildAt c
                       scs  <- ifV  $ compSetChildren c
                       par  <- ifP  $ compParseConstructor c
                       ran  <- ifR  $ compMakeRandomConstructor c
                       dep  <- ifD  $ compDepthConstructor c 
                       siz  <- ifSi $ compSizeConstructor c 
                       let isc = compIsX c
                       let syn = compSymbolName c
                       let body = vcat [mem,smem,ctor,mak,syn,
                                        get,set,tos,toh,eqs,hac,
                                        dup,ini,inh,haf,isc,gcc,gca,
                                        gcs,sca,scs,par,ran,dep,siz]
                       cls  <- wrap body
                       return $ Class (show c) cls

  where rempty = return empty
        ifV  = flip (ifConfM visit  ) rempty
        ifSh = flip (ifConfM sharing) rempty
        ifP  = flip (ifConfM parsers) rempty
        ifR  = flip (ifConfM random ) rempty
        ifD  = flip (ifConfM depth  ) rempty
        ifSi = flip (ifConfM size   ) rempty
        -- ifNG == if not generated
        ifNG a = askSt (isGenerated c) >>= maybe a (const rempty)
        wrap b = do
          gen <- askSt (isGenerated c)                                     
          let rcls d = rClass public (pretty c) (Just d) [] b            
          case gen of Nothing -> do co  <- askSt (codomainOf c)
                                    qco <- qualifiedSort co
                                    return $ rcls qco
                      Just bc -> do qbc <- qualifiedCtor bc
                                    return $ rcls qbc

-- | Given a non-variadic constructor @C(x1:T1,..,xn:Tn)@,
-- generates @m.types.T1 x1; ...; m.types.Tn xn;@
compMembersOfConstructor :: CtorId -> Gen Doc
compMembersOfConstructor c = iterOverFields rdr rBody c
  where rdr f s = do qs <- qualifiedSort s
                     return $ private <+> qs <+> pretty f

-- | Given a non-variadic constructor @C(x1:T1,..,xn:Tn)@,
-- generates:
--
-- > private int hashCode;
-- > private static C proto = new C();
-- > private static int nameHash = 
-- >   shared.HashFunctions.stringHashFunction(mod.types.s.c,n);
compSharingMembers :: CtorId -> Gen Doc
compSharingMembers c = do
  qc  <- qualifiedCtor c
  len <- length `liftM` askSt (fieldsOf c) 
  return $ rBody [text "private static int nameHash" <+> equals <+>
                  rMethodCall (text "shared.HashFunctions")
                              (text "stringHashFunction") 
                              [dquotes qc, int len],
                  text "private int hashCode",
                  text "private static" <+> pretty c <+> 
                  text "proto = new" <+> pretty c <> text "()"]

-- | Given a non-variadic constructor @C(x1:T1,..,xn:Tn)@,
-- generates the constructor:
--
-- > private C(m.types.T1 x1, ..., m.types.Tn xn) {
-- >   this.x1 = x1;
-- >   ...
-- >   this.xn = xn;
-- > }
compCtorOfConstructor :: CtorId -> Gen Doc
compCtorOfConstructor c = ifConfM sharing ctorShr ctorNoShr
  where ctorShr   = return $ rMethodDef private empty (pretty c) [] empty
        ctorNoShr = do fis <- askSt (fieldsOf c)
                       a <- mapM rdr1 fis
                       let b = rBody $ map rdr2 fis
                       return $ rMethodDef private empty (pretty c) a b
          where rdr1 (f,s) = do qs <- qualifiedSort s
                                return $ qs <+> (text . show) f
                rdr2 (f,_) = this <> dot <> pretty f <+> equals <+> pretty f

-- | Given a non-variadic constructor @C(x1:T1,..,xn:Tn)@,
-- generates the make method:
--
-- > public static C make(m.types.T1 x1, ..., m.types.Tn xn) {
-- >  proto.initHashCode(x1,..,xn);
-- >  return (C) factory.build(proto);
-- > }
--
-- or the following if @--noSharing@ has been toggled:
--
-- > public static C make(m.types.T1 x1, ..., m.types.Tn xn) {
-- >   return new C(x1, ..., xn);
-- > } 
compMakeOfConstructor :: CtorId -> Gen Doc
compMakeOfConstructor c = ifConfM sharing cmakes cmake
  where -- the sharing case
        cmakes = do cfs <- cfields 
                    let call = rMethodCall proto inith (map (pretty.fst) cfs)
                    let ret  = jreturn <+> parens (pretty c) <+> build
                    makeDef $ rBody [call,ret]
          where proto = text "proto"
                inith = text "initHashCode"
                build = text "factory.build(proto)"
        -- the no sharing case
        cmake  = do cfs <- cfields
                    let b = newC (map (pretty . fst) cfs) <> semi
                    makeDef b
          where newC fs = jreturn <+> rConstructorCall (pretty c) fs
        -- takes a body bd and returns public static C make(...) { bd }
        makeDef bd = do cfs <- cfields 
                        a <- mapM rdr cfs
                        return $ rMethodDef (public <+> static) 
                                 (pretty c) (text "make") a bd
          where rdr (f,s) = do qs <- qualifiedSort s
                               return $ qs <+> (text .show) f
        -- the fields of c
        cfields = askSt (fieldsOf c)

-- | Given a non-variadic constructor @C(x1:T1,..,xn:Tn)@,
-- generates the methods: 
--
-- > public m.types.T1 getx1() { return x1; }
-- > ...
-- > public m.types.Tn getxn() { return xn; }
compGettersOfConstructor :: CtorId -> Gen Doc
compGettersOfConstructor = iterOverFields getter vcat
  where getter f s = do qs <- qualifiedSort s
                        let fun = text "get" <> pretty f
                        let b = rBody [jreturn <+> pretty f]
                        return $ rMethodDef public qs fun [] b 

-- | Given a non-variadic constructor @C(x1:T1,..,xn:Tn)@,
-- generates the methods: 
--
-- > public void setx1(m.types.T1 x1) 
-- >   { this.x1 = x1; }
-- > ...
-- > public void setxn(m.types.Tn xn) 
-- >   { this.xn = xn; }
compSettersOfConstructor :: CtorId -> Gen Doc
compSettersOfConstructor = iterOverFields setter vcat
  where setter f s = do qs <- qualifiedSort s
                        let fun = text "set" <> pretty f
                        let a = [pretty qs <+> pretty f]
                        let b = rBody [this <> dot <> pretty f 
                                       <+> equals <+> pretty f]
                        return $ rMethodDef public void fun a b 

-- | Given a non-variadic constructor @C(x1:T1,..,xn:Tn)@,
-- generates 
--
-- > public String toStringBuilder(java.lang.StringBuilder buf) {
-- >   buffer.append("C(");
-- >   x1.toStringBuilder(buf);
-- >   buffer.append(",");
-- >   ...
-- >   buffer.append(",");
-- >   xn.toStringBuilder(buf);
-- >   buffer.append(")");
-- > }
compToStringBuilder :: CtorId -> Gen Doc
compToStringBuilder c = do rcalls <- iterOverFields rcall id c
                           return $ rMethodDef 
                             public void (text "toStringBuilder")
                             [stringBuilder <+> text "buf"] (complete rcalls)
  where complete b = rBody $ open : intersperse apcomma b ++ [close]
        bapp arg   = text "buf.append" <> parens arg
        apcomma    = bapp $ dquotes comma
        open       = bapp $ dquotes (pretty c <> lparen)
        close      = bapp $ dquotes rparen
        rcall x s  = return $
          if isBuiltin s then renderBuiltin s x (text "buf")
                         else rMethodCall (this <> dot <> pretty x)
                                          (text "toStringBuilder") 
                                          [text "buf"]

-- | Given a non-variadic constructor @C(x1:T1,..,xn:Tn)@,
-- generates 
--
-- > public String toHaskellBuilder(java.lang.StringBuilder buf) {
-- >   buffer.append("(C");
-- >   buffer.append(" ");
-- >   x1.toStringBuilder(buf);
-- >   buffer.append(" ");
-- >   ...
-- >   buffer.append(" ");
-- >   xn.toStringBuilder(buf);
-- >   buffer.append(")");
-- > }
compToHaskellBuilder :: CtorId -> Gen Doc
compToHaskellBuilder c = do rcalls <- iterOverFields rcall id c
                            return $ rMethodDef 
                              public void (text "toHaskellBuilder")
                              [stringBuilder <+> text "buf"] (complete rcalls)
  where complete b = rBody $ open : addspaces b ++ [close]
        bapp arg   = text "buf.append" <> parens arg
        apspace    = bapp $ dquotes space
        addspaces  = foldr (\x r -> apspace:x:r) []
        open       = bapp $ dquotes (lparen <> pretty c)
        close      = bapp $ dquotes rparen
        rcall x s  = return $
          if isBuiltin s then renderBuiltin s x (text "buf")
                         else rMethodCall (this <> dot <> pretty x)
                                          (text "toHaskellBuilder") 
                                          [text "buf"]

-- | Given a non-variadic constructor @C(x1:T1,..,xn:Tn)@,
-- @compEquiv C@ generates 
--
-- > public boolean equivalent(shared.SharedObject o) {
-- >   if (o instanceof C) {
-- >     C typed_o = (C) o;
-- >     return true &&
-- >            this.x1 == o.x1 &&
-- >            ...
-- >            this.xn == o.xn;
-- >   } else {
-- >     return false;
-- >   }
-- > }
compEquiv :: CtorId -> Gen Doc
compEquiv = compEqAux meth comb jShared
  where meth = text "equivalent"
        comb lhs rhs = lhs <+> text "==" <+> rhs

-- | Given a non-variadic constructor @C(x1:T1,..,xn:Tn)@,
-- @compEquals C@ generates 
--
-- > public boolean equals(java.lang.Object o) {
-- >   if (o instanceof C) {
-- >     C typed_o = (C) o;
-- >     return true &&
-- >            this.x1.equals(o.x1) &&
-- >            ...
-- >            this.xn.equals(o.xn);
-- >   } else {
-- >     return false;
-- >   }
-- > }
compEquals :: CtorId -> Gen Doc
compEquals = compEqAux meth comb jObject
  where meth = text "equals"
        comb lhs rhs = rMethodCall lhs meth [rhs]

-- | Given a constructor @C(x1:T1,..,xn:Tn)@, generates
--
-- > public shared.SharedObject duplicate() {
-- >   C clone = new C();
-- >   clone.init(this.x1,..,this.xn,hashCode);
-- >   return clone;
-- > }
compDuplicate :: CtorId -> Gen Doc
compDuplicate c = rdr `liftM` askSt (fieldsOf c)
  where pc  = pretty c
        cl  = text "clone"
        th  = (text "this." <>) . pretty . fst
        rdr = rMethodDef public jShared (text "duplicate") [] . body
        body fis = rBody 
          [pc <+> cl <+> equals <+> rConstructorCall pc [],
           rMethodCall cl (text "init") (map th fis ++ [text "hashCode"]),
           jreturn <+> cl]

-- | Given a constructor @C(x1:T1,..,xn:Tn)@, generates
--
-- > private void init(T1 x1, ..., Tn xn, int hashCode) {
-- >   this.x1 = x1;
-- >   ...
-- >   this.xn = xn;
-- >   this.hashCode = hashCode;
-- > }
compInit :: CtorId -> Gen Doc 
compInit c = do cfs <- askSt $ fieldsOf c
                tfs <- mapM rdr cfs
                let args = tfs ++ [text "int hashCode"]
                let body = rBody $ map ass cfs ++ [lastLine]
                return $ rMethodDef private void (text "init") args body
  where rdr (f,s) = do qs <- qualifiedSort s
                       return $ qs <+> (text . show) f
        ass (f,s) = let pf = pretty f 
                    in this <> dot <> pf <+> equals <+> 
                       if isString s then pf <> text ".intern()" else pf
        lastLine  = text "this.hashCode = hashCode"

-- | Given a constructor @C(x1:T1,..,xn:Tn)@, generates
--
-- > private void initHashCode(T1 x1, ..., Tn xn) {
-- >   this.x1 = x1;
-- >   ...
-- >   this.xn = xn;
-- >   this.hashCode = hashFunction();
-- > }
compInitHash :: CtorId -> Gen Doc 
compInitHash c = do cfs <- askSt $ fieldsOf c
                    args <- mapM rdr cfs
                    let body = rBody $ map ass cfs ++ [lastLine]
                    return $ rMethodDef private void 
                                        (text "initHashCode") args body
  where rdr (f,s) = do qs <- qualifiedSort s
                       return $ qs <+> (text . show) f
        ass (f,s) = let pf = pretty f 
                    in this <> dot <> pf <+> equals <+> 
                       if isString s then pf <> text ".intern()" else pf
        lastLine  = text "this.hashCode = hashFunction()"

-- | Auxiliary function for @'compHashFun'@, generates the fields-related part of
-- the @hashFunction@ method.
hashArgs :: [(FieldId,SortId)] -> Int -> [Doc]
hashArgs fis len = zipWith (\i (f,s) -> hashArg i f s) (desc (len -1)) fis
  where desc n = n:desc (n-1)

-- | Auxiliary function for @'hashArgs'@, generates the recursive call
-- to the @hashFunction@ method for a non-builin field, ad-hoc magic
-- otherwise.
hashArg :: Int -> FieldId -> SortId -> Doc
hashArg idx fid sid = let res d = char accum <+> text "+=" <+> parens d in
                      if shift == 0 then res go
                                    else res $ go <+> text "<<" <+> int shift
  where go | isILFC    sid = pfid
           | isBoolean sid = pfid <> text "?1:0"
           | isDouble  sid = text "(int)" <> toLong pfid <> text "^" <>
                             parens (toLong pfid) <> text ">>>32"
           | otherwise     = rMethodCall pfid (text "hashCode") []
        shift = (idx `mod` 4) * 8
        accum = "aaaabbbbcccc" !! (idx `mod` 12)
        isILFC s = any ($ s) [isInt,isLong,isFloat,isChar]
        toLong x = text "java.lang.Double.doubleToLongBits" <> parens x
        pfid = this <> dot <> pretty fid

-- | Given a constructor @C(x1:T1,..,xn:Tn)@, generates
--
-- > protected int hashFunction() {
-- > int a, b, c;
-- >   a = 0x9e3779b9;
-- >   b = nameHash << 8;
-- >   c = n;
-- >   hashArgs [(x1,T1),..,(xn,Tn)] n
-- >   a -= b; a -= c; a ^= (c >> 13);
-- >   b -= c; b -= a; b ^= (a << 8);
-- >   c -= a; c -= b; c ^= (b >> 13);
-- >   a -= b; a -= c; a ^= (c >> 12);
-- >   b -= c; b -= a; b ^= (a << 16);
-- >   c -= a; c -= b; c ^= (b >> 5);
-- >   a -= b; a -= c; a ^= (c >> 3);
-- >   b -= c; b -= a; b ^= (a << 10); 
-- >   c -= a; c -= b; c ^= (b >> 15);
-- >   return c;
-- > }
compHashFun :: CtorId -> Gen Doc
compHashFun c = do fis <- askSt (fieldsOf c)
                   let len = length fis
                   let modif = protected <+> if len == 0 then static else empty 
                   return $ rMethodDef modif jint 
                                       (text "hashFunction") [] (body fis len)
  where body f l = rBody (prologue ++ mid f l ++ epilogue)
        prologue = map text ["int a, b, c",
                             "a = 0x9e3779b9",
                             "b = nameHash << 8"]
        mid f l  = (text "c =" <+> int l) : hashArgs f l
        epilogue = map text ["a -= b; a -= c; a ^= (c >> 13)",
                             "b -= c; b -= a; b ^= (a << 8)" ,
                             "c -= a; c -= b; c ^= (b >> 13)",
                             "a -= b; a -= c; a ^= (c >> 12)",
                             "b -= c; b -= a; b ^= (a << 16)",
                             "c -= a; c -= b; c ^= (b >> 5)" ,
                             "a -= b; a -= c; a ^= (c >> 3)" ,
                             "b -= c; b -= a; b ^= (a << 10)",
                             "c -= a; c -= b; c ^= (b >> 15)",
                             "return c"]

-- | Given a constructor @c@ of arity @n@, generates
--
-- > public int getChildCount() {
-- >   return n;
-- > }
compGetChildCount ::  CtorId -> Gen Doc
compGetChildCount c = do ar <- length `liftM` askSt (fieldsOf c)
                         return $ wrap ar
  where wrap n = rMethodDef public jint (text "getChildCount") 
                            [] (jreturn <+> int n <> semi)

-- | Given a constructor @c@ of fields @x1,..,xn@ generates
--
-- > public tom.library.sl.Visitable getChildAt(int n) {
-- >   switch(n) {
-- >     case 0: return this.x1;
-- >     ...
-- >     case n-1: return this.xn;
-- >     default: throw new IndexOutOfBoundsException();
-- >   }
-- > }
--
-- Common.Builtins are boxed in @tom.library.sl.VisitableBuiltin@.
compGetChildAt :: CtorId -> Gen Doc
compGetChildAt c = do fis <- askSt (fieldsOf c)
                      let cs  = zip (map int [0..]) (map cook fis)
                      let arg = text "n"
                      return $ rMethodDef 
                                 public jVisitable (text "getChildAt")
                                 [jint <+> arg] (body arg cs)
  where cook (f,s)  = jreturn <+> wrap (this <> dot <> pretty f) s <> semi 
        body n cs   = rSwitch n cs (Just outOfBounds)
        outOfBounds = text "throw new IndexOutOfBoundsException();"
        wrap f s | isBuiltin s = rConstructorCall (rWrapBuiltin qs) [f]
                 | otherwise   = f
          where qs = qualifiedBuiltin s

-- | Given a constructor @c@ of fields @x1,..,xn@ generates
--
-- > public tom.library.sl.Visitable[] getChildren() {
-- >   return new tom.library.sl.Visitable[] {
-- >     this.x1, ..., this.xn
-- >   };
-- > }
--
-- Common.Builtins are boxed in @tom.library.sl.VisitableBuiltin@.
compGetChildren :: CtorId -> Gen Doc
compGetChildren c = do fis <- askSt (fieldsOf c)
                       return $ rMethodDef public jVisitableArray
                                           (text "getChildren")
                                           [] (body fis)
  where body fs = let cs = align . sep . punctuate comma $ map child fs
                  in jreturn <+> new <+> jVisitableArray <+> ibraces cs <> semi
        child (f,s) =
          if isBuiltin s then rConstructorCall (rWrapBuiltin qs) [df] else df
            where qs = qualifiedBuiltin s
                  df = this <> dot <> pretty f

-- | Given a constructor @c@ of fields @x1,..,xn@ generates
--
-- > public tom.library.sl.Visitable setChildAt(tom.library.sl.Visitable v) {
-- >   switch(n) {
-- >     case 0: return c.make((m.foo.T1) x1,this.x2,..,this.xn);
-- >     ...
-- >     case n-1: return c.make(this.x1,...,(m.foo.Tn) this.xn);
-- >     default: throw new IndexOutOfBoundsException();
-- >   }
-- > }
--
-- Common.Builtins are unboxed from @tom.library.sl.VisitableBuiltin@.
compSetChildAt :: CtorId -> Gen Doc
compSetChildAt c = do fis  <- askSt (fieldsOf c)
                      fis' <- mapM set (parts fis)
                      let cs  = zip (map int [0..]) fis'
                      return $ rMethodDef 
                                 public jVisitable (text "setChildAt")
                                 [jint <+> text "n", jVisitable <+> text "v"] 
                                 (body cs)
  where body cs     = rSwitch (text "n") cs (Just outOfBounds)
        outOfBounds = text "throw new IndexOutOfBoundsException();"
        set (xs1,(_,t),xs2) = 
          let f (x,_) = this <> dot <> pretty x
              dxs1    = map f xs1
              dxs2    = map f xs2
          in do dx <- cast t
                let args = dxs1++[dx]++dxs2
                let call = rMethodCall (pretty c) (text "make") args
                return $ jreturn <+> call <> semi
        parts l = go [] l where go _  []     = []
                                go xs [x]    = [(xs,x,[])]
                                go xs (x:ys) = (xs,x,ys) : go (xs++[x]) ys
        cast t = if isBuiltin t 
                   then let qbt = rWrapBuiltin (qualifiedBuiltin t)
                            cas = parens (parens qbt <+> text "v")
                        in return $ rMethodCall cas (text "getBuiltin") []
                   else do qt <- qualifiedSort t
                           return $ parens qt <+> text "v"

-- | Given a constructor @c@ of fields @x1,..,xn@ generates
--
-- > public tom.library.sl.Visitable 
-- >   setChildren(tom.library.sl.Visitable[] cs) {
-- >   if (cs.length == n-1 && 
-- >       cs[0] instanceof T0 &&
-- >       ..
-- >       cs[n] instanceof Tn) {
-- >        return c.make(cs[0],..,cs[n])
-- >   } else {
-- >     throw new IndexOutOfBoundsException();
-- >   }
-- > }
--
-- Common.Builtins are unboxed from @tom.library.sl.VisitableBuiltin@.
compSetChildren :: CtorId -> Gen Doc 
compSetChildren c = do cs  <- askSt (fieldsOf c)
                       csn <- zipWithM cook [0..] cs
                       let cd  = cond csn
                       let bd  = body csn
                       let ite = rIfThenElse cd bd er
                       return $ rMethodDef 
                                  public jVisitable (text "setChildren")
                                  [jVisitableArray <+> text "cs"] ite
  where cond csn = let cl = checkLen (length csn)
                       ci = map checkInstance csn
                   in align . fillSep $ intersperse (text "&&") (cl:ci)
        checkLen n = text "cs.length ==" <+> int n
        checkInstance (n,t,qt) = nth n <+> instanceof <+> td
          where td = if isBuiltin t 
                       then text "tom.library.sl.VisitableBuiltin"
                       else pretty qt
        body csn = let call = rMethodCall (pretty c) (text "make") (map r csn)
                   in jreturn <+> call <> semi
          where r (n,t,qt) | isBuiltin t = rMethodCall cas (text "getBuiltin") []
                           | otherwise   = parens (pretty qt) <+> nth n 
                  where wqt = rWrapBuiltin (qualifiedBuiltin t)
                        cas = parens (parens wqt <+> nth n)
        er = text "throw new IndexOutOfBoundsException();"
        nth n = text "cs" <> brackets (int n)
        cook n (_,t) = do qt <- qualifiedSort t
                          return (n,t,qt)

-- | Given a constructor C, generates
--
-- > public boolean isC() { return true; }
compIsX :: CtorId -> Doc
compIsX c = let fun = text "is" <> pretty c
                b   = rBody [jreturn <+> jtrue]
            in rMethodDef public jboolean fun [] b 

-- | @compSymbolName c@ renders 
--
-- > public String symbolName() {
-- >   return "c";
-- > }
compSymbolName 
  :: CtorId -- ^ constructor name
  -> Doc
compSymbolName c =
  text "public String symbolName()" <+> 
  ibraces (rBody [jreturn <+> dquotes (pretty c)])

-- | Auxiliary function for 'compEquals' and 'compEquiv'. 
-- Given a non-variadic constructor @C(x1:T1,..,xn:Tn)@,
-- a method name @method@, a type name @ty@ and a combinator @comb@, 
-- @compEqAux method comb ty C@ generates 
--
-- > public boolean method(ty o) {
-- >   if (o instanceof C) {
-- >     C typed_o = (C) o;
-- >     return true &&
-- >            this.x1 `comb` o.x1 &&
-- >            ...
-- >            this.xn `comb` o.xn;
-- >   } else {
-- >     return false;
-- >   }
-- > }
compEqAux :: Doc -> (Doc -> Doc -> Doc) -> Doc -> CtorId -> Gen Doc
compEqAux meth comb ty c = do rcalls <- iterOverFields rcall id c
                              return $ rMethodDef 
                                (public <+> final) jboolean meth
                                [ty <+> text "o"] (complete rcalls)
  where cdoc = pretty c
        complete b = rIfThenElse cond (branch1 b) (jreturn <+> jfalse <> semi) 
        cond       = text "o" <+> instanceof <+> cdoc
        branch1 b  = rBody [l1,l2 (jtrue:b)]
        l1 = cdoc <+> text "typed_o" <+> equals <+> parens cdoc <+> text "o"
        l2 b = jreturn <+> (align . fillSep $ intersperse (text "&&") b)
        rcall x s = let lhs = this <> dot <> pretty x
                        rhs = text "typed_o." <> pretty x 
                    in return $ if isBuiltin s
                                  then lhs <+> text "==" <+> rhs
                                  else lhs `comb` rhs


-- | Given a sort @s@, @randomRecCall s@ generates
--
--  * @mod.modAbstractType.randoms(rand)@ if @s@ is a builtin, 
--
--  * @mod.types.s.makeRandom(rand,depth)@ otherwise
randomRecCall :: SortId -> Gen Doc
randomRecCall s = do
  qs <- qualifiedSort s
  at <- qualifiedAbstractType
  return $ if isBuiltin s 
    then rMethodCall at (text "random" <> pretty s) [text "rand"]
    else rMethodCall qs (text "makeRandom") [text "rand", text "depth"]


-- | Given a constructor @C(x1:T1,...,xn:Tn)@, 
-- of codomain @Co@, generates
--
-- > final static public
-- > mod.types.Co makeRandom(java.util.Random rand, int depth) {
-- >   return
-- >   mod.types.co.C.make(mod.types.T1.makeRandom(rand,depth),
-- >                       ...,
-- >                       mod.types.Tn.makeRandom(rand,depth));
-- > }
compMakeRandomConstructor :: CtorId -> Gen Doc
compMakeRandomConstructor c = do
  qc  <- qualifiedCtor c
  co  <- askSt (codomainOf c)
  qco <- qualifiedSort co
  tys <- map snd `liftM` askSt (fieldsOf c)
  rcalls <- mapM randomRecCall tys
  return $ rMethodDef (final <+> static <+> public)
                      qco (text "makeRandom")
                      [text "java.util.Random rand", text "int depth"] 
                      (body qc rcalls)
  where body qc rc = jreturn <+> rMethodCall qc (text "make") rc <> semi

-- | Given a constructor @C(x1:T1,...,xn:Tn)@, generates
--
-- > final public static int depth() {
-- >   int max = 0;
-- >   int cd = 0;
-- >   cd = x1.depth();
-- >   if (cd > max) max = cd;
-- >   ...
-- >   cd = xn.depth();
-- >   if (cd > max) max = cd;
-- >   return max + 1;
-- > }
compDepthConstructor :: CtorId -> Gen Doc
compDepthConstructor c = do
  fis <- askSt (fieldsOf c)
  return .wrap $ if null fis then text "return 0;" else pack fis
  where call (f,s) | isBuiltin s = []
                   | otherwise = [this <> dot <> pretty f <> text ".depth();",
                                  text "if (cd > max) max = cd;"]
        pack l = vcat ([pre] ++ mid ++ [post]) 
           where pre  = text "int max = 0; int cd = 0;"
                 mid  = concatMap call l
                 post = text "return max + 1;"
        wrap d = text "final public int depth()" <+> ibraces d

-- > final public static int size() {
-- >   return x1.size() + ... + xn.size();
-- > }
compSizeConstructor :: CtorId -> Gen Doc
compSizeConstructor c = do
  fis <- askSt (fieldsOf c)
  return . pack $ if null fis then one else add (map call fis)
  where add = align . fillSep . intersperse (text "+")
        one = text "1"
        call (f,s) | isBuiltin s = one
                   | otherwise   = this <> dot <> pretty f 
                                   <> text ".size()"
        pack d = text "final public int size()" 
                 <+> ibraces (jreturn <+> d <> semi)
