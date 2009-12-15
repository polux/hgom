module Gom.CodeGen where

import Gom.Sig
import Gom.SymbolTable
import Gom.Java
import Gom.Constants
import Gom.Config

import Text.PrettyPrint.Leijen
import Control.Monad.Reader
import Control.Arrow((***))
import Data.Char(toLower)
import Data.List(nub, intersperse)

-- | Compiles a symbol table into a Java hierarchy
st2java :: SymbolTable -> Config -> FileHierarchy
st2java st c = runReader compSt (st,c)

-- | Turns a 'String' into lowercase.
lower :: String -> String
lower = map toLower

-- | Returns the package prefix ended by a dot.
--
-- As an example, returns @aa.bb.cc.foo@ for the module @Foo@,
-- provided the user toggled @-p aa.bb.cc@.
packagePrefix :: Gen Doc
packagePrefix = do m <- lower `liftM` askSt modName
                   go (pretty m) `liftM` askConf package
  where go dm Nothing  = dm
        go dm (Just l) = hcat . intersperse dot $ (map text l)++[dm]

-- | Given a sort @S@, returns @module.types.S@
qualifiedSort :: SortId -> Gen Doc
qualifiedSort s 
  | isBuiltin s = return $ pretty s
  | otherwise   = do p <- packagePrefix
                     return $ p <> dot <> text "types" <> dot <> pretty s

-- | Given a constructor @C@ of codomain @S@, returns
-- @module.types.s.C@
qualifiedCtor :: CtorId -> Gen Doc
qualifiedCtor c = 
  do p <- packagePrefix
     lows <- lowerSortId `liftM` askSt (codomainOf c)
     return $ p <> dot <> text "types" <> dot <> 
              pretty lows <> dot <> pretty c

-- | Generates @mAbstractType@ for the current module @m@.
abstractType :: Gen String
abstractType = do mn <- askSt modName
                  return $ mn ++ "AbstractType"

-- | Generates @m.mAbstractType@ for the current module @m@.
qualifiedAbstractType :: Gen Doc
qualifiedAbstractType = do at <- abstractType
                           pr <- packagePrefix 
                           return $ pr <> dot <> (text at)

-- | A computation inside a context containing a read-only symbol table.
type Gen a = Reader (SymbolTable,Config) a

-- | Query symbol table.
askSt :: (SymbolTable -> a) -> Gen a
askSt f = asks (f . fst)

-- | Query configuration.
askConf :: (Config -> a) -> Gen a
askConf f = asks (f . snd)

-- | @switch f d e@ is @e@ if @f config@ holds, else @return d@ .
--
-- Example usage: 
--
-- > do hs <- switch haskell [] [rMethod ...]
-- >    return rClass ... (vcat $ defaultMethods ++ hs)
switch :: (Config -> Bool) -> a -> a -> Gen a
switch f d e = mswitch f d (return e) 

-- | @mswitch f d e@ is @return e@ if @f config@ holds, else @return d@ .
--
-- Example usage: 
--
-- > do hs <- switch haskell [] compSomeMethods
-- >    return rClass ... (vcat $ defaultMethods ++ hs)
mswitch :: (Config -> Bool) -> a -> Gen a -> Gen a
mswitch f d e = do cond <- askConf f
                   if cond then e else return d

-- | Generates @%include { x.tom }@ for every imported sort @x@
compIncludes :: Gen Doc
compIncludes = do is <- askSt importedSorts
                  return $ vcat (map rdr is)
  where rdr s = text "%include" <+> 
                sbraces (builtinImport s)

-- | Generates the whole file hierarchy of the \"global\" symbol table.
compSt :: Gen FileHierarchy
compSt = do mn <- lower `liftM` askSt modName
            ds <- askSt definedSortsIds
            hs <- mapM compSort ds
            ac <- compAbstract
            tf <- compTomFile
            pr <- askConf package
            return . wrap pr $ Package mn [ac,tf,Package "types" (concat hs)]
  where -- wraps the package in the user-provided prefix hierarchy (-p option)
        wrap Nothing  h = h
        wrap (Just l) h = foldr w h l
        w p h = Package p [h]
         

-- | Generates the @ModAbstractType@ abstract java class for module @Mod@.
compAbstract :: Gen FileHierarchy
compAbstract = do at <- abstractType
                  -- if haskell option is enabled, generate toHaskell
                  hs <- switch haskell [] hask
                  -- if visit option is enabled, implement visitable 
                  iv <- switch visit   [] [jVisitable]
                  -- if sharing option is enabled, implement shared
                  is <- switch sharing [] [jShared] 
                  -- if String is imported we generate renderString
                  im <- askSt importsString
                  let rs = if im then str else []
                  -- build the class
                  return $ Class at (cl at (hs++rs) (iv++is))
  where cl at e i = rClass (public <+> abstract) (text at) 
                         Nothing i (body e)
        body e    = vcat $ always ++ e
        always    = [abstractSymbolName,toStringBody,abstractToStringBuilder]
        hask      = [toHaskellBody,abstractToHaskellBuilder]
        str       = [renderStringMethod] 

-- | Generates the @Mod.tom@ tom mappings file for module @Mod@
compTomFile :: Gen FileHierarchy
compTomFile = do mn    <- askSt modName
                 srts  <- askSt definedSortsIds
                 ctrs  <- askSt simpleConstructorsIds
                 vctrs <- askSt variadicConstructorsIds
                 incls <- compIncludes
                 tyts  <- mapM compTypeTerm srts
                 ops   <- mapM compOp ctrs
                 opls  <- mapM compOpList vctrs
                 return $ Tom mn (vsep $ incls:(tyts++ops++opls))

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
                return $ [ac, Package (show $ lowerSortId s) (avs ++ ccs)] 

-- | Helper fonction for 'compEmptyGettersOfSort' and 
-- 'compEmptySettersOfSort'. Iters the first argument on the fields of the
-- subject sort and combines them with the second one. Duplicate fields are
-- merged.
iterOverSortFields 
  :: (SortId -> FieldId -> SortId -> Gen a) -- ^ gets codomain, field name and field sort
  -> ([a] -> b) -- ^ combinator of results
  -> SortId     -- ^ subject sort
  -> Gen b
iterOverSortFields f g s = do cs <- askSt (sCtorsOf s)
                              fs <- (nub . concat) `liftM` mapM combine cs
                              ms <- mapM (\(co,(fi,ty)) -> f co fi ty) fs
                              return $ g ms
   where combine c = do fis <- askSt (fieldsOf c)
                        co  <- askSt (codomainOf c)
                        return $ map ((,) co) fis

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

-- | Given a sort @S@, generates an abstract class @S.java@.
compAbstractSort :: SortId -> Gen FileHierarchy
compAbstractSort s = do eg <- compEmptyGettersOfSort s
                        es <- compEmptySettersOfSort s
                        ei <- compEmptyIsX s
                        cl <- wrap $ vcat [eg,es,ei]
                        return $ Class (show s) cl
  where wrap body = do qat <- qualifiedAbstractType
                       return $ rClass (public <+> abstract) 
                                       (pretty s) (Just qat)  
                                       [] body

-- | Given a variadic constructor @VC@, generates an abstract class @VC.java@.
compAbstractVariadic :: CtorId -> Gen FileHierarchy
compAbstractVariadic vc = do cl <- body
                             return $ Class (show vc) cl
  where body = do co  <- askSt (codomainOf vc)
                  qto <- qualifiedSort co
                  return $ rClass (public <+> abstract)
                                  (pretty vc) (Just qto)
                                  [] empty 

-- | Given a non-variadic constructor @C@, generates a concrete class @C.java@.
compConstructor :: CtorId -> Gen FileHierarchy
compConstructor c = do mem  <- compMembersOfConstructor c
                       ctor <- compCtorOfConstructor c
                       get  <- compGettersOfConstructor c
                       set  <- compSettersOfConstructor c
                       tos  <- compToStringBuilder c
                       toh  <- mswitch haskell empty (compToHaskellBuilder c)
                       eqs  <- compEqualsConstructor c
                       gcc  <- ifv $ compGetChildCount c
                       gca  <- ifv $ compGetChildAt c
                       gcs  <- ifv $ compGetChildren c
                       sca  <- ifv $ compSetChildAt c
                       scs  <- ifv $ compSetChildren c
                       let isc = compIsX c
                       let syn = compSymbolName c
                       let body = vcat [mem,ctor,syn,get,set,tos,toh,
                                        eqs,isc,gcc,gca,gcs,sca,scs]
                       cls  <- wrap body
                       return $ Class (show c) cls
  where wrap b = do
          gen <- askSt (isGenerated c)                                     
          let rcls d = rClass public (pretty c) (Just d) [] b              
          case gen of Nothing -> do co  <- askSt (codomainOf c)            
                                    qco <- qualifiedSort co                     
                                    return $ rcls qco                           
                      Just bc -> do qbc <- qualifiedCtor bc                     
                                    return $ rcls qbc                        
        ifv = mswitch visit empty

-- | Helper fonction that iters over the fields of
-- a constructor and combines them.
iterOverFields 
  :: (FieldId -> SortId -> Gen a) -- ^ the function to iter
  -> ([a] -> b)                   -- ^ the combinator
  -> CtorId                       -- ^ the constructor
  -> Gen b
iterOverFields f g c = do fis  <- askSt (fieldsOf c)
                          fis' <- mapM (uncurry f) fis
                          return $ g fis'

-- | @renderBuiltin s f b@ generates what is necessary to put
-- the representation of @f@ (field of sort @s@) in the buffer @b@.
renderBuiltin 
  :: SortId -> FieldId -> Doc -> Doc
renderBuiltin s f b =
  if s == makeSortId "String" 
    then text "renderString" <> parens (b <> comma <> pretty f)
    else if s == makeSortId "char"
           then rMethodCall b (text "append") [fMinus0]
           else rMethodCall b (text "append") [pretty f]
  where fMinus0 = text "(int)" <> pretty f <> text " - (int)'0'"

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
  where complete b = rBody $ open:(intersperse apcomma b)++[close]
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
  where complete b  = rBody $ open:(addspaces b)++[close]
        bapp arg    = text "buf.append" <> parens arg
        apspace     = bapp $ dquotes space
        addspaces l = foldr (\x r -> apspace:x:r) [] l
        open        = bapp $ dquotes (lparen <> pretty c)
        close       = bapp $ dquotes rparen
        rcall x s   = return $
          if isBuiltin s then renderBuiltin s x (text "buf")
                         else rMethodCall (this <> dot <> pretty x)
                                          (text "toHaskellBuilder") 
                                          [text "buf"]

-- | Given a non-variadic constructor @C(x1:T1,..,xn:Tn)@,
-- generates 
--
-- > public boolean equals(java.lang.Object o) {
-- >   if (o instanceof C) {
-- >     C typed_o = (C) o;
-- >     return true &&
-- >            this.x1.equals(typed_o.getx1()) &&
-- >            ...
-- >            this.xn.equals(typed_o.getxn());
-- >   } else {
-- >     return false;
-- >   }
-- > }
compEqualsConstructor :: CtorId -> Gen Doc
compEqualsConstructor c = do rcalls <- iterOverFields rcall id c
                             return $ rMethodDef 
                               (public <+> final) jboolean (text "equals")
                               [jObject <+> text "o"] (complete rcalls)
  where cdoc = pretty c
        complete b = rIfThenElse cond (branch1 b) (jreturn <+> jfalse <> semi) 
        cond       = text "o" <+> instanceof <+> cdoc
        branch1 b  = rBody [l1,l2 (jtrue:b)]
        l1 = cdoc <+> text "typed_o" <+> equals <+> parens cdoc <+> text "o"
        l2 b = jreturn <+> (align . fillSep $ intersperse (text "&&") b)
        rcall x s = let lhs = this <> dot <> pretty x
                        rhs = text "typed_o.get" <> pretty x <> text "()"
                    in return $ if isBuiltin s
                                  then lhs <+> text "==" <+> rhs
                                  else rMethodCall lhs (text "equals") [rhs]

-- | Given a constructor @c@ of arity @n@, generates
--
-- > public int getChildCount() {
-- >   return n;
-- > }
compGetChildCount :: CtorId -> Gen Doc
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
-- Builtins are boxed in @tom.library.sl.VisitableBuiltin@.
compGetChildAt :: CtorId -> Gen Doc
compGetChildAt c = do fis <- askSt (fieldsOf c)
                      let cs  = zip (map int [0..]) (map cook fis)
                      let arg = text "n"
                      return $ rMethodDef 
                                 public jVisitable (text "getChildAt")
                                 [jint <+> arg] (body arg cs)
  where cook (f,s)  = jreturn <+> wrap (this <> dot <> pretty f) s <> semi 
        body n cs   = rSwitch n cs outOfBounds
        outOfBounds = text "throw new IndexOutOfBoundsException();"
        wrap f s | isBuiltin s = new <+> rWrapBuiltin qs <> parens f
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
-- Builtins are boxed in @tom.library.sl.VisitableBuiltin@.
compGetChildren :: CtorId -> Gen Doc
compGetChildren c = do fis <- askSt (fieldsOf c)
                       return $ rMethodDef public jVisitableArray
                                           (text "getChildren")
                                           [] (body fis)
  where body fs = let cs = align . sep . punctuate comma $ map child fs
                  in jreturn <+> new <+> jVisitableArray <+> ibraces cs <> semi
        child (f,s) =
          if isBuiltin s then new <+> rWrapBuiltin qs <> parens df else df
            where qs = qualifiedBuiltin s
                  df = this <> dot <> pretty f

-- | Given a constructor @c@ of fields @x1,..,xn@ generates
--
-- > public tom.library.sl.Visitable setChildAt(tom.library.sl.Visitable v) {
-- >   switch(n) {
-- >     case 0: return new c((m.foo.T1) x1,this.x2,..,this.xn);
-- >     ...
-- >     case n-1: return new c(this.x1,...,(m.foo.Tn) this.xn);
-- >     default: throw new IndexOutOfBoundsException();
-- >   }
-- > }
--
-- Builtins are unboxed from @tom.library.sl.VisitableBuiltin@.
compSetChildAt :: CtorId -> Gen Doc
compSetChildAt c = do fis  <- askSt (fieldsOf c)
                      fis' <- mapM set (parts fis)
                      let cs  = zip (map int [0..]) fis'
                      return $ rMethodDef 
                                 public jVisitable (text "setChildAt")
                                 [jint <+> text "n", jVisitable <+> text "v"] 
                                 (body cs)
  where body cs     = rSwitch (text "n") cs outOfBounds
        outOfBounds = text "throw new IndexOutOfBoundsException();"
        set (xs1,(_,t),xs2) = 
          let f (x,_) = this <> dot <> pretty x
              dxs1    = map f xs1
              dxs2    = map f xs2
          in do dx <- cast t
                let args = encloseCommas $ dxs1++[dx]++dxs2
                return $ jreturn <+> new <+> pretty c <> args <> semi
        parts l = go [] l where go _  []     = []
                                go xs [x]    = [(xs,x,[])]
                                go xs (x:ys) = (xs,x,ys):(go (xs++[x]) ys)
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
-- >        return new c(cs[0],..,cs[n])
-- >   } else {
-- >     throw new IndexOutOfBoundsException();
-- >   }
-- > }
--
-- Builtins are unboxed from @tom.library.sl.VisitableBuiltin@.
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
        body csn = jreturn <+> new <+> pretty c <+> 
                   encloseCommas (map r csn) <> semi
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

-- | Given a non-variadic constructor @C(x1:T1,..,xn:Tn)@,
-- generates @m.types.T1 x1; ...; m.types.Tn xn;@
compMembersOfConstructor :: CtorId -> Gen Doc
compMembersOfConstructor = iterOverFields rdr rBody
  where rdr f s = do qs <- qualifiedSort s
                     return $ private <+> qs <+> pretty f

-- | Given a non-variadic constructor @C(x1:T1,..,xn:Tn)@,
-- generates the constructor:
--
-- > public C(m.types.T1 x1, ..., m.types.Tn xn) {
-- >   this.x1 = x1;
-- >   ...
-- >   this.xn = xn;
-- > }
compCtorOfConstructor :: CtorId -> Gen Doc
compCtorOfConstructor c = 
  do fis <- askSt (fieldsOf c)
     a <- mapM rdr1 fis
     let b = rBody $ map rdr2 fis
     return $ rMethodDef public empty (pretty c) a b
  where rdr1 (f,s) = do qs <- qualifiedSort s
                        return $ qs <+> (text .show) f
        rdr2 (f,_) = this <> dot <> pretty f <+> equals <+> pretty f

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

-- | Given a sort @S@, generates
--
-- > %typeterm S {
-- >   implement { m.types.S }
-- >   is_sort(t) { ($t instanceof m.types.S) }
-- >   equals(t1,t2) { ($t1.equals($t2)) }
-- > }
compTypeTerm :: SortId -> Gen Doc
compTypeTerm s = do qs <- qualifiedSort s
                    return $ rTypeterm (pretty s) qs

-- | Given a non-variadic constructor @C(x1:T1,..,xn:Tn)@
-- of codomain @Co@, generates
--
-- > %op Co C(x1:T1,..,xn:Tn) {
-- >   is_fsym(t) { ($t instanceof m.types.C) }
-- >   get_slot(x1,t) { $t.getx1() }
-- >   ...
-- >   get_slot(xn,t) { $t.getxn() }
-- >   make (t1,..,tn) { (new m.types.co.c($t1,..,$tn)) }
-- > }
compOp :: CtorId -> Gen Doc
compOp c = do isfsym <- compIsFsym
              slots  <- iterOverFields compSlot vcat c
              s      <- askSt (codomainOf c)
              fis    <- askSt (fieldsOf c)
              let pfis = map (pretty *** pretty) fis
              make   <- compMake (map fst pfis)
              return $ rOp (pretty s) (pretty c) pfis
                           (vcat [isfsym,slots,make])
  where compIsFsym   = do qc <- qualifiedCtor c
                          return $ rIsFsym qc
        compSlot x _ = return $ rGetSlot (pretty x)
        compMake xs  = do qc <- qualifiedCtor c
                          return $ rMake qc xs 

-- | Given a variadic constructor @VC(T*)@
-- of codomain @Co@, generates
--
-- > %oplist Co VC(T*) {
-- >   is_fsym(t) { (    ($t instanceof foo.types.co.ConsVC) 
-- >                  || ($t instanceof foo.types.co.EmptyVC)) }
-- >   make_empty() { new foo.types.co.EmptyVC() }
-- >   make_insert(e,l) { new foo.types.co.ConsVC($e,$l) }
-- >   get_head(l) { $l.getHeadVC() }
-- >   get_tail(l) { $l.getTailVC() }
-- >   is_empty(l) { $l.isEmptyVC() }
-- > }
compOpList :: CtorId -> Gen Doc
compOpList c = do co     <- askSt (codomainOf c)
                  dom    <- askSt (fieldOf c)
                  consc  <- qualifiedCtor (prependCons c)
                  emptyc <- qualifiedCtor (prependEmpty c)
                  return $ rOpList (pretty c) (pretty co) 
                                   (pretty dom) consc emptyc
