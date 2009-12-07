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
     lows <- lowerSortId `liftM` askSt (flip codomainOf c)
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

-- | Generates @%include { x.tom }@ for every imported sort @x@
compIncludes :: Gen Doc
compIncludes = do is <- askSt importedSorts
                  return $ vcat (map rdr is)
  where rdr s = text "%include" <+> sbraces (pretty s <> text ".tom")

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
                  return $ Class at (cl at)
  where cl at = rClass (public <+> abstract) (text at) Nothing Nothing body
        body = toStringBody <$> abstractToStringBuilder

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
                vctrs <- askSt (flip vCtorsOf s)
                ctrs  <- askSt (flip sCtorsOf s)
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
iterOverSortFields f g s = do cs <- askSt (flip sCtorsOf s)
                              fs <- (nub . concat) `liftM` mapM combine cs
                              ms <- mapM (\(co,(fi,ty)) -> f co fi ty) fs
                              return $ g ms
   where combine c = do fis <- askSt (flip fieldsOf c)
                        co  <- askSt (flip codomainOf c)
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
compEmptyIsX s = do cs  <- askSt (flip sCtorsOf s) 
                    return . vcat $ map isx cs
  where isx f = let fun = text "is" <> pretty f
                    b   = rBody [jreturn <+> jfalse]
                in rMethodDef public jboolean fun [] b 

-- | Given a sort @S@, generates an abstract class @S.java@.
compAbstractSort :: SortId -> Gen FileHierarchy
compAbstractSort s = do eg <- compEmptyGettersOfSort s
                        es <- compEmptySettersOfSort s
                        ei <- compEmptyIsX s
                        cl <- wrap $ eg <$> es <$> ei
                        return $ Class (show s) cl
  where wrap body = do qat <- qualifiedAbstractType
                       return $ rClass (public <+> abstract) 
                                       (pretty s) (Just qat)  
                                       Nothing body

-- | Given a variadic constructor @VC@, generates an abstract class @VC.java@.
compAbstractVariadic :: CtorId -> Gen FileHierarchy
compAbstractVariadic vc = do cl <- body
                             return $ Class (show vc) cl
  where body = do co  <- askSt (flip codomainOf vc)
                  qto <- qualifiedSort co
                  return $ rClass (public <+> abstract)
                                  (pretty vc) (Just qto)
                                  Nothing empty 

-- | Given a non-variadic constructor @C@, generates a concrete class @C.java@.
compConstructor :: CtorId -> Gen FileHierarchy
compConstructor c = do mem  <- compMembersOfConstructor c
                       ctor <- compCtorOfConstructor c
                       get  <- compGettersOfConstructor c
                       set  <- compSettersOfConstructor c
                       tos  <- compToStringBuilder c
                       eqs  <- compEqualsConstructor c
                       let isc = compIsX c
                       let body = vcat [mem,ctor,get,set,tos,eqs,isc]
                       cls  <- wrap body
                       return $ Class (show c) cls
 where wrap b = do
         gen <- askSt (flip isGenerated c)                                     
         let rcls d = rClass public (pretty c) (Just d) Nothing b              
         case gen of Nothing -> do co  <- askSt (flip codomainOf c)            
                                   qco <- qualifiedSort co                     
                                   return $ rcls qco                           
                     Just bc -> do qbc <- qualifiedCtor bc                     
                                   return $ rcls qbc                        

-- | Helper fonction that iters over the fields of
-- a constructor and combines them.
iterOverFields 
  :: ((FieldId,SortId) -> Gen a) -- ^ the function to iter
  -> ([a] -> b)                  -- ^ the combinator
  -> CtorId                      -- ^ the constructor
  -> Gen b
iterOverFields f g c = do fis  <- askSt (flip fieldsOf c)
                          fis' <- mapM f fis
                          return $ g fis'

-- | Given a non-variadic constructor @C(x1:T1,..,xn:Tn)@,
-- generates 
--
-- > public String toStringBuilder(java.lang.StringBuilder buffer) {
-- >   buffer.append("C(");
-- >   x1.toStringBuilder(buffer);
-- >   buffer.append(",");
-- >   ...
-- >   buffer.append(",");
-- >   xn.toStringBuilder(buffer);
-- >   buffer.append(")");
-- > }
compToStringBuilder :: CtorId -> Gen Doc
compToStringBuilder c = do rcalls <- iterOverFields rcall id c
                           return $ rMethodDef 
                             public void (text "toStringBuilder")
                             [stringBuilder <+> text "buf"] (complete rcalls)
  where complete b  = rBody $ open:(intersperse apcomma b)++[close]
        bapp arg    = text "buf.append" <> parens arg
        apcomma     = bapp $ dquotes comma
        open        = bapp $ dquotes (pretty c <> lparen)
        close       = bapp $ dquotes rparen
        rcall (x,s) = return $
          if isBuiltin s then bapp (pretty x)
                         else rMethodCall (this <> dot <> pretty x)
                                          (text "toStringBuilder") 
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
        rcall (x,s) = let lhs = this <> dot <> pretty x
                          rhs = text "typed_o.get" <> pretty x <> text "()"
                      in return $ if isBuiltin s
                                    then lhs <+> text "==" <+> rhs
                                    else rMethodCall lhs (text "equals") [rhs]

-- | Given a constructor C, generates
--
-- > public boolean isC() { return true; }
compIsX :: CtorId -> Doc
compIsX c = let fun = text "is" <> pretty c
                b   = rBody [jreturn <+> jtrue]
            in rMethodDef public jboolean fun [] b 

-- | Given a non-variadic constructor @C(x1:T1,..,xn:Tn)@,
-- generates @m.types.T1 x1; ...; m.types.Tn xn;@
compMembersOfConstructor :: CtorId -> Gen Doc
compMembersOfConstructor = iterOverFields rdr rBody
  where rdr (f,s) = do qs <- qualifiedSort s
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
  do fis <- askSt (flip fieldsOf c)
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
  where getter (f,s) = do qs <- qualifiedSort s
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
  where setter (f,s) = do qs <- qualifiedSort s
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
              s      <- askSt (flip codomainOf c)
              fis    <- askSt (flip fieldsOf c)
              let pfis = map (pretty *** pretty) fis
              make   <- compMake (map fst pfis)
              return $ rOp (pretty s) (pretty c) pfis
                           (vcat [isfsym,slots,make])
  where compIsFsym     = do qc <- qualifiedCtor c
                            return $ rIsFsym qc
        compSlot (x,_) = return $ rGetSlot (pretty x)
        compMake xs    = do qc <- qualifiedCtor c
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
compOpList c = do co     <- askSt (flip codomainOf c)
                  dom    <- askSt (flip fieldOf c)
                  consc  <- qualifiedCtor (prependCons c)
                  emptyc <- qualifiedCtor (prependEmpty c)
                  return $ rOpList (pretty c) (pretty co) 
                                   (pretty dom) consc emptyc
