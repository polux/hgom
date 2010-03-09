------------------------------------------------------------------
-- |
-- Module      : HGom.CodeGen.Strategies
-- Copyright   : (c) Paul Brauner 2009
--               (c) Emilie Balland 2009
--               (c) INRIA 2009
-- Licence     : GPL (see COPYING)
--
-- Maintainer  : paul.brauner@inria.fr
-- Stability   : provisional
-- Portability : non-portable (requires generalized newtype deriving)
--------------------------------------------------------------------

module HGom.CodeGen.Strategies (
  compStrategy
) where

import Common.Sig
import Common.FileGen
import Common.SymbolTable
import Common.CodeGen
import HGom.CodeGen.Common

import Text.PrettyPrint.Leijen

-- | Given a sort @S@ of constructors @Ci@, 
-- generates the package @s@ containing the
-- @_Ci@ congruence classes.
compStrategy :: SortId -> HGen FileHierarchy
compStrategy s = do ctrs  <- askSt (sCtorsOf s)
                    cs <- mapM compCongruence ctrs
                    ms <- mapM compMake ctrs
                    is <- mapM compIs ctrs
                    return $ Package (show $ lowerId s) (cs++ms++is) 

-- | Given a non-variadic constructor @C@, 
-- generates a congruence strategy class @_C@.
compCongruence :: CtorId -> HGen FileHierarchy
compCongruence c = 
  do body <- vcat `fmap` sequence 
               [compStratConstructor (text "_") c,
                compVisitCongr c, 
                compVisitLightCongr c]
     return $ Class classname (wrap body)
  where wrap = rClass public (text classname) (Just jSCombinator) []
        classname = '_':show c

-- | Given a non-variadic constructor @C@,
-- generates the creation strategy class @Make_C@.
compMake :: CtorId -> HGen FileHierarchy
compMake c = do
  body <- vcat `fmap` sequence 
            [compStratConstructor (text "Make_") c,
             compVisitMake c, 
             compVisitLightMake c]
  return $ Class classname (wrap body)
  where wrap = rClass public (text classname) (Just jSCombinator) []
        classname = "Make_" ++ show c

-- | Given a non-variadic constructor @C@,
-- generates the test strategy class @Is_C@.
compIs :: CtorId -> HGen FileHierarchy
compIs c = do
  qc <- qualifiedCtor c
  return $ Class classname (wrap $ body qc)
  where wrap = rClass public (text classname) (Just jSCombinator) []
        classname = "Is_" ++ show c
        body qc = vcat $ map text
          ["private static final String msg = \"Not a " ++ show c ++"\";",
           "public Is_" ++ show c ++ "() {",
           "  initSubterm();",
           "}",
           "public <T> T visitLight(T __any, tom.library.sl.Introspector __i)",
           "  throws tom.library.sl.VisitFailure {",
           "  if(__any instanceof " ++ show qc ++ "){",
           "    return __any;",
           "  } else {",
           "    throw new tom.library.sl.VisitFailure(msg);",
           "  }",
           "}",
           "public int visit(tom.library.sl.Introspector __i) {",
           "  Object __any = environment.getSubject();",
           "  if(__any instanceof " ++ show qc ++ ") {",
           "    return tom.library.sl.Environment.SUCCESS;",
           "  } else {",
           "    return tom.library.sl.Environment.FAILURE;",
           "  }",
           "}"]

-- | Given a non-variadic constructor @C@, generates
-- the method @public int visit(Introspector introspector) { ... }@
-- for class @_C@.
compVisitCongr :: CtorId -> HGen Doc
compVisitCongr c = body `fmap` qualifiedCtor c
  where body qc = vcat $ map text 
          ["public int visit(tom.library.sl.Introspector __i) {",
           "  environment.setIntrospector(__i);",
           "  Object __any = environment.getSubject();",
           "  if (__any instanceof " ++ show qc ++ ") {",
           "    int childCount = __i.getChildCount(__any);",
           "    Object[] childs = null;",
           "    for(int i = 0; i < childCount; i++) {",
           "      Object oldChild = __i.getChildAt(__any,i);",
           "      environment.down(i+1);",
           "      int status = arguments[i].visit(__i);",
           "      if(status != tom.library.sl.Environment.SUCCESS) {",
           "        environment.upLocal();",
           "        return status;",
           "      }",
           "      Object newChild = environment.getSubject();",
           "      if(childs != null) {",
           "        childs[i] = newChild;",
           "      } else if(newChild != oldChild) {",
           "        // allocate the array, and fill it",
           "        childs = __i.getChildren(__any);",
           "        childs[i] = newChild;",
           "      } ",
           "      environment.upLocal();",
           "    }",
           "    if(childs!=null) {",
           "      environment.setSubject",
           "        (__i.setChildren(__any,childs));",
           "    }",
           "    return tom.library.sl.Environment.SUCCESS;",
           "  } else {",
           "    return tom.library.sl.Environment.FAILURE;",
           "  }",
           "}"]

-- | Given a non-variadic constructor @C@, generates
-- the method @public int visitLight(Introspector introspector) { ... }@
-- for class @_C@.
compVisitLightCongr :: CtorId -> HGen Doc
compVisitLightCongr c = do 
  n <- length `fmap` askSt (fieldsOf c)
  qc <- qualifiedCtor c
  return $ body qc n
  where body qc n = vcat $ map text 
          ["public <T> T visitLight(T __any,",
           "  tom.library.sl.Introspector __i)", 
           "  throws tom.library.sl.VisitFailure {",
           "  if(__any instanceof " ++ show qc ++ ") {",
           "    T result = __any;",
           "    Object[] childs = null;",
           "    for (int i = 0, nbi = 0; i < " ++ show n ++"; i++) {",
           "        Object oldChild = __i.getChildAt(__any,nbi);",
           "        Object newChild =",
           "           arguments[i].visitLight(oldChild,__i);",
           "        if(childs != null) {",
           "          childs[nbi] = newChild;",
           "        } else if(newChild != oldChild) {",
           "          // allocate the array, and fill it",
           "          childs = __i.getChildren(__any);",
           "          childs[nbi] = newChild;",
           "        }",
           "        nbi++;",
           "    }",
           "    if(childs!=null) {",
           "      result = __i.setChildren(__any,childs);",
           "    }",
           "    return result;",
           "  } else {",
           "    throw new tom.library.sl.VisitFailure();",
           "  }",
           "}"]

-- | Given a non-variadic constructor @C(_:T0,...,_:Tn)@, generates
--
-- > public int visit(tom.library.Introspector i) {
-- >   getChildAt(0).visit(i);
-- >   if (! (getEnvironment().getSubject() instanceof m.types.T0)) {
-- >     return tom.library.sl.Environment.FAILURE;
-- >   }
-- >   foo.types.T0 new_t0 = (foo.types.T0) getEnvironment().getSubject();
-- >
-- >   ...
-- >
-- >   getChildAt(n).visit(i);
-- >   if (! (getEnvironment().getSubject() instanceof m.types.Tn)) {
-- >     return tom.library.sl.Environment.FAILURE;
-- >   }
-- >   foo.types.Tn new_tn = (foo.types.Tn) getEnvironment().getSubject();
-- > 
-- >   getEnvironment().setSubject(m.types.co.C.make(new_t0, ..., new_tn));
-- >   return tom.library.sl.Environment.SUCCESS;
-- > }
compVisitMake :: CtorId -> HGen Doc
compVisitMake c = do
  qc  <- qualifiedCtor c
  ss  <- map snd `fmap` askSt (fieldsOf c)
  qss <- mapM myQualifiedSort ss
  let n = length qss
  return . method . vcat $ 
    (zipWith block qss [0..]) ++ [setSub qc n, lastLine]
  where myQualifiedSort s | isBuiltin s = return $ qualifiedBuiltin s
                          | otherwise = qualifiedSort s
        -- getChildAt(n) [...] = (foo.types.Tn) getEnvironment().getSubject()
        block qs i = vcat [getChild, testSort, assignTi]
          where getChild = cast (text "getChildAt" <> parens (int i))
                           <> text ".visit(__i);"
                testSort = rIfThen cond failure
                assignTi = qs <+> text "new_t" <> int i <+> equals <+> 
                           parens qs <+> text "getEnvironment().getSubject();"
                cond = text "!(getEnvironment().getSubject() instanceof "
                       <> qs <> text ")"
                failure = text "return tom.library.sl.Environment.FAILURE;"
                cast x = parens (parens jStrategy <+> x)
        -- getEnvironment().setSubject(m.types.co.C.make(new_t0, ..., new_tn))
        setSub qc n = rMethodCall (text "getEnvironment()") (text "setSubject")
                        [rMethodCall qc (text "make") ts] <> semi
          where ts = [text "new_t" <> int i | i <- [0..n-1]]
        lastLine = text "return tom.library.sl.Environment.SUCCESS;"
        method body = text "public int visit(tom.library.sl.Introspector __i)"
                      <+> ibraces body

-- | Given a non-variadic constructor @C(_:T0,...,_:Tn)@, generates
--
-- > public <T> T visitLight(T any, tom.library.Introspector i) 
-- >   throws tom.library.sl.VisitFailure {
-- >   Object tmp_t0 = ((tom.library.sl.Strategy) getChildAt(0)).visit(any,i);
-- >   if (!(tm_t0 instanceof m.types.T0)) {
-- >     throw new tom.library.sl.VisitFailure();
-- >   }
-- >   foo.types.T0 new_t0 = (foo.types.T0) tmp_t0;
-- >
-- >   ...
-- >
-- >   Object tmp_tn = ((tom.library.sl.Strategy) getChildAt(n)).visit(any,i);
-- >   if (!(tm_tn instanceof m.types.Tn)) {
-- >     throw new tom.library.sl.VisitFailure();
-- >   }
-- >   foo.types.Tn new_tn = (foo.types.Tn) tmp_tn;
-- > 
-- >   return (T) m.types.co.C.make(new_t0, ..., new_tn);
-- > }
compVisitLightMake :: CtorId -> HGen Doc
compVisitLightMake c = do
  qc  <- qualifiedCtor c
  ss  <- map snd `fmap` askSt (fieldsOf c)
  qss <- mapM myQualifiedSort ss
  let n = length qss
  return . method . vcat $ 
    (zipWith block qss [0..]) ++ [lastLine qc n]
  where myQualifiedSort s | isBuiltin s = return $ qualifiedBuiltin s
                          | otherwise = qualifiedSort s
        -- Object tmp_tn [...] = (foo.types.Tn) tmp_tn
        block qs i = vcat [visitChild, testSort, assignTi]
          where visitChild = text "Object" <+> tmp_i <+> equals <+>
                             cast (text "getChildAt" <> parens (int i))
                             <> text ".visit(__any,__i);"
                tmp_i    = text "__tmp_" <> int i
                testSort = rIfThen cond failure
                assignTi = qs <+> text "__new_t" <> int i <+> equals
                           <+> parens qs <+> tmp_i <> semi
                cond = text "!(" <> tmp_i <+> instanceof <+> qs <> text ")"
                failure = text "throw new tom.library.sl.VisitFailure();"
                cast x = parens (parens jStrategy <+> x)
        -- return (T) m.types.co.C.make(new_t0, ..., new_tn);
        lastLine qc n = text "return (T)" <+> 
                        rMethodCall qc (text "make") ts <> semi
          where ts = [text "__new_t" <> int i | i <- [0..n-1]]
        method body = (vcat $ map text 
                         ["public <T> T visitLight(T __any,",
                          "  tom.library.sl.Introspector __i)",
                          "  throws tom.library.sl.VisitFailure"]) 
                      <+> ibraces body
 
-- | Given a non-variadic constructor @C(x1,...,xn)@ 
-- and a prefix @prefix@, generates
--
-- >   public prefixf(tom.library.sl.Strategy s_x1, 
-- >                  ...,
-- >                  tom.library.sl.Strategy s_xn) {
-- >    this.initSubterm(
-- >      new tom.library.sl.Strategy[] { s_x1, ..., s_xn });
-- >  }
compStratConstructor :: Doc -> CtorId -> HGen Doc
compStratConstructor pr c = do
  fs <- map convert `fmap` askSt (fieldsOf c)
  return $ rMethodDef public empty (pr <> pretty c)
                      (map (jStrategy <+>) fs) (rBody [body fs])
  where convert = (text "__s_" <>) . pretty . fst
        body fs = rMethodCall this (text "initSubterm") [array]
          where array   = new <+> jStrategyArray <+> sbraces content 
                content = align $ sep (punctuate comma fs)
