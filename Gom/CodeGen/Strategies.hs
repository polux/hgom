------------------------------------------------------------------
-- |
-- Module      : Gom.CodeGen.Strategies
-- Copyright   : (c) Paul Brauner 2009
--               (c) Emilie Balland 2009
--               (c) INRIA 2009
-- Licence     : GPL (see COPYING)
--
-- Maintainer  : paul.brauner@inria.fr
-- Stability   : provisional
-- Portability : non-portable (requires generalized newtype deriving)
--------------------------------------------------------------------

module Gom.CodeGen.Strategies (
  compStrategy
) where

import Gom.Sig
import Gom.FileGen
import Gom.SymbolTable
import Gom.CodeGen.Common

import Text.PrettyPrint.Leijen

-- | Given a sort @S@ of constructors @Ci@, 
-- generates the package @s@ containing the
-- @_Ci@ congruence classes.
compStrategy :: SortId -> Gen FileHierarchy
compStrategy s = do ctrs  <- askSt (sCtorsOf s)
                    cs <- mapM compCongruence ctrs
                    return $ Package (show $ lowerId s) cs 

-- | Given a non-variadic constructor @C@, 
-- generates a congruence strategy class @_C.java@.
compCongruence :: CtorId -> Gen FileHierarchy
compCongruence c = 
  do body <- vcat `fmap` sequence [compCongruenceConstructor c,
                                   compVisit c, compVisitLight c]
     return $ Class classname (wrap body)
  where wrap = rClass public (text classname) (Just jSCombinator) []
        classname = '_':show c

-- | Given a non-variadic constructor @C@, generates
-- the method @public int visit(Introspector introspector) { ... }@
-- for class @_C@.
compVisit :: CtorId -> Gen Doc
compVisit c = body `fmap` qualifiedCtor c
  where body qc = vcat $ map text 
          ["public int visit(tom.library.sl.Introspector introspector) {",
           "  environment.setIntrospector(introspector);",
           "  Object any = environment.getSubject();",
           "  if (any instanceof " ++ show qc ++ ") {",
           "    int childCount = introspector.getChildCount(any);",
           "    Object[] childs = null;",
           "    for(int i = 0; i < childCount; i++) {",
           "      Object oldChild = introspector.getChildAt(any,i);",
           "      environment.down(i+1);",
           "      int status = arguments[i].visit(introspector);",
           "      if(status != tom.library.sl.Environment.SUCCESS) {",
           "        environment.upLocal();",
           "        return status;",
           "      }",
           "      Object newChild = environment.getSubject();",
           "      if(childs != null) {",
           "        childs[i] = newChild;",
           "      } else if(newChild != oldChild) {",
           "        // allocate the array, and fill it",
           "        childs = introspector.getChildren(any);",
           "        childs[i] = newChild;",
           "      } ",
           "      environment.upLocal();",
           "    }",
           "    if(childs!=null) {",
           "      environment.setSubject",
           "        (introspector.setChildren(any,childs));",
           "    }",
           "    return tom.library.sl.Environment.SUCCESS;",
           "  } else {",
           "    return tom.library.sl.Environment.FAILURE;",
           "  }",
           "}"]

-- | Given a non-variadic constructor @C@, generates
-- the method @public int visitLight(Introspector introspector) { ... }@
-- for class @_C@.
compVisitLight :: CtorId -> Gen Doc
compVisitLight c = do n <- length `fmap` askSt (fieldsOf c)
                      qc <- qualifiedCtor c
                      return $ body qc n
  where body qc n = vcat $ map text 
          ["public <T> T visitLight(T any,",
           "  tom.library.sl.Introspector introspector)", 
           "  throws tom.library.sl.VisitFailure {",
           "  if(any instanceof " ++ show qc ++ ") {",
           "    T result = any;",
           "    Object[] childs = null;",
           "    for (int i = 0, nbi = 0; i <" ++ show n ++"; i++) {",
           "        Object oldChild = introspector.getChildAt(any,nbi);",
           "        Object newChild =",
           "           arguments[i].visitLight(oldChild,introspector);",
           "        if(childs != null) {",
           "          childs[nbi] = newChild;",
           "        } else if(newChild != oldChild) {",
           "          // allocate the array, and fill it",
           "          childs = introspector.getChildren(any);",
           "          childs[nbi] = newChild;",
           "        }",
           "        nbi++;",
           "    }",
           "    if(childs!=null) {",
           "      result = introspector.setChildren(any,childs);",
           "    }",
           "    return result;",
           "  } else {",
           "    throw new tom.library.sl.VisitFailure();",
           "  }",
           "}"]
        
-- | Given a non-variadic constructor @C@, generates
-- the constructor of @_C@.
compCongruenceConstructor :: CtorId -> Gen Doc
compCongruenceConstructor c = do
  fs <- map convert `fmap` askSt (fieldsOf c)
  return $ rMethodDef public empty (text "_" <> pretty c) 
                      (map (jStrategy <+>) fs) (rBody [body fs])
  where convert = (text "s" <>) . pretty . fst
        body fs = rMethodCall this (text "initSubterm") [array]
          where array   = new <+> jStrategyArray <+> sbraces content 
                content = align $ sep (punctuate comma fs)
