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

module Gom.CodeGen.Strategies where

import Gom.Sig
import Gom.FileGen
import Gom.SymbolTable
import Gom.CodeGen.Common

import Text.PrettyPrint.Leijen
import Control.Monad.Reader

-- | Given a sort @S@ of constructors @Ci@, 
-- generates the package @s@ containing the
-- @_Ci@ congruence classes.
compStrategy :: SortId -> Gen FileHierarchy
compStrategy s = do ctrs  <- askSt (sCtorsOf s)
                    cs <- mapM compCongruence ctrs
                    return $ Package (show $ lowerSortId s) cs 

-- | Given a non-variadic constructor @C@, 
-- generates a congruence strategy class @_C.java@.
compCongruence :: CtorId -> Gen FileHierarchy
compCongruence c = 
  do body <- vcat `liftM` sequence [compCongruenceConstructor c,
                                    compVisit c, compVisitLight c]
     return $ Class classname (wrap body)
  where wrap = rClass public (text classname) (Just jSCombinator) []
        classname = '_':show c

-- | Given a non-variadic constructor @C@, generates
-- the method @public int visit(Introspector introspector) { ... }@
-- for class @_C@.
compVisit :: CtorId -> Gen Doc
compVisit c = return $ rMethodDef public (text "int") (text "visit") 
                                  [jIntrospector <+> text "introspector"] body
  where body = vcat $ map text 
          ["environment.setIntrospector(introspector);",
           "Object any = environment.getSubject();",
           "if (any instanceof " ++ show c ++ ") {",
           "  int childCount = introspector.getChildCount(any);",
           "  Object[] childs = null;",
           "  for(int i = 0; i < childCount; i++) {",
           "    Object oldChild = introspector.getChildAt(any,i);",
           "    environment.down(i+1);",
           "    int status = arguments[ARG].visit(introspector);",
           "    if(status != Environment.SUCCESS) {",
           "      environment.upLocal();",
           "      return status;",
           "    }",
           "    Object newChild = environment.getSubject();",
           "    if(childs != null) {",
           "      childs[i] = newChild;",
           "    } else if(newChild != oldChild) {",
           "      // allocate the array, and fill it",
           "      childs = introspector.getChildren(any);",
           "      childs[i] = newChild;",
           "    } ",
           "    environment.upLocal();",
           "  }",
           "  if(childs!=null) {",
           "    environment.setSubject(introspector.setChildren(any,childs));",
           "  }",
           "  return Environment.SUCCESS;",
           "} else {",
           "  return Environment.FAILURE;",
           "}"]


-- | Given a non-variadic constructor @C@, generates
-- the method @public int visitLight(Introspector introspector) { ... }@
-- for class @_C@.
compVisitLight :: CtorId -> Gen Doc
compVisitLight c = return $ 
  rMethodDef public typeGenericT (text "visitLight") 
             [typeT <+> text "any", jIntrospector <+> text "introspector"] body
  where typeT = text "T"
        typeGenericT = text "<T> T"
        body = vcat $ map text 
          ["if(any instanceof " ++ show c ++ ") {",
          "   T result = any;",
          "   Object[] childs = null;",
          "   for (int i = 0, nbi = 0; i < 1; i++) {",
          "       Object oldChild = introspector.getChildAt(any,nbi);",
          "       Object newChild = args[i].visitLight(oldChild,introspector);",
          "       if(childs != null) {",
          "         childs[nbi] = newChild;",
          "       } else if(newChild != oldChild) {",
          "         // allocate the array, and fill it",
          "         childs = introspector.getChildren(any);",
          "         childs[nbi] = newChild;",
          "       }",
          "       nbi++;",
          "   }",
          "   if(childs!=null) {",
          "     result = introspector.setChildren(any,childs);",
          "   }",
          "   return result;",
          " } else {",
          "   throw new tom.library.sl.VisitFailure(msg);",
          " }"]
        
-- | Given a non-variadic constructor @C@, generates
-- the constructor of @_C@.
compCongruenceConstructor :: CtorId -> Gen Doc
compCongruenceConstructor c = do
  fis <- askSt (fieldsOf c)
  let typedArgs = prettyArgs fis (text "Strategy ")
      args      = prettyArgs fis empty
  return $ rMethodDef public empty (text "_" <> pretty c) typedArgs 
                      (rBody [rMethodCall this (text "initSubterm") args])
  where prettyArgs fis prefix = map (pre . pretty . fst) fis
          where pre x = prefix <> text "s_" <> x
