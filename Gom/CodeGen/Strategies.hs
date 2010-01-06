module Gom.CodeGen.Strategies where

import Gom.Sig
import Gom.SymbolTable
import Gom.Java
import Gom.CodeGen.Common

import Text.PrettyPrint.Leijen
import Control.Monad.Reader

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
     return $ Class ('_':show c) (wrap body)
  where wrap = rClass public (pretty c) (Just jSCombinator) []
-- | TODO
compVisit :: CtorId -> Gen Doc
compVisit c = return $ rMethodDef private empty (pretty c) [] empty

--   public <T> T visitLight(T any, tom.library.sl.Introspector introspector) throws tom.library.sl.VisitFailure {
--    if(any instanceof gom.term.types.term.f) {
--      T result = any;
--      Object[] childs = null;
--      for (int i = 0, nbi = 0; i < 1; i++) {
--          Object oldChild = introspector.getChildAt(any,nbi);
--          Object newChild = args[i].visitLight(oldChild,introspector);
--          if(childs != null) {
--            childs[nbi] = newChild;
--          } else if(newChild != oldChild) {
--            // allocate the array, and fill it
--            childs = introspector.getChildren(any);
--            childs[nbi] = newChild;
--          }
--          nbi++;
--      }
--      if(childs!=null) {
--        result = introspector.setChildren(any,childs);
--      }
--      return result;
--    } else {
--      throw new tom.library.sl.VisitFailure(msg);
--    }
--  }


compVisitLight :: CtorId -> Gen Doc
compVisitLight c = return $ rMethodDef private empty (pretty c) [jVisitable <+> text "o"] body
                     where body = rBody [ifthenelse]
                           cdoc = pretty c
                           ifthenelse =  rIfThenElse cond successBranch (throw <+> (rConstructorCall jVisitFailure []) <> semi) 
                           cond = text "o" <+> instanceof <+> cdoc
                           successBranch = rMethodCall (rConstructorCall (text "All") []) jVisitLight [(text "o")]
                           initResult = typeT <+> result <+> equals <+> anyVar  
                           initChilds = typeT <+> childs <+> equals <+> new <+> jObject <+> text("[]") 
                           typeT = (text "T")
                           result = (text "result")
                           anyVar = (text "any")
                           childs = (text "childs")
                           newChild = (text "newChild")
                           oldChild = (text "oldChild")
-- | Given a Constructor @C(x1:T1, ..., xn:Tn)@, generates
--
-- > private C(Strategy s_x1, ..., Strategy s_xn) {
-- >   initSubterm(s_x1,...,s_xn);
-- > }
compCongruenceConstructor :: CtorId -> Gen Doc
compCongruenceConstructor c = do
  fis <-  askSt (fieldsOf c)
  let typedArgs = prettyArgs fis (text "Strategy ")
  let args =  prettyArgs fis empty
  return $ rMethodDef private empty (pretty c) typedArgs 
                      (rBody [rMethodCall this (text "initSubterm") args])
  where prettyArgs fis prefix = map (pre . pretty . fst) fis
          where pre x = prefix <> text "s_" <> x


