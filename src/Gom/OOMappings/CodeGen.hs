-------------------------------------------------------------------
-- |
-- Module      : Gom.CodeGen
-- Copyright   : (c) Paul Brauner 2009
--               (c) Emilie Balland 2009
--               (c) INRIA 2009
-- Licence     : GPL (see COPYING)
--
-- Maintainer  : emilie.balland@inria.fr
-- Stability   : provisional
-- Portability : non-portable (requires generalized newtype deriving)
--
-- Generation of a file hierarchy from a symbol table.
-------------------------------------------------------------------- 

module Gom.OOMappingGen (st2oomapping) where

import Gom.Sig
import Gom.SymbolTable
import Gom.Config
import Gom.FileGen
import Gom.CodeGen.Common

import Text.PrettyPrint.Leijen
import Control.Arrow((***))
import Data.Char(toLower)

-- | Compiles a symbol table into OO Mapping
st2oomapping :: SymbolTable -> Config -> FileHierarchy
st2oomapping =  runGen compOOMapping

-- | Generates the @Mod.tom@ tom mapping file for module @Mod@ based
-- on OO mappings and the OOMapping interface class
compOOMapping :: Gen FileHierarchy
compOOMapping = do m <- askSt modName
                   let mn = map toLower m 
                   pr <- askConf package
                   ctrs  <- askSt simpleConstructorsIds
                   vctrs <- askSt variadicConstructorsIds
                   srts  <- askSt definedSortsIds
                   tyts  <- mapM compTypeTerm srts
                   ops   <- mapM compOp ctrs
                   vops  <- mapM compVOp vctrs
                   isig  <- compISignature ctrs vctrs 
                   let mapping = Tom m . vsep $ tyts++ops++vops
                   return . wrap pr $ Package mn [mapping,isig]
  where wrap Nothing  h = h
        wrap (Just l) h = foldr w h l
        w p h = Package p [h]
                 
-- | Given a sort @S@, generates
--
-- > %typeterm S {
-- >   implement { S }
-- >   is_sort(t) { ($t instanceof S) }
-- >   equals(t1,t2) { ($t1 == $t2) }
-- > }
--
-- generats @$t1.equals($t2)@ if @--noSharing@ has been
-- toggled
compTypeTerm :: SortId -> Gen Doc
compTypeTerm s = do 
  cs <- askSt (concreteTypeOf s)
  return $ rTypeterm (pretty s) (text (getClassName cs)) False 

-- | Given a non-variadic constructor @C(x1:T1,..,xn:Tn)@
-- of codomain @Co@, generates
--
-- > %op Co C(x1:T1,..,xn:Tn) {
-- >   is_fsym(t) { (getSignature().getMapping_C().isInstanceOf($t)) }
-- >   get_slot(x1,t) { getSignature().getMapping_f().get1($t) }
-- >   ...
-- >   get_slot(xn,t) { getSignature().getMapping_f().getn($t) }
-- >   make (t1,..,tn) { (getSignature().getMapping_f().make($s1, $s2)) }
-- > }
compOp :: CtorId -> Gen Doc
compOp c = do pfields <- map (pretty *** pretty) `fmap` askSt (fieldsOf c)
              co <- askSt (codomainOf c)
              let pfs = map fst pfields
              return $ rOp (pretty co) (pretty c) pfields
                           (vcat [isfsym, getters pfs, make pfs])
  where mapping = text "getSignature().getMapping_" <> pretty c <> text "()"
        isfsym  = text "is_fsym(t) {" <>  mapping <> text ".isInstanceOf($t) }"
        make fs = text "make" <> gen fs <+> (sbraces . parens $ body)
          where body = mapping <> text ".make" <> (gen $ map inline fs)
                gen  = parens . hcat . punctuate comma
        getters fs = vcat $ zipWith getter [0 :: Int ..] fs
        getter i s = text "get_slot(" <> pretty s <> 
                     text ",t) {" <> mapping <> text ".get" <> 
                     text (show i) <> text "($t) }"

-- | Given a variadic constructor @L(T*)@
-- of codomain @Co@, generates
-- %oplist Co L(T*) {
--  is_fsym(l)       { getSignature().getMapping_L().isInstanceOf($l) }
--  make_empty()     { getSignature().getMapping_L().makeEmpty() } 
--  make_insert(o,l) { getSignature().getMapping_L().makeInsert($o,$l) }
--  get_head(l)      { getSignature().getMapping_L().getHead($l) }
--  get_tail(l)      { getSignature().getMapping_L().getTail($l) }
--  is_empty(l)      { getSignature().getMapping_L().isEmpty($l) }
--}
compVOp :: CtorId -> Gen Doc
compVOp c = do 
  codom  <- askSt (codomainOf c)
  dom  <- askSt (fieldOf c)
  return $ text "%oplist" <+> pretty codom <+> pretty c <> 
           parens (pretty dom <> text "*") <+> ibraces (vcat $ map text 
             [" is_fsym(l)       { " ++ mapping ++ ".isInstanceOf($l) }",
              " make_empty()     { " ++ mapping ++ ".makeEmpty() }",
              " make_insert(o,l) { " ++ mapping ++ ".makeInsert($o,$l) }",
              " get_head(l)      { " ++ mapping ++ ".getHead($l) }",
              " get_tail(l)      { " ++ mapping ++ ".getTail($l) }",
              " is_empty(l)      { " ++ mapping ++ ".isEmpty($l) }"])
  where mapping = "getSignature().getMapping_" ++ show c ++ "()"


-- | Given a list of constructors @cs@ and a list of var constructors
-- @vcs@, generates the ISignature of new oo mappings.
compISignature :: [CtorId] -> [CtorId] -> Gen FileHierarchy
compISignature cs vcs = do 
  methDecls  <- mapM compMDecl cs
  vmethDecls <- mapM compMVDecl vcs
  let code = rInterface public (text "ISignature") 
                        (rBody (methDecls ++ vmethDecls))
  return $ Class "ISignature" code 

compMDecl :: CtorId -> Gen Doc
compMDecl c = do 
  cfields <- askSt (fieldsOf c)
  s       <- askSt (codomainOf c)
  cs      <- askSt (concreteTypeOf s) 
  let arity = length cfields
  classes <- mapM (prettyConcreteType . snd) cfields
  let types = gen (pretty cs:classes) 
  return $ text "tom.library.oomapping.Mapping" <> pretty arity <> 
           types <> text " getMapping_" <> pretty c <> text "()"
  where gen = angles . hcat . punctuate comma
        prettyConcreteType s = do cs <- askSt (concreteTypeOf s)
                                  return $ pretty cs

compMVDecl :: CtorId -> Gen Doc
compMVDecl c = do 
  dom    <- askSt (fieldOf c)
  codom  <- askSt (codomainOf c)
  cdom   <- askSt (concreteTypeOf dom) 
  ccodom <- askSt (concreteTypeOf codom) 
  return $ text "tom.library.oomapping.ListMapping<" <> pretty ccodom <> 
           text "," <> pretty cdom <> text ">" <+> text "getMapping_" <> 
           pretty c <> text "()"
