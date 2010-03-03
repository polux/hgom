------------------------------------------------------------------
-- |
-- Module      : Gom.CodeGen.Mappings
-- Copyright   : (c) Paul Brauner 2009
--               (c) Emilie Balland 2009
--               (c) INRIA 2009
-- Licence     : GPL (see COPYING)
--
-- Maintainer  : paul.brauner@inria.fr
-- Stability   : provisional
-- Portability : non-portable (requires generalized newtype deriving)
--------------------------------------------------------------------

module Gom.CodeGen.Mappings (
  compTomFiles
) where

import Gom.Sig
import Gom.Config
import Gom.FileGen
import Gom.SymbolTable
import Gom.CodeGen.Common

import Text.PrettyPrint.Leijen
import Control.Arrow((***))

-- | Generates the @Mod.tom@ and @_Mod.tom@ tom mappings files for
-- module @Mod@
compTomFiles :: Gen [FileHierarchy]
compTomFiles = do mn    <- askSt modName
                  srts  <- askSt definedSortsIds
                  ctrs  <- askSt simpleConstructorsIds
                  vctrs <- askSt variadicConstructorsIds
                  incls <- compIncludes
                  tyts  <- mapM compTypeTerm srts
                  ops   <- mapM compOp ctrs
                  opls  <- mapM compOpList vctrs
                  sops <-  mapM compSOp ctrs
                  vcongr <- askConf congr
                  let mappings = incls:(tyts++ops++opls)
                  let slinclude = text "%include { sl.tom }"
                  return $ case vcongr of 
                     NoCongr -> [Tom mn (vsep mappings)]
                     SameFile  -> [Tom mn (vsep $ slinclude:mappings++sops)]
                     SeparateFile -> [Tom mn $ vsep mappings, Tom ("_"++mn) (vsep $ slinclude:sops)]

-- | Generates @%include { x.tom }@ for every imported sort @x@
compIncludes :: Gen Doc
compIncludes = do is <- askSt importedSorts
                  return $ vcat (map rdr is)
  where rdr s = text "%include" <+> 
                sbraces (builtinImport s)

-- | Given a sort @S@, generates
--
-- > %typeterm S {
-- >   implement { m.types.S }
-- >   is_sort(t) { ($t instanceof m.types.S) }
-- >   equals(t1,t2) { ($t1 == $t2) }
-- > }
--
-- generats @$t1.equals($t2)@ if @--noSharing@ has been
-- toggled
compTypeTerm :: SortId -> Gen Doc
compTypeTerm s = do qs <- qualifiedSort s
                    sh <- askConf sharing 
                    return $ rTypeterm (pretty s) qs sh

-- | Given a non-variadic constructor @C(x1:T1,..,xn:Tn)@
-- of codomain @Co@, generates
--
-- > %op Co C(x1:T1,..,xn:Tn) {
-- >   is_fsym(t) { ($t instanceof m.types.C) }
-- >   get_slot(x1,t) { $t.getx1() }
-- >   ...
-- >   get_slot(xn,t) { $t.getxn() }
-- >   make (t1,..,tn) { (m.types.co.c.make($t1,..,$tn)) }
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
-- >   make_empty() { foo.types.co.EmptyVC.make() }
-- >   make_insert(e,l) { foo.types.co.ConsVC.make($e,$l) }
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

-- | Given a non-variadic constructor @C(x1:T1,..,xn:Tn)@
-- of codomain @Co@, generates
--
-- > %op Strategy _C(s1:Strategy,..,sn:Strategy) {
-- >   is_fsym(t) { $t m.strategy.co._C }
-- >   get_slot(s1,t) { (tom.library.sl.Strategy) $t.getChildAt(0) }
-- >   ...
-- >   get_slot(sn,t) { (tom.library.sl.Strategy) $t.getChildAt(n-1) }
-- >   make (t1,..,tn) { new m.strategy.co._C($t1,..,$tn) }
-- > }
compSOp :: CtorId -> Gen Doc
compSOp c = do sc <- compStratClass
               n  <- length `fmap` askSt (fieldsOf c)
               return $ rOp (text "Strategy") (_u $ pretty c) (makeTypedArgs n)
                           (vcat [rIsFsym sc,slots n,makeMake n sc])
  where makeSlot i = text "get_slot(x" <> int i <> text ",t) { (tom.library.sl.Strategy) $t.getChildAt(" <> int (i-1) <> text ") }"
        makeMake n sc =  text "make" <> makeArgs n "s" <> text "{ new " <> sc <> makeArgs n "$s" <> text " }"
        makeArgs arity name = gen [text name <> int i | i <- [1..arity]]
        makeTypedArgs n = [ (text "s" <> int i, text "Strategy") | i <- [1..n] ]
        compStratClass = do m <- packagePrefix
                            co <- lowerId `fmap` askSt (codomainOf c)
                            return $ m <> dot <> pretty co <> dot <> text "strategy" <> dot <> _u (pretty c) 
        slots n = vcat [makeSlot i | i <- [1..n]] 
        gen   = parens . hcat . punctuate comma
