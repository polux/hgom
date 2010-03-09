------------------------------------------------------------------
-- |
-- Module      : HGom.CodeGen.Mappings
-- Copyright   : (c) Paul Brauner 2009
--               (c) Emilie Balland 2009
--               (c) INRIA 2009
-- Licence     : GPL (see COPYING)
--
-- Maintainer  : paul.brauner@inria.fr
-- Stability   : provisional
-- Portability : non-portable (requires generalized newtype deriving)
--------------------------------------------------------------------

module HGom.CodeGen.Mappings (
  compTomFiles
) where

import Common.Sig
import Common.FileGen
import Common.SymbolTable
import Common.CodeGen
import HGom.Config
import HGom.CodeGen.HGenMonad

import Text.PrettyPrint.Leijen
import Control.Arrow((***))

-- | Generates the @Mod.tom@ and @_Mod.tom@ tom mappings files for
-- module @Mod@
compTomFiles :: HGen [FileHierarchy]
compTomFiles = do mn    <- askSt modName
                  srts  <- askSt definedSortsIds
                  ctrs  <- askSt simpleConstructorsIds
                  vctrs <- askSt variadicConstructorsIds
                  incls <- compIncludes
                  tyts  <- mapM compTypeTerm srts
                  ops   <- mapM compOp ctrs
                  opls  <- mapM compOpList vctrs
                  let always = incls:(tyts++ops++opls)
                  cops  <-  mapM (compSOp empty) ctrs
                  mops  <-  mapM (compSOp $ text "Make") ctrs
                  iops  <-  mapM compIsOp ctrs
                  let wops = map (rWhenOp . pretty) ctrs
                      sops = cops++mops++iops++wops
                  vcongr <- askConf congr
                  return $ case vcongr of 
                     NoCongr  -> [Tom mn (vsep always)]
                     SameFile -> [Tom mn (vsep $ includeSl:always++sops)]
                     SeparateFile -> [Tom mn $ vsep always, 
                                      Tom ('_':mn) (vsep $ includeSl:sops)]

-- | Generates @%include { x.tom }@ for every imported sort @x@
compIncludes :: HGen Doc
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
compTypeTerm :: SortId -> HGen Doc
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
compOp :: CtorId -> HGen Doc
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
compOpList :: CtorId -> HGen Doc
compOpList c = do co     <- askSt (codomainOf c)
                  dom    <- askSt (fieldOf c)
                  consc  <- qualifiedCtor (prependCons c)
                  emptyc <- qualifiedCtor (prependEmpty c)
                  return $ rOpList (pretty c) (pretty co) 
                                   (pretty dom) consc emptyc

-- | Given a non-variadic constructor @C(x1:T1,..,xn:Tn)@
-- of codomain @Co@, @compSOp prefix C@ generates
--
-- > %op Strategy prefix_C(s1:Strategy,..,sn:Strategy) {
-- >   is_fsym(t) { $t m.strategy.co.prefix_C }
-- >   get_slot(s1,t) { (tom.library.sl.Strategy) $t.getChildAt(0) }
-- >   ...
-- >   get_slot(sn,t) { (tom.library.sl.Strategy) $t.getChildAt(n-1) }
-- >   make (t1,..,tn) { new m.strategy.co.prefix_C($t1,..,$tn) }
-- > }
compSOp :: Doc -> CtorId -> HGen Doc
compSOp pr c = do sc <- compStratClass
                  n  <- length `fmap` askSt (fieldsOf c)
                  return $ rOp strat cln (rArgs n)
                               (vcat [rIsFsym sc, slots n, rMakeStrat n sc])
  where arg = text "s"
        cln = pr <> _u (pretty c)
        strat  = text "Strategy"
        rArgs n = [(arg <> int i, strat) | i <- [0..n-1]]
        slots n = vcat $ map (rGetSlotStrat arg) [0..n-1]
        compStratClass = do 
          spr <- qualifiedStratPrefix =<< askSt (codomainOf c)
          return $ spr <> dot <> cln

-- | Given a non-variadic constructor @C@, generates
--
-- >  %op Strategy Is_C() {
-- >    make() { new m.strategy.co.Is_C()}
-- >  }
compIsOp :: CtorId -> HGen Doc
compIsOp c = do 
  sc <- compStratClass
  return $ rOp (text "Strategy") cln [] (rMakeStrat 0 sc)
  where cln = text "Is_" <> pretty c
        compStratClass = do 
          spr <- qualifiedStratPrefix =<< askSt (codomainOf c)
          return $ spr <> dot <> cln
