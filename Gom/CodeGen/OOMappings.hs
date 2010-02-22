------------------------------------------------------------------
-- |
-- Module      : Gom.CodeGen.OOMappings
-- Copyright   : (c) Paul Brauner 2009
--               (c) Emilie Balland 2009
--               (c) INRIA 2009
-- Licence     : GPL (see COPYING)
--
-- Maintainer  : emilie.balland@inria.fr
-- Stability   : provisional
-- Portability : non-portable (requires generalized newtype deriving)
--------------------------------------------------------------------

module Gom.CodeGen.OOMappings (
  compOOMapping
) where

import Gom.Sig
import Gom.Config
import Gom.FileGen
import Gom.SymbolTable
import Gom.CodeGen.Common

import Text.PrettyPrint.Leijen
import Control.Arrow((***))

-- | Generates the @Mod.tom@ tom mapping file for module @Mod@ based
-- on OO mappings
compOOMapping :: Gen FileHierarchy

compOOMapping = do mn    <- askSt modName
                   ctrs  <- askSt simpleConstructorsIds
                   srts  <- askSt definedSortsIds
                   isig  <- compISignature ctrs
                   tyts  <- mapM compTypeTerm srts
                   ops   <- mapM compOp ctrs
                   return $ Tom mn (vsep $ (isig:(tyts++ops)))

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
compTypeTerm s = do qs <- qualifiedSort s
                    sh <- askConf sharing 
                    return $ rTypeterm (pretty s) qs sh

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
compOp c = do slots   <- iterOverFields compSlot vcat c
              s       <- askSt (codomainOf c)
              fis     <- askSt (fieldsOf c)
              let pfis = map (pretty *** pretty) fis
              let make = compMake (map fst pfis)
              return $ rOp (pretty s) (pretty c) pfis
                           (vcat [isfsym,slots,make])
  where mapping = text "getSignature().getMapping_" <> pretty c <> text "()"
        isfsym  = mapping <> text ".isInstanceOf($t)"
        compIsFsym   = do qc <- qualifiedCtor c
                          return $ rIsFsym qc
        compSlot x _ = 
          return $ (text "get_slot(" <> (pretty x) <> text ",t) {" <> mapping <> text ".get" <> (pretty x) <> text "() }")
        compMake as =
          text "make" <> args <+> (sbraces . parens) 
                (mapping <> text ".make" <> iargs)
          where gen   = parens . hcat . punctuate comma
                args  = gen as
                iargs = gen (map inline as)

-- | Given a list of sorts @S@ of constructors @Ci@, 
-- generates the Tom mapping of new oo mappings.
compISignature :: [CtorId] -> Gen Doc
compISignature s = return $ text "// ISignature needs to be generated"
