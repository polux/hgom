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
import Control.Monad.Reader

-- | Compiles a symbol table into OO Mapping
st2oomapping :: SymbolTable -> Config -> FileHierarchy
st2oomapping =  runGen compOOMapping


-- | Generates the @Mod.tom@ tom mapping file for module @Mod@ based
-- on OO mappings and the OOMapping interface class
compOOMapping :: Gen FileHierarchy

compOOMapping = do mn <- map toLower `liftM` askSt modName
                   pr <- askConf package
                   ctrs  <- askSt simpleConstructorsIds
                   srts  <- askSt definedSortsIds
                   tyts  <- mapM compTypeTerm srts
                   ops   <- mapM compOp ctrs
                   isig  <- compISignature ctrs
                   let mapping = Tom mn (vsep $ (tyts++ops))
                   return . wrap pr $ Package mn [mapping,isig]
                where 
                   -- wraps the package in the user-provided prefix hierarchy (-p option)
                   wrap Nothing  h = h
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
compTypeTerm s = do sh <- askConf sharing 
                    return $ rTypeterm (pretty s) (pretty s) sh

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
compOp c = do slots   <- compSlots 
              s       <- askSt (codomainOf c)
              fis     <- askSt (fieldsOf c)
              let pfis = map (pretty *** pretty) fis
              let make = compMake (map fst pfis)
              return $ rOp (pretty s) (pretty c) pfis
                           (vcat [isfsym,slots,make])
  where mapping = text "getSignature().getMapping_" <> pretty c <> text "()"
        isfsym  = text "is_fsym(t) {" <>  mapping <> text ".isInstanceOf($t) }"
        compSlot (i,s) = 
          return $ (text "get_slot(" <> (pretty s) <> text ",t) {" <> mapping <> text ".get" <> text (show i) <> text "($t) }")
        compMake as =
          text "make" <> args <+> (sbraces . parens) 
                (mapping <> text ".make" <> iargs)
          where gen   = parens . hcat . punctuate comma
                args  = gen as
                iargs = gen (map inline as)
        compSlots = do fis  <- askSt (fieldsOf c)
                       fis' <- mapM compSlot [(i,fst (fis!!i)) | i <- [0..length fis-1]]
                       return $ vcat fis'


-- | Given a list of constructors @Ci@, 
-- generates the ISignature of new oo mappings.
compISignature :: [CtorId] -> Gen FileHierarchy
compISignature s = do methDecls <- mapM compMDecl s
                      let code = rInterface public (text "ISignature") [] (rBody methDecls)
                      return $ Class "ISignature" code 

compMDecl :: CtorId -> Gen Doc
compMDecl c = do cfields <- askSt (fieldsOf c)
                 s       <- askSt (codomainOf c)
                 let arity = length cfields
                 let cfieldsSort = map (pretty . snd) cfields
                 let types  =  gen (pretty s:cfieldsSort) 
                 return $ text "tom.library.oomapping.Mapping"<> pretty arity <> types <> text " getMapping_" <> pretty c <> text "()"
              where gen   =  angles . hcat . punctuate comma
