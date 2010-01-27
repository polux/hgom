-------------------------------------------------------------------
-- |
-- Module      : Gom.Pretty
-- Copyright   : (c) Paul Brauner 2009
--               (c) Emilie Balland 2009
--               (c) INRIA 2009
-- Licence     : GPL (see COPYING)
--
-- Maintainer  : paul.brauner@inria.fr
-- Stability   : provisional
-- Portability : non-portable (requires GeneralizedNewtypeDeriving)
--
-- This module exports nothing but defines instances of
-- 'Text.PrettyPrint.Leijen.Pretty' for 'Ctor', 'SortDef' and 'Module'.
--------------------------------------------------------------------



module Gom.Pretty () where

import Gom.Sig
import Text.PrettyPrint.Leijen

lb ::  Doc
lb = linebreak

instance Pretty Ctor where
  pretty (Simple n fs) = text (show n) <> parens (hsep $ punctuate (char ',') prettyfs)
     where prettyfs = map (\(x,t) -> text (show x) <> char ':' <> text (show t)) fs
  pretty (Variadic n t) = text (show n) <> parens (text (show t) <> char '*')

instance Pretty SortDef where
  pretty (SortDef n cs) = text (show n) <+> align (char '=' <+> prettycs)
    where prettycs = fillSep $ withPipes (map pretty cs)  
          withPipes (x:y:xs) = x:withPipes ((char '|' <+> y):xs)
          withPipes l        = l

instance Pretty Module where
  pretty (Module n i d) = (text "module" <+> text n) 
                          <$$> (text "imports" <+> hsep (map (text . show) i))
                          <$$> text "abstract syntax" <> lb
                          <$$> vcat (punctuate lb (map pretty d))

