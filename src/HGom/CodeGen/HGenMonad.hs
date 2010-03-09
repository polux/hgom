-------------------------------------------------------------------
-- |
-- Module      : HGom.CodeGen.HGenMonad
-- Copyright   : (c) Paul Brauner 2009
--               (c) Emilie Balland 2009
--               (c) INRIA 2009
-- Licence     : GPL (see COPYING)
--
-- Maintainer  : paul.brauner@inria.fr
-- Stability   : provisional
-- Portability : non-portable (requires generalized newtype deriving)
--
-- Gen monad specialisation for hgom.
-------------------------------------------------------------------- 

module HGom.CodeGen.HGenMonad (HGen) where

import HGom.Config
import Common.CodeGen

-- | Gen Monad tuned for HGom
type HGen = Gen Config


