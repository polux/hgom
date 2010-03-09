-------------------------------------------------------------------
-- |
-- Module      : OOMappings.CodeGen.OGenMonad
-- Copyright   : (c) Paul Brauner 2009
--               (c) Emilie Balland 2009
--               (c) INRIA 2009
-- Licence     : GPL (see COPYING)
--
-- Maintainer  : emilie.balland@inria.fr
-- Stability   : provisional
-- Portability : non-portable (requires generalized newtype deriving)
--
-- Gen monad specialisation for oomappings.
-------------------------------------------------------------------- 

module OOMappings.CodeGen.OGenMonad (OGen) where

import OOMappings.Config
import Common.CodeGen.GenMonad

-- | OGen Monad tuned for HGom
type OGen = Gen Config
