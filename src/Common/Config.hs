-------------------------------------------------------------------
-- |
-- Module      : Common.Config
-- Copyright   : (c) Paul Brauner 2009
--               (c) Emilie Balland 2009
--               (c) INRIA 2009
-- Licence     : GPL (see COPYING)
--
-- Maintainer  : paul.brauner@inria.fr
-- Stability   : provisional
-- Portability : non-portable (requires generalized newtype deriving)
--
-- Class defining the minimum requirements for a configuration.
--------------------------------------------------------------------

module Common.Config (IsConfig(..)) where

class IsConfig c where
  package :: c -> Maybe [String]
