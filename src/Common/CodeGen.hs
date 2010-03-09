-------------------------------------------------------------------
-- |
-- Module      : Common.CodeGen
-- Copyright   : (c) Paul Brauner 2009
--               (c) Emilie Balland 2009
--               (c) INRIA 2009
-- Licence     : GPL (see COPYING)
--
-- Maintainer  : paul.brauner@inria.fr
-- Stability   : provisional
-- Portability : non-portable (requires generalized newtype deriving)
--
-- Handle user args.
--------------------------------------------------------------------

module Common.CodeGen (
  module Common.CodeGen.Builtins,
  module Common.CodeGen.Constants,
  module Common.CodeGen.GenMonad,
  module Common.CodeGen.Helpers
) where

import Common.CodeGen.Builtins
import Common.CodeGen.Constants
import Common.CodeGen.GenMonad
import Common.CodeGen.Helpers

