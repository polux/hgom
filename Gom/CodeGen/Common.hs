------------------------------------------------------------------
-- |
-- Module      : Gom.CodeGen.Common
-- Copyright   : (c) Paul Brauner 2009
--               (c) Emilie Balland 2009
--               (c) INRIA 2009
-- Licence     : GPL (see COPYING)
--
-- Maintainer  : paul.brauner@inria.fr
-- Stability   : provisional
-- Portability : non-portable (requires generalized newtype deriving)
--
-- Exports common modules used by code generation functions.
--------------------------------------------------------------------

module Gom.CodeGen.Common (
  module Gom.CodeGen.Common.GenMonad,
  module Gom.CodeGen.Common.Helpers,
  module Gom.CodeGen.Common.Constants,
  module Gom.CodeGen.Common.Builtins
) where

import  Gom.CodeGen.Common.GenMonad
import  Gom.CodeGen.Common.Helpers
import  Gom.CodeGen.Common.Constants
import  Gom.CodeGen.Common.Builtins

