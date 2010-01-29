------------------------------------------------------------------
-- |
-- Module      : Gom.CodeGen.Abstract
-- Copyright   : (c) Paul Brauner 2009
--               (c) Emilie Balland 2009
--               (c) INRIA 2009
-- Licence     : GPL (see COPYING)
--
-- Maintainer  : paul.brauner@inria.fr
-- Stability   : provisional
-- Portability : non-portable (requires generalized newtype deriving)
--------------------------------------------------------------------

module Gom.CodeGen.Abstract where

import Gom.SymbolTable
import Gom.Java
import Gom.Constants
import Gom.Config
import Gom.CodeGen.Common

import Text.PrettyPrint.Leijen

-- | Generates the @ModAbstractType@ abstract java class for module @Mod@.
compAbstract :: Gen FileHierarchy
compAbstract = do at <- abstractType
                  -- if haskell option is enabled, generate abstract toHaskell
                  hs <- ifConf haskell hask []
                  -- if sharing option is enabled, generate afferent abstract
                  -- methods
                  ss <- ifConf sharing share []
                  -- if visit option is enabled, implement visitable 
                  iv <- ifConf visit [jVisitable] []
                  -- if sharing option is enabled, implement shared
                  is <- ifConf sharing [jSharedId] []
                  -- if String is imported we generate renderString
                  im <- askSt importsString
                  let rs = if im then str else []
                  -- if random is enabled, generate builtin random generation
                  ra <- ifConf random rdm []
                  -- build the class
                  return $ Class at (cl at (hs++ss++rs++ra) (iv++is))
  where cl at e i = rClass (public <+> abstract) (text at) 
                           Nothing i (body e)
        body      = vcat . (always ++)
        always    = [abstractSymbolName,toStringBody,abstractToStringBuilder]
        hask      = [toHaskellBody,abstractToHaskellBuilder]
        share     = [abstractSharing]
        rdm       = [abstractRandom]
        str       = [renderStringMethod] 


