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

module Gom.CodeGen.Abstract (
  compAbstract
) where

import Gom.CodeGen.Common
import Gom.Common.SymbolTable
import Gom.Config
import Gom.Common.FileGen

import Control.Applicative
import Text.PrettyPrint.Leijen hiding ((<$>))

-- | Generates the @ModAbstractType@ abstract java class for module @Mod@.
compAbstract :: Gen FileHierarchy
compAbstract = do at <- abstractType
                  -- if haskell option is enabled, generate abstract toHaskell
                  hs <- ifConf haskell hask []
                  -- if sharing option is enabled, generate abstract methods
                  ss <- ifConf sharing [abstractSharing] []
                  -- if visit option is enabled, implement visitable 
                  iv <- ifConf visit [jVisitable] []
                  -- if sharing option is enabled, implement shared
                  is <- ifConf sharing [jSharedId] []
                  -- generate renderString and/or renderChar
                  rs <- str <$> askSt importsString <*> askSt importsChar
                  -- if random is enabled, generate builtin random generation
                  ra <- ifConf random [randomBuiltins] []
                  -- if depth is enabled, generate size abstract method
                  de <- ifConf depth [abstractDepth] []
                  -- if size is enabled, generate size abstract method
                  si <- ifConf size [abstractSize] []
                  -- build the class
                  return $ Class at (cl at (hs++ss++rs++ra++de++si) (iv++is))

  where cl at e i = rClass (public <+> abstract) (text at) Nothing i (body e)
        body      = vcat . (always ++)
        always    = [abstractSymbolName,toStringBody,abstractToStringBuilder]
        hask      = [toHaskellBody,abstractToHaskellBuilder]

        str True  _     = [renderStringMethod,renderCharMethod] 
        str False True  = [renderCharMethod]
        str False False = []
