-------------------------------------------------------------------
-- |
-- Module      : OOMappings.Config
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

module OOMappings.Config (
  Config(..),
  oomOpts,
  usage,
  paramsError
) where

import Control.Monad.Error
import Data.Foldable(foldlM)
import System.Console.GetOpt

import qualified Common.Config as CC

-- | Datatype representing the parameters passed to hgom by the user.
data Config =
  Config {
    help    :: Bool, -- ^ display help message ?
    version :: Bool, -- ^ display version information ?
    package :: Maybe [String], -- ^ optional package prefix
    prprint :: Bool,  -- ^ pretty-print module and exit ?
#if TEST
    utests  :: Maybe [String], -- ^ run unit tests ? 
#endif
    checker :: Bool, -- ^ perform checks ?
    compact :: Bool  -- ^ generate compact code ?
  } 

-- | Default configuration.
defaultConfig :: Config
defaultConfig = 
  Config {
    help    = False,
    version = False,
    package = Nothing,
    prprint = False,
#if TEST
    utests  = Nothing,
#endif
    checker = True,
    compact = False
  }

-- | OOMappings.Config is an instance of Common.Config
instance CC.IsConfig Config where
  package = package

-- | Pyhton-like split function.
-- @split \'x\' \"aaxbbxcc\" = [\"aa\",\"bb\",\"cc\"]@
split :: Char -> String -> [String]
split _ [] = []
split c (x:xs) 
  | x == c    = split c xs
  | otherwise = let (xs1,xs2) = break (== c) (x:xs)
                in xs1:split c xs2

-- | Options description for 'getOpt'.
options :: [OptDescr (Config -> Either String Config)]
options =
  [Option [] ["help"] (NoArg  chelp) 
          "show this message and exit"
  ,Option ['V'] ["version"] (NoArg  cversion)                
          "show version number and exit"
  ,Option ['P'] ["pretty"] (NoArg  cpretty)
          "pretty-print the module and exit"
#if TEST
  ,Option [] ["test"] (ReqArg cutests "args")
          (unlines ["run test suite with args"])
#endif
  ,Option ['p'] ["package"] (ReqArg cpackage "packageName") 
          "specify package name"
  ,Option [] ["noCheck"] (NoArg ccheck)
          "don't perform consistency checks"
  ,Option [] ["compact"] (NoArg ccompact)
          "generate compact code (no indentation)"]

  where chelp      c = return $ c { help    = True  }
        cversion   c = return $ c { version = True  }
        cpretty    c = return $ c { prprint = True  }
        ccheck     c = return $ c { checker = False }
        ccompact   c = return $ c { compact = True  }
#if TEST
        cutests  s c = return $ c { utests = Just (words s) }
#endif
        cpackage p c = return $ c { package = Just (split '.' p) }

-- | Usage info message header : @Usage: oomappings [OPTION...] file@.
header :: String
header = "Usage: oomappings [OPTION...] file"

-- | Usage info message.
usage :: String
usage = usageInfo header options

-- | Parse user args. Returns either an error message, 
-- either a configuration along with unparsed arguments.
oomOpts :: [String] -> Either String (Config,[String])
oomOpts argv = 
  case getOpt Permute options argv of
    (o,n,[]  ) -> do conf <- foldlM (\c f -> f c) defaultConfig o
                     return (conf,n)
    (_,_,errs) -> throwError $ concat errs

-- | Report an error concerning user args (includes usage info reminder).
paramsError :: String -> IO a
paramsError err = error (err ++ usageInfo header options)

