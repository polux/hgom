-- | Handle user args.

module Gom.Config (
  Config(..),
  CongrOpt(..),
  gomOpts,
  usage,
  paramsError
) where

import Data.Foldable (foldlM)
import System.Console.GetOpt

-- | Datatype representing the parameters passed to hgom by the user.
data Config =
  Config {
    help    :: Bool, -- ^ display help message ?
    version :: Bool, -- ^ display version information ?
    package :: Maybe [String], -- ^ optional package prefix
    prprint :: Bool,  -- ^ pretty-print module and exit ?
    haskell :: Bool, -- ^ generate toHaskell methods ?
    visit   :: Bool, -- ^ implement visitable ? 
    checker :: Bool, -- ^ perform checks ?
    congr   :: CongrOpt
  } 

-- | Default configuration.
defaultConfig :: Config
defaultConfig = 
  Config {
    help    = False,
    version = False,
    package = Nothing,
    prprint = False,
    haskell = False,
    visit   = True,
    checker = True,
    congr   = NoCongr
  }

-- | Represents the three options values for --withCongruenceStrategies
data CongrOpt = NoCongr | SameFile | SeparateFile

-- | Pyhton-like split function.
-- @split \'x\' \"aaxbbxcc\" = [\"aa\",\"bb\",\"cc\"]@
split :: Char -> String -> [String]
split _ [] = []
split c (x:xs) 
  | x == c    = split c xs
  | otherwise = let (xs1,xs2) = break (== c) (x:xs)
                in xs1:(split c xs2)

-- | Options description for 'getOpt'.
options :: [OptDescr (Config -> IO Config)]
options =
  [Option [] ["help"] (NoArg  chelp) 
          "show this message"
  ,Option ['V'] ["version"] (NoArg  cversion)                
          "show version number"
  ,Option ['r'] ["pretty"] (NoArg  cpretty)
          "pretty-print the module and exit"
  ,Option ['p'] ["package"] (ReqArg cpackage "packageName") 
          "specify package name"
  ,Option ['h'] ["haskell"] (NoArg  chaskell)                
          "generate 'toHaskell' methods"
  ,Option ['c'] ["congruence"] (ReqArg ccongr "(no|same|sep)")
          (unlines ["generate congruence strategies",
                    "in the same or in a separate .tom",
                    "file (defaults to no)"])
  ,Option [] ["noVisitable"] (NoArg cvisit)
          "don't implement Visitable"
  ,Option [] ["noCheck"] (NoArg ccheck)
          "don't perform consistency checks"]

  where chelp      c = return $ c { help = True }
        cversion   c = return $ c { version = True }
        cpackage p c = return $ c { package = Just (split '.' p) }
        cpretty    c = return $ c { prprint = True }
        chaskell   c = return $ c { haskell = True }
        cvisit     c = return $ c { visit = False }
        ccheck     c = return $ c { checker = False }

        ccongr "no"   c = return $ c { congr = NoCongr } 
        ccongr "same" c = return $ c { congr = SameFile }
        ccongr "sep"  c = return $ c { congr = SeparateFile }
        ccongr _      _ = paramsError 
           "'--congruence' argument must be 'no', 'same' or 'sep'.\n"

-- | Usage info message header : @Usage: hgom [OPTION...] file@.
header :: String
header = "Usage: hgom [OPTION...] file"

-- | Usage info message.
usage :: String
usage = usageInfo header options

-- | Parse user args.
gomOpts :: [String] -> IO (Config, [String])
gomOpts argv = 
  case getOpt Permute options argv of
    (o,n,[]  ) -> do conf <- foldlM (\c f -> f c) defaultConfig o
                     return (conf,n)
    (_,_,errs) -> paramsError (concat errs)

-- | Report an error concerning user args.
paramsError :: String -> IO a
paramsError err = error (err ++ usageInfo header options)

