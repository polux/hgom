-- | Handle user args.

module Gom.Config (
  Config(..),
  gomOpts,
  usage,
  paramsError
) where

import System.Console.GetOpt

-- | Datatype representing the parameters passed to hgom by the user.
data Config =
  Config {
    help    :: Bool, -- ^ display help message ?
    version :: Bool, -- ^ display version information ?
    package :: Maybe [String], -- ^ optional package prefix
    prprint :: Bool  -- ^ pretty-print module and exit ?
  } 

-- | Default configuration.
defaultConfig :: Config
defaultConfig = 
  Config {
    help    = False,
    version = False,
    package = Nothing,
    prprint = False
  }

-- | Pyhton-like split function.
-- @splitAt \'x\' \"aaxbbxcc\" = [\"aa\",\"bb\",\"cc\"]@
split :: Char -> String -> [String]
split _ [] = []
split c (x:xs) 
  | x == c    = split c xs
  | otherwise = let (xs1,xs2) = break (== c) (x:xs)
                in xs1:(split c xs2)

-- | Options description for 'getOpt'.
options :: [OptDescr (Config -> Config)]
options =
  [Option []    ["help"]    (NoArg chelp)                   "show this message",
   Option ['V'] ["version"] (NoArg cversion)                "show version number",
   Option ['p'] ["package"] (ReqArg cpackage "packageName") "specify package name",
   Option ['r'] ["pretty"]  (NoArg cpretty)                 "pretty-print the module and exit"]
  where chelp      c = c { help    = True }
        cversion   c = c { version = True }
        cpackage p c = c { package = Just (split '.' p) }
        cpretty    c = c { prprint = True }

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
    (o,n,[]  ) -> return  (foldl (flip id) defaultConfig o, n)
    (_,_,errs) -> paramsError (concat errs)

-- | Report an error concerning user args.
paramsError :: String -> IO a
paramsError err = ioError (userError (err ++ usageInfo header options))

