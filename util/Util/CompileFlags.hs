-- | This module (which needs to be expanded by .\/configure) contains flags
-- which (via Template Haskell) control compilation.
module Util.CompileFlags where

ghcShortVersion :: Int 
ghcShortVersion = 612

osTitle :: String
osTitle = "linux"

isWindows :: Bool
isWindows = (0 /= 0)

#ifdef DEBUG
isDebug :: Bool
isDebug = True
#else
isDebug :: Bool
isDebug = False
#endif

uniVersion :: String
uniVersion = "2.0"
