{- Access to environment variables not provided elsewhere -}
module SysVars(
   unSetEnv, -- :: String -> IO ()
      -- delete an environment variable from the process's environment.
   ) where

import CString

foreign import ccall unsafe "stdlib.h unsetenv" unSetEnvPrim 
   :: CString -> IO ()

unSetEnv :: String -> IO ()
unSetEnv str = withCString str unSetEnvPrim


