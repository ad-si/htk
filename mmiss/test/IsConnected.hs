{- This program is not part of the regular sources and uses nothing outside
   GHC's standard libraries; its function is to determine if anyone is
   already listening to a particular socket, so that the runWorkbenchRemote
   script can determine whether to set up an SSH tunnel or not.  If so it
   returns with ExitSuccess, otherwise it returns with ExitFailure(4).

   First argument is host name, second is number giving the port number.
   We are not very tolerant of errors, since this program is only meant to
   be called from a script we provide.
   -}
module Main(main) where

import System
import IO hiding (try)

import Network
import Control.Exception

main =
   do
      [hostStr,portStr] <- getArgs
      connected <- try (connectTo hostStr
        (PortNumber (fromIntegral (read portStr :: Int))))
      case connected of
         Left error -> exitWith (ExitFailure 4)
         Right handle ->
            do
               hPutStr handle "\x80\x80\x80"
                  -- null service key, userid and password to tell the
                  -- MMiSS server to quietly tell us to get lost.
               exitWith ExitSuccess




