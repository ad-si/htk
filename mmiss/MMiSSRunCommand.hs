{- This module runs some command in a similar way to posixutil/safeSystem,
   but displays output in an HTk log window.

   Really this ought to be in the posixutil or htk.  But it can't go in
   posixutil because it needs htk, and it can't go in htk because (thanks to
   Windows) htk isn't allowed to assume posixutil.
   -}
module MMiSSRunCommand(runCommand) where

import System

import Computation

import SafeSystem

import DialogWin
import LogWin
import HTk(text)

---
-- We return True if the command succeeds; IE returns with exit code
-- ExitSuccess.
--
-- The first String is the title of the window for command output.
-- The second String is the command to run.
runCommand :: String -> String -> IO Bool
runCommand title command =
   do
      logWin <- createLogWin [text title]
      let
         outputSink str = writeLogWin logWin (str ++"\n")
      exitCode <- safeSystemGeneral command outputSink
      case exitCode of
         ExitSuccess -> return True
         ExitFailure code ->
            do
               createErrorWin (title ++ " returned with error code "
                  ++show code) []
               return False