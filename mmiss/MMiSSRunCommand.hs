{- This module runs some command in a similar way to posixutil/safeSystem,
   but displays output in an HTk log window.

   Really this ought to be in the posixutil or htk.  But it can't go in
   posixutil because it needs htk, and it can't go in htk because (thanks to
   Windows) htk isn't allowed to assume posixutil.
   -}
module MMiSSRunCommand(runCommand) where

import System

import Concurrent

import Computation

import SafeSystem

import DialogWin
import TextDisplay
import HTk

---
-- We return True if the command succeeds; IE returns with exit code
-- ExitSuccess.
--
-- The first String is the title of the tool.
-- The second String is the command to run.
runCommand :: String -> String -> IO Bool
runCommand title command =
   do
      toolOutputMVar <- newMVar []
      let
         outputSink str = 
            do
               toolOutput <- takeMVar toolOutputMVar
               putMVar toolOutputMVar (str : toolOutput)

      exitCode <- safeSystemGeneral command outputSink
      case exitCode of
         ExitSuccess -> return True
         ExitFailure code ->
            do
               toolOutput <- takeMVar toolOutputMVar
               errorWin title code (reverse toolOutput)
               return False


errorWin :: String -> Int -> [String] -> IO () 
errorWin title code output =
   do
      showOutput <- createDialogWin
         [("Continue",False),("Show Tool Output",True)]
         Nothing
         [text (title ++ " returned with error code "++show code)]
         [text (title ++ " error")]
      if showOutput
         then
            createTextDisplay (title ++ ": output") (unlines output) []
         else
            done