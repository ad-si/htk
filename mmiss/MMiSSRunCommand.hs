-- | This module runs some command in a similar way to "SafeSystem",
-- but displays output in an HTk log window.
-- 
-- Really this ought to be in the posixutil or htk.  But it can't go in
-- posixutil because it needs htk, and it can't go in htk because (thanks to
-- Windows) htk isn't allowed to assume posixutil.
module MMiSSRunCommand(
   runTool,
   runCommand,
   copyFileBool, 
   errorWin
   ) where

import System

import Control.Concurrent

import Computation
import Messages

import SafeSystem
import CopyFile

import TextDisplay
import HTk
import DialogWin

-- | Run a tool, return ExitCode and tool output.
-- 
-- The first String is the title of the tool.
-- The second String is the command to run.
runTool :: String-> String-> IO (ExitCode, String)
runTool title command =
   do toolOutputMVar <- newMVar []
      let outputSink str =
              do toolOutput <- takeMVar toolOutputMVar
                 putMVar toolOutputMVar (str: toolOutput)
      exitCode <- safeSystemGeneral command outputSink
      output   <- takeMVar toolOutputMVar
      return (exitCode, unlines (reverse output ))

-- | Run a command; all output is considered error reporting,
-- 
-- We return True if the command succeeds; IE returns with exit code
-- ExitSuccess.
runCommand :: String -> String -> IO Bool
runCommand title command =
   do (exitCode, toolOutput) <- runTool title command
      case exitCode of
         ExitSuccess -> return True
         ExitFailure code ->
            do errorWin title code toolOutput
               return False


errorWin :: String -> Int -> String -> IO () 
errorWin title code output =
   do
      showOutput <- createDialogWin
         [("Continue",False),("Show Tool Output",True)]
         Nothing
         [text (title ++ " returned with error code "++show code)]
         [text (title ++ " error")]
      when showOutput $ createTextDisplay (title ++ ": output") output []


-- Equivalent action, using CopyFile.copyFile, for copying a file.
copyFileBool :: String -> String -> IO Bool
copyFileBool source destination =
   do
      unitWE <- copyFileWE source destination
      case (fromWithError unitWE) of
         Right () -> return True
         Left mess -> 
            do
               errorMess mess
               return False
