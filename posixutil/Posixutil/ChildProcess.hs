{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Description: Calling other programs.
--
-- Calling other programs.
--
-- This module now serves basically as an interface to GHC's new
-- System.Process module.
module Posixutil.ChildProcess (
   ChildProcess,
   PosixProcess, -- holds information about a process to be created

   ChildProcessStatus(ChildExited,ChildTerminated),

   -- linemode, arguments, &c encode configuration options for
   -- a process to be created.  Various functions for creating new
   -- processes take a [Config PosixProcess] as an argument.
   -- In particular newChildProcess does.
   linemode,       -- :: Bool -> Config PosixProcess
   -- for meaning of linemode see readMsg.
   arguments,      -- :: [String] -> Config PosixProcess
   appendArguments,-- :: [String] -> Config PosixProcess
   environment,    -- :: [(String,String)] -> Config PosixProcess
   standarderrors, -- :: Bool -> Config PosixProcess
   -- if standarderrors is true, we send stderr to the childprocesses
   -- out channel (of which there is only one).  Otherwise we
   -- display them, with the name of the responsible process,
   -- on our stderr.
   challengeResponse, -- :: (String,String) -> Config PosixProcess
   -- Set a "challenge" and "response".  Each is given as a String.
   -- The challenge (first String) will have newline appended if in line-mode.
   -- However the response will always be expected exactly as is.
   toolName, -- :: String -> Config PosixProcess
   -- The name of the tool, used in error messages and in the debug file.

   newChildProcess, -- :: FilePath -> [Config PosixProcess] -> IO ChildProcess


   sendMsg, -- :: ChildProcess -> String -> IO ()
   -- sendMsg sends a String to the ChildProcess, adding a new line
   -- for line mode.

   sendMsgRaw, -- :: ChildProcess -> CStringLen -> IO ()
   -- sendMsgRaw writes a CStringLen
   -- to the child process.  It does not append a newline.

   readMsg, -- :: ChildProcess -> IO String

   waitForChildProcess, -- :: ChildProcess -> IO ChildProcessStatus
    -- waits until the ChildProcess exits or is terminated
   )
where

import System.IO

import Foreign.C.String
import System.Exit
import System.Process
import Control.Concurrent
import qualified Control.Exception as Exception

import Util.Computation
import Util.CompileFlags
import Util.Object
import Util.IOExtras
import Util.Debug
import Util.FileNames

import Events.Destructible

import Posixutil.BlockSigPIPE
import Posixutil.ProcessClasses

-- --------------------------------------------------------------------------
--  Tool Parameters
-- --------------------------------------------------------------------------

-- | Describes configuration options for the process.
data PosixProcess =
   PosixProcess {
      args            :: [String],
      ppenv           :: Maybe [(String, String)],
      lmode           :: Bool, -- line mode
      includestderr   :: Bool, -- include stderr
      cresponse       :: Maybe (String,String),
      toolname        :: Maybe String
     }

-- | Initial configuration options.
defaultPosixProcess :: PosixProcess
defaultPosixProcess =
   PosixProcess {
      args = [],
      ppenv = Nothing,
      lmode = True,
      includestderr = True,
      cresponse = Nothing,
      toolname = Nothing
      }


-- | If 'True', 'readMsg' returns lines, otherwise it returs the first input
-- that's available
linemode :: Bool -> Config PosixProcess
linemode lm' parms = return parms{lmode = lm'}

-- | Set command arguments
arguments :: [String] -> Config PosixProcess
arguments args' parms = return parms{args = args'}

-- | Append command arguments
appendArguments :: [String] -> Config PosixProcess
appendArguments args' parms = return parms{args = (args parms) ++ args'}

-- | Set the process' environment.
environment :: [(String,String)] -> Config PosixProcess
environment env' parms = return parms{ppenv = Just env'}

-- if 'True', we send stderr to the childprocesses
-- out channel (of which there is only one).  Otherwise we
-- display them, with the name of the responsible process,
-- on our stderr.
standarderrors :: Bool -> Config PosixProcess
standarderrors err' parms = return parms{includestderr = err'}

-- | Set a "challenge" and "response".  This is used as a test
-- when the tool starts up, to make sure that everything is
-- working properly.
---
-- The challenge (first String) will have newline appended if in line-mode.
-- However the response will always be expected exactly as is.
challengeResponse :: (String,String) -> Config PosixProcess
challengeResponse cr parms = return parms {cresponse = Just cr}

-- | The name of the tool, used in error messages and in the debug file.
toolName :: String -> Config PosixProcess
toolName n parms = return parms {toolname = Just n}


-- -------------------------------------------------------------------------
-- Data Declaration
-- -------------------------------------------------------------------------

-- | A running process
data ChildProcess = ChildProcess {
   processHandle :: ProcessHandle,
      -- | GHC's handle to the process.

   processIn :: Handle,
   processOutput :: Chan String,

   childObjectID :: ObjectID,
   lineMode :: Bool,
      -- | if True readMsg returns lines, otherwise
      -- it returns the first input that's available.
   toolTitle :: String
      -- Title of the tool, derived from the file name or
      -- supplied by the toolName function,
      -- used in the debugging file.
   }

-- | Status if a process
data ChildProcessStatus = ChildExited ExitCode
                        | ChildTerminated
   deriving (Eq, Ord, Show)

-- -------------------------------------------------------------------------
-- Initialising
-- -------------------------------------------------------------------------

-- | Starting a new 'ChildProcess'
newChildProcess :: FilePath -> [Config PosixProcess] -> IO ChildProcess
newChildProcess filePath configurations =
   do
      parms <- configure defaultPosixProcess configurations

      debug("newChildProcess:")
      debug(filePath:(args parms))

      blockSigPIPE

      -- run the process.
      (processIn,processOut,processErr,processHandle) <- runInteractiveProcess
         filePath (args parms) Nothing (ppenv parms)

      childObjectID <- newObject

      processOutput <- newChan

      let
         toolTitle :: String
         toolTitle =
            case (toolname parms,splitName filePath) of
               (Just toolTitle,_) -> toolTitle
               (Nothing,(dir,toolTitle)) -> toolTitle

         lineMode :: Bool
         lineMode = lmode parms

         getFn :: Handle -> IO String
         getFn = if lineMode then hGetLine else getAvail

         -- Worker thread which reads input from the tool and sends it to
         -- processOutput
         monitorHandle :: Handle -> IO ()
         monitorHandle handle =
            foreverUntil (
               do
                  nextOrEOF <- catchEOF (getFn handle)
                  case nextOrEOF of
                     Nothing -> return True
                     Just line ->
                        do
                           debugRead childProcess (line ++ "\n")
                           writeChan processOutput line
                           return False
               )

         -- Worker thread which reads input from the tool (in fact, stderr)
         -- and reports it.
         reportErrors :: IO ()
         reportErrors =
            foreverUntil (
               do
                  nextOrEOF <- catchEOF (getFn processErr)
                  case nextOrEOF of
                     Nothing -> return True
                     Just line ->
                        do
                           hPutStrLn stderr ("Error from " ++ toolTitle
                              ++ ": " ++ line)
                           hFlush stderr
                           return False
               )

         getAvail :: Handle -> IO String
         getAvail handle =
            do
               c0 <- hGetChar handle -- force a wait if necessary
               getAvail0 [c0] handle

         getAvail0 :: String -> Handle -> IO String
         getAvail0 acc handle =
            do
               ready <- hReady handle
               if ready
                  then
                     do
                        c <- hGetChar handle
                        getAvail0 (c : acc) handle
                  else
                     return (reverse acc)

         childProcess = ChildProcess {
            processHandle = processHandle,
            processIn = processIn,
            processOutput = processOutput,
            childObjectID = childObjectID,
            lineMode = lineMode,
            toolTitle = toolTitle
            }

      -- Do challenge-response
      case cresponse parms of
         Nothing -> done
         Just (challenge,response) ->
            do
               sendMsg childProcess challenge
               responseLineOrError
                  <- Exception.try (mapM (const (hGetChar processOut))
                     [1..length response])
               case responseLineOrError of
                  Left excep -> error
                     $ "Starting " ++ toolTitle ++ " got IO error "
                     ++ show (excep :: Exception.IOException)
                  Right line -> if line == response
                     then
                        done
                     else
                        do
                           remainder <- getAvail0 [] processOut
                           error (
                              "Starting " ++ toolTitle
                              ++ " got unexpected response "
                              ++ line ++ remainder
                              )

      forkIO (monitorHandle processOut)
      if includestderr parms
         then
            forkIO (monitorHandle processErr)
         else
            forkIO (reportErrors)

      return childProcess

-- -------------------------------------------------------------------------
-- Communicating with the process
-- -------------------------------------------------------------------------

-- | Sends a String to the ChildProcess, adding a new line
-- for line mode.
sendMsg :: ChildProcess -> String -> IO ()
sendMsg childProcess line =
   do
      debugWrite childProcess line
      let
         lineToWrite =
            if lineMode childProcess then line ++ recordSep else line
      hPutStr (processIn childProcess) lineToWrite
      hFlush (processIn childProcess)



-- | Writes a CStringLen
-- to the child process.  It does not append a newline.
sendMsgRaw :: ChildProcess -> CStringLen -> IO ()
sendMsgRaw childProcess (cStrLn@(ptr,len)) =
   do
      if isDebug
         then
            do
               str <- peekCStringLen cStrLn
               debugWrite childProcess str
         else
            done
      hPutBuf (processIn childProcess) ptr len
      hFlush (processIn childProcess)

-- | Reads a string from the ChildProcess
readMsg :: ChildProcess -> IO String
readMsg childProcess = readChan (processOutput childProcess)

-- -------------------------------------------------------------------------
-- Waiting for a process
-- -------------------------------------------------------------------------

-- | Waits for the ChildProcess to exit or be terminated
waitForChildProcess :: ChildProcess -> IO ChildProcessStatus
waitForChildProcess p =
  Exception.catch (waitForChild p)
    (\ (_ :: Exception.SomeException) -> return ChildTerminated)
  where
    waitForChild p = do
      exitCode <- waitForProcess (processHandle p)
      return (ChildExited exitCode)

-- -------------------------------------------------------------------------
-- Writing debugging information to a file
-- -------------------------------------------------------------------------

debugWrite :: ChildProcess -> String -> IO ()
debugWrite childProcess str =
   debugString (toolTitle childProcess++">"++str++"\n")

debugRead :: ChildProcess -> String -> IO ()
debugRead childProcess str =
   debugString (toolTitle childProcess++"<"++str++"\n")

-- -------------------------------------------------------------------------
-- Instances
-- -------------------------------------------------------------------------

instance Object ChildProcess where
   objectID = childObjectID

instance Destroyable ChildProcess where
   destroy childProcess = terminateProcess (processHandle childProcess)

instance Tool ChildProcess where
   getToolStatus childProcess = getProcessExitCode (processHandle childProcess)
