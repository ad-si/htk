-- |
-- Description: Calling other programs.
--
-- Calling other programs.
--
-- CAVEAT
-- When you start up a ChildProcess, sigPIPE is disabled.  This is necessary
-- as otherwise I know no way of stopping the whole program crashing when
-- the tool shuts the pipe.
-- 
-- ######################################################################### 


module ChildProcess (
   ChildProcess,
   
   Tool(..),
   Destructible(..),
   UnixTool(..),
   
    
   PosixProcess(..), -- holds information about a process to be created
   
   -- linemode, arguments, &c encode configuration options for
   -- a process to be created.  Various functions for creating new
   -- processes take a [Config PosixProcess] as an argument.
   -- In particular newChildProcess does.
   linemode,       -- :: Bool -> Config PosixProcess               
   -- for meaning of linemode see readMsg.
   arguments,      -- :: [String] -> Config PosixProcess
   appendArguments,-- :: [String] -> Config PosixProcess
   environment,    -- :: [(String,String)] -> Config PosixProcess  
   -- workingdir,     -- :: FilePath -> Config PosixProcess
   -- workingdir is removed because it involves getting the child process
   -- to do some work.
   chunksize,      -- :: Int-> Config PosixProcess
   -- the maximal size of one "chunk" of characters read from the 
   -- child process at one time (default 1000).
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

   -- the settings encoded in (parms::[Config PosixProcess]) can be
   -- decoded, with a standard set of defaults, using
   -- Computation.configure defaultPosixProcess parms 
   defaultPosixProcess, -- :: PosixProcess
   
   newChildProcess, -- :: FilePath -> [Config PosixProcess] -> IO ChildProcess


   sendMsg, -- :: ChildProcess -> String -> IO ()
   -- sendMsg sends a String to the ChildProcess, adding a new line
   -- for line mode.

   sendMsgRaw, -- :: ChildProcess -> CStringLen -> IO ()
   -- sendMsgRaw writes a CStringLen
   -- to the child process.  It does not append a newline.

   readMsg, -- :: ChildProcess -> IO String
   closeChildProcessFds,
   
   ProcessStatus(..),
   ToolStatus,
   ProcessID,
   
   Fd,
   readLine,
   writeLine,
   
   readLineError,
   writeLineError  
   
   
   ) 
where

import List
import qualified System

import qualified IO
import qualified System.Posix.IO 
import System.Posix.Types 
import System.Posix.Signals
import System.Posix.Process
import Control.Concurrent
import Foreign.C.String
import Control.Exception

import Computation
import Object
import Debug(debug,debugString,alwaysDebug)
import Thread
import FileNames
import WBFiles
import DeepSeq
import IOExtras
import CompileFlags
import TemplateHaskellHelps
import Maybes
import ExtendedPrelude

import Destructible
import WrapIO

import ProcessClasses
import FdRead

-- --------------------------------------------------------------------------
--  Posix Tool Parameters
-- --------------------------------------------------------------------------

data PosixProcess = 
   PosixProcess {
      args            :: [String], 
      env             :: Maybe [(String, String)],
      wdir            :: Maybe FilePath,
      chksize         :: Int, 
      lmode           :: Bool, -- line mode
      stderr          :: Bool, -- include stderr
      cresponse       :: Maybe (String,String),
      toolname        :: Maybe String
     }

defaultPosixProcess :: PosixProcess
defaultPosixProcess = 
   PosixProcess {
      args = [], 
      env = Nothing, 
      wdir = Nothing, 
      chksize = 1000,
      lmode = True,
      stderr = True,
      cresponse = Nothing,
      toolname = Nothing
      }


linemode        :: Bool -> Config PosixProcess
arguments       :: [String] -> Config PosixProcess
appendArguments :: [String] -> Config PosixProcess
environment     :: [(String,String)] -> Config PosixProcess
-- workingdir      :: FilePath -> Config PosixProcess
chunksize       :: Int-> Config PosixProcess
standarderrors  :: Bool -> Config PosixProcess
challengeResponse :: (String,String) -> Config PosixProcess
toolName :: String -> Config PosixProcess

linemode lm' parms = return parms{lmode = lm'}
arguments args' parms = return parms{args = args'}
environment env' parms = return parms{env = Just env'}
-- workingdir wdir' parms = return parms{wdir = Just wdir'}
chunksize size' parms = return parms{chksize= size' }
standarderrors err' parms = return parms{stderr = err'}
challengeResponse cr parms = return parms {cresponse = Just cr}
toolName n parms = return parms {toolname = Just n}

appendArguments args' parms = return parms{args = (args parms) ++ args'}

-- -------------------------------------------------------------------------
-- Data Declaration
-- -------------------------------------------------------------------------

data ChildProcess = 
   ChildProcess {
      childObjectID :: ObjectID, 
      lineMode :: Bool,-- if True readMsg returns lines, otherwise
                       -- it returns the first input that's available.
      writeTo :: System.Posix.Types.Fd,   -- to write to the process
      readFrom :: System.Posix.Types.Fd,  -- to read from the process
      
      closeAction :: IO (), -- action when we closeChildProcessFd's.

      processID :: System.Posix.Types.ProcessID,
                       -- process id of child
      bufferVar :: (MVar String),
                       -- bufferVar of previous characters (only relevant
                       -- for line mode)
      toolTitle :: String,
                       -- Title of the tool, derived from the file name,
                       -- used in the debugging file.
      chunkSize :: System.Posix.Types.ByteCount 
                       -- max. size of one "chunk" of characters read
                       -- at one time
      }

-- -------------------------------------------------------------------------
-- Writing debugging information to a file
-- (This code needs to be early because of the splice)
-- -------------------------------------------------------------------------

$(
   if isDebug
      then
         [d|
            debugWrite childProcess str =
               debugString (toolTitle childProcess++">"++str)

            debugRead childProcess str =
               debugString (toolTitle childProcess++"<"++str)
         |]
      else
         [d|
            debugWrite _ _ = done
            debugRead _ _ = done
         |]
   )

-- NB - these functions expect a newline to be at the end of the string.
debugWrite :: ChildProcess -> String -> IO ()
debugRead :: ChildProcess -> String -> IO ()

-- -------------------------------------------------------------------------
-- Primitive operation for forking a process equivalent to GHC 6.02's.
-- -------------------------------------------------------------------------

$(
   if ghcShortVersion >= 602
      then
         [d|
            forkProcess0 :: IO () -> IO ProcessID
            forkProcess0 = $(dynName "forkProcess")
         |]
      else
         [d|
            forkProcess0 :: IO () -> IO ProcessID
            forkProcess0 childAct =
               do
                  processIDOpt <- $(dynName "forkProcess")
                  case processIDOpt of
                     Nothing -> -- child
                        do
                           childAct
                           error "ChildProcess.forkProcess0: childAct\
                              \ shouldn't terminate."
                     Just processID -> return processID
         |]
   )

-- -------------------------------------------------------------------------
-- Constructor
-- -------------------------------------------------------------------------

newChildProcess :: FilePath -> [Config PosixProcess] -> IO ChildProcess
newChildProcess path confs  =
   do                     -- (write,read)
      parms <- configure defaultPosixProcess confs

      debug("newChildProcess:")
      debug(path:(args parms))

      -- Disable sigPIPE.  This means that the whole program
      -- won't crash when the tool exits.  Unfortunately there
      -- doesn't seem to be another way of doing this.
      System.Posix.Signals.installHandler System.Posix.Signals.sigPIPE 
		  System.Posix.Signals.Ignore Nothing


      (readIn,writeIn) <- System.Posix.IO.createPipe 
      -- Pipe to send things to child
      (readOut,writeOut) <- System.Posix.IO.createPipe 
      -- Pipe to read things back from child.

      let
         passOnStdErrs = stderr parms

      readWriteErr <- 
         if passOnStdErrs 
            then
               return Nothing
            else
               do
                  (readErr,writeErr) <- System.Posix.IO.createPipe
                  return (Just (readErr,writeErr))

      -- precompute various things, to prevent the child and parent both
      -- having to compute them.
      let
         arguments1 = args parms
         environment1 = env parms

         errorResponse = "Attempt to start program "++path++" failed: "

      (path,arguments1,environment1,errorResponse) `deepSeq` done

      let
         -- what the childProcess will do.
         childAct :: IO ()
         childAct =
            do
               System.Posix.IO.dupTo readIn System.Posix.IO.stdInput
               System.Posix.IO.closeFd readIn
               System.Posix.IO.closeFd writeIn

               System.Posix.IO.dupTo writeOut System.Posix.IO.stdOutput
               System.Posix.IO.closeFd readOut
               System.Posix.IO.closeFd writeOut

               case readWriteErr of
                  Nothing -> 
                     do
                        System.Posix.IO.dupTo System.Posix.IO.stdOutput System.Posix.IO.stdError
                        done
                  Just (readErr,writeErr) ->
                     do
                        System.Posix.IO.dupTo writeErr System.Posix.IO.stdError
                        System.Posix.IO.closeFd readErr
                        System.Posix.IO.closeFd writeErr

               System.Posix.Process.executeFile path True arguments1 environment1
               putStrLn errorResponse
               System.exitWith (System.ExitFailure 16)

      processID <- forkProcess0 childAct

      childObjectID <- newObject
      bufferVar <- newMVar ""

-- Closing these seems to confuse GHCi, when we run wish more than once.
-- So instead we close all the fds during the destruction action.
--               System.Posix.IO.closeFd readIn
--               System.Posix.IO.closeFd writeOut

      closeAction <-
         case readWriteErr of
            Nothing ->
               return (
                  do
                     System.Posix.IO.closeFd readIn
                     System.Posix.IO.closeFd writeIn
                     System.Posix.IO.closeFd readOut
                     System.Posix.IO.closeFd writeOut
                  )
            Just (readErr,writeErr) ->
               do
                  displayProcess <- 
                     forkIO (goesQuietly(displayStdErr path readErr))
                  return (
                     do
                        killThread displayProcess

                        System.Posix.IO.closeFd readIn
                        System.Posix.IO.closeFd writeIn
                        System.Posix.IO.closeFd readOut
                        System.Posix.IO.closeFd writeOut
                        System.Posix.IO.closeFd readErr
                        System.Posix.IO.closeFd writeErr
                     )
      let
         toolTitle = 
            case (toolname parms,splitName path) of
               (Just toolTitle,_) -> toolTitle
               (Nothing,(dir,toolTitle)) -> toolTitle

         newChild = ChildProcess {
            childObjectID = childObjectID,
            lineMode = lmode parms,
            writeTo = writeIn,
            readFrom = readOut,
            processID = fromIntegral processID,
            bufferVar = bufferVar,
            chunkSize = fromIntegral (chksize parms),
            toolTitle = toolTitle,
            closeAction = closeAction
            }

      -- Do challenge-response
      case (cresponse parms) of
         Nothing -> done
         Just (challenge,response) ->
            Control.Exception.catch
               (do
                  howLong <- getToolTimeOut
                  let
                     timedOutIO :: IO a -> IO (Maybe a)
                     timedOutIO act = impatientIO act 
                        (msecs (fromIntegral howLong))

                     -- Used when things go wrong
                     badResponse :: String -> IO a
                     badResponse mess =
                        do
                           putStrLn ("Challenge response starting "
                              ++toolTitle++" from path \""++path
                              ++"\"  failed")
                           putStrLn mess
                           dumpPending
                           error "Challenge-Response failed"

                     -- Print out pending characters, up to a maximum
                     -- of 1000 characters and waiting up to the time-
                     -- limit if necessary to do so.
                     dumpPending :: IO ()
                     dumpPending =
                        do
                           pendingOpt <- timedOutIO 
                              (readChunk 1000 (readFrom newChild))
                           putStrLn (case pendingOpt of
                              Nothing -> "No more pending output"
                              Just pending -> ("Pending: "
                                 ++show pending)
                              )

                  sendMsg newChild challenge
                  howLong <- getToolTimeOut
                  resultOpt <- timedOutIO
                     (readChunkFixed (fromIntegral (length response)) 
                        (readFrom newChild))
                  result <- case resultOpt of
                     Nothing ->
                        badResponse (
                           "Timed out waiting for initial output\n" ++
                           "Guess: either it's the wrong tool, " ++
                           "or else you need to set the option \n"
                           ++ "  --uni-toolTimeOut=" ++
                           "[LARGE NUMBER OF MILLISECONDS]")
                     Just result -> return result

                  debugRead newChild (result++"\n")

                  if response == result
                     then
                        done
                     else
                        -- Trim common case
                        if isPrefixOf result errorResponse
                           || isPrefixOf errorResponse result
                        then
                           badResponse ("Couldn't execute tool")
                        else
                           badResponse ("Unexpected response was "++
                              show result)
                  )
               (\ exception ->  
                  do
                     putStrLn ("Attempt to start "++toolTitle++
                        " from path \""++path++"\" failed")
                     case Control.Exception.errorCalls exception of
                        Just mess -> 
                           do
                              putStrLn mess
                              error "Tool start failed"
                        Nothing ->
                           do
                              putStrLn ("Mysterious exception: "
                                 ++show exception)
                              Control.Exception.throw exception
                  ) 
      return newChild

getStatus :: System.Posix.Types.ProcessID -> IO ToolStatus
-- Immediately return Nothing if tool hasn't yet finished, or if
-- it finished too long ago and the system has forgotten its
-- status; otherwise return Just (its exit status).
getStatus pid = 
   do
      ans <- try(System.Posix.Process.getProcessStatus False True pid)
      -- translation: call waitpid on process specifying
      -- (1) set WNOHANG - don't block if status isn't available,
      --     instead return immediately with Nothing.
      -- (2) set WUNTRACED -- also report stopped
      --     childprocesses if they haven't previously
      --     been reported.
      case ans of
         Left e -> return Nothing
         -- Error.  Process must have terminated and disappeared.
         Right status -> return status

-- --------------------------------------------------------------------------
--  Tool Instance
-- --------------------------------------------------------------------------

instance Object ChildProcess where
   objectID = childObjectID

instance Destroyable ChildProcess where
   destroy child = 
      do
         res <- try(System.Posix.Signals.signalProcess 
		    System.Posix.Signals.sigKILL (processID child))
         case res of
            Left error -> 
               debug "ChildProcess.destroy failed; destruction anticipated?"
            _ -> return ()

instance Tool ChildProcess where
   getToolStatus (ChildProcess {processID = processID}) = getStatus processID


instance UnixTool ChildProcess where
   getUnixProcessID child = return(processID child)


-- -------------------------------------------------------------------------
-- Commands
-- -------------------------------------------------------------------------

{- line mode readMsg has been changed so it doesn't do a System.Posix.IO.fdRead
   on every character. -}
readMsg :: ChildProcess -> IO String
readMsg (child@ChildProcess 
      {lineMode = True, chunkSize = chunkSize, 
       readFrom = readFrom, bufferVar = bufferVar}) = 
   do
      buffer <- takeMVar bufferVar 
      (newBuffer,result) <- readWithBuffer readFrom buffer []
      putMVar bufferVar newBuffer
      debugRead child (result++"\n")
      return result
   where
      readWithBuffer readFrom [] acc = 
      -- we use an accumulating parameter since I don't want a
      -- non-tail-recursive action.
         do
            nextChunk <- readChunk chunkSize readFrom
            readWithBuffer readFrom nextChunk acc
      readWithBuffer readFrom ('\n' : rest) acc =
         return (rest,reverse acc)
      readWithBuffer readFrom (other : rest) acc =
         readWithBuffer readFrom rest (other : acc)
readMsg (child @ ChildProcess {lineMode = False, readFrom = readFrom,
	               chunkSize = chunkSize}) =
   do
      result <- readChunk chunkSize readFrom
      debugRead child (result++"\n")
      return result

readChunk :: System.Posix.Types.ByteCount -> System.Posix.Types.Fd -> IO String
-- read a chunk of characters, waiting until at least one is available.
readChunk size fd =
   do
      waitForInputFd fd
      fdReadOpt <- catchEOF (System.Posix.IO.fdRead fd size)
      case fdReadOpt of
         Nothing -> 
            error "ChildProcess : fdRead returned EOF"
         Just (input,count) ->
            if count <= 0 
               then 
                  raise (userError "ChildProcess: input error")
               else
                  return input

readChunkFixed :: System.Posix.Types.ByteCount -> System.Posix.Types.Fd -> IO String
-- like readChunk except that it tries to read as many characters
-- as are asked for.
readChunkFixed size fd = readChunkInner "" size
   where
      readChunkInner s toRead = 
         if toRead == 0 
         then 
            return s
         else
            do               
               next <- readChunk toRead fd
               readChunkInner (s ++ next) (toRead 
                  - (fromIntegral (length next)))
         
sendMsg :: ChildProcess -> String -> IO ()
sendMsg (child @ ChildProcess{lineMode = True,writeTo = writeTo}) str  = 
   do
      debugWrite child (str++"\n")
      writeLine writeTo str
sendMsg (child @ (ChildProcess{lineMode = False,writeTo = writeTo})) str  = 
   do 
      debugWrite child (str ++ "\n") 
      countByteCount <- System.Posix.IO.fdWrite writeTo str
      -- see man -s 2 write for when write() returns 0.
      let
         count = fromIntegral countByteCount

      if count < 0 
         then
            raise writeLineError
         else if count < l then
            sendMsg child (drop count str)
         else
            return ()
   where l = length str

-- sendMsgRaw writes a CStringLen to the child process.  
-- It does not append a newline.
sendMsgRaw :: ChildProcess -> CStringLen -> IO ()
sendMsgRaw (child@ChildProcess{writeTo = writeTo}) cStringLen =
   do
      if isDebug
         then
               do
                  str <- peekCStringLen cStringLen
                  debugWrite child str
         else
            done
      fdWritePrim writeTo cStringLen


closeChildProcessFds  :: ChildProcess -> IO ()
closeChildProcessFds (ChildProcess{closeAction = closeAction}) = 
   closeAction

-- -------------------------------------------------------------------------
-- Displaying stdErr output
-- -------------------------------------------------------------------------

displayStdErr :: FilePath -> System.Posix.Types.Fd -> IO ()
displayStdErr progName stdErrFd =
   do
      errOutput <- readChunk 1000 stdErrFd 
      let
         errHead = progName++" error:"
         -- Put errHead at the start of every line of output.
         errPretty = unlines(map (errHead++) (lines errOutput))
      IO.hPutStr IO.stderr errPretty
      IO.hPutStr IO.stderr "\n"
      IO.hFlush IO.stderr
      displayStdErr progName stdErrFd

-- -------------------------------------------------------------------------
-- Reading and Writing Lines from Channels
-- -------------------------------------------------------------------------

readLine :: System.Posix.Types.Fd -> String -> IO String
-- Read a line from the Fd, and returns it without the final "\n".  
-- Raises error
-- if we get to EOF first or there is an IO error.  
-- The second argument should be "".  (It's an accumulating parameter)
readLine fd buf =
   do
      (inp,count) <- System.Posix.IO.fdRead fd 1 -- at most one char
      if count /= 1 
         then 
            raise readLineError
         else
            next inp                   
        where 
           next "\n" = 
              let
                 contents = reverse buf
              in
                 do
                    debug ("readLine read " ++ contents)
                    return (reverse buf)

           next x = readLine fd (x ++ buf)

-- writeLine fd str writes the given string to the Fd adding a newline
-- char
writeLine :: System.Posix.Types.Fd -> String -> IO ()
writeLine fd str =
   do
      countByteCount <- System.Posix.IO.fdWrite fd msg
      let
         count = fromIntegral countByteCount

      if count < 0
      -- see man -s 2 write for when write() returns 0.
         then
            raise writeLineError
         else if count /= length msg 
         then
            writeLine fd (drop count str)
         else
            done
   where msg = str ++ "\n" 

readLineError :: IOError
readLineError = userError "ChildProcess: read line error"

writeLineError :: IOError
writeLineError = userError "ChildProcess: write line error"

-- -------------------------------------------------------------------------
-- Waiting for input on an Fd.
-- -------------------------------------------------------------------------

waitForInputFd :: System.Posix.Types.Fd -> IO()
waitForInputFd fd  = threadWaitRead(fromIntegral fd)

