{- #########################################################################

MODULE        : ChildProcess
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Utility for modelling loosely coupled Posix client/server
                communication.

CAVEATS       : If a thread is waiting on threadWaitRead fd, and another 
                another thread closes the fd, the the whole process is
                terminated (rather than just the waiting thread).

                If a thread attempts to write to a channel that is closed,
                then it is automatically garbage collected, where we 
                would have prefereed an exception to be raised. 

                This means that it becomes very difficult for an application
                like the ChildProcess utility to terminate itself
                gracefully.
When you start up a ChildProcess, sigPIPE is disabled.  This is necessary
as otherwise I know no way of stopping the whole program crashing when
the tool shuts the pipe.

   ######################################################################### -}


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
import qualified System.Posix.IO as Posix
import System.Posix.Types as PosixTypes
import qualified System.Posix.Signals as PosixSignals
import System.Posix.Process as PosixProcess

import ByteArray
import qualified CString
import qualified Exception
import qualified Select

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

import Concurrent
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
      writeTo :: PosixTypes.Fd,   -- to write to the process
      readFrom :: PosixTypes.Fd,  -- to read from the process
      
      closeAction :: IO (), -- action when we closeChildProcessFd's.

      processID :: PosixTypes.ProcessID,
                       -- process id of child
      bufferVar :: (MVar String),
                       -- bufferVar of previous characters (only relevant
                       -- for line mode)
      toolTitle :: String,
                       -- Title of the tool, derived from the file name,
                       -- used in the debugging file.
      chunkSize :: PosixTypes.ByteCount 
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
            forkProcess0 = $(dynName "PosixProcess.forkProcess")
         |]
      else
         [d|
            forkProcess0 :: IO () -> IO ProcessID
            forkProcess0 childAct =
               do
                  processIDOpt <- $(dynName "PosixProcess.forkProcess")
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
      PosixSignals.installHandler PosixSignals.sigPIPE 
		  PosixSignals.Ignore Nothing


      (readIn,writeIn) <- Posix.createPipe 
      -- Pipe to send things to child
      (readOut,writeOut) <- Posix.createPipe 
      -- Pipe to read things back from child.

      let
         passOnStdErrs = stderr parms

      readWriteErr <- 
         if passOnStdErrs 
            then
               return Nothing
            else
               do
                  (readErr,writeErr) <- Posix.createPipe
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
               Posix.dupTo readIn Posix.stdInput
               Posix.closeFd readIn
               Posix.closeFd writeIn

               Posix.dupTo writeOut Posix.stdOutput
               Posix.closeFd readOut
               Posix.closeFd writeOut

               case readWriteErr of
                  Nothing -> 
                     do
                        Posix.dupTo Posix.stdOutput Posix.stdError
                        done
                  Just (readErr,writeErr) ->
                     do
                        Posix.dupTo writeErr Posix.stdError
                        Posix.closeFd readErr
                        Posix.closeFd writeErr

               PosixProcess.executeFile path True arguments1 environment1
               putStrLn errorResponse
               System.exitWith (System.ExitFailure 16)

      processID <- forkProcess0 childAct

      childObjectID <- newObject
      bufferVar <- newMVar ""

-- Closing these seems to confuse GHCi, when we run wish more than once.
-- So instead we close all the fds during the destruction action.
--               Posix.closeFd readIn
--               Posix.closeFd writeOut

      closeAction <-
         case readWriteErr of
            Nothing ->
               return (
                  do
                     Posix.closeFd readIn
                     Posix.closeFd writeIn
                     Posix.closeFd readOut
                     Posix.closeFd writeOut
                  )
            Just (readErr,writeErr) ->
               do
                  displayProcess <- 
                     forkIO (goesQuietly(displayStdErr path readErr))
                  return (
                     do
                        killThread displayProcess

                        Posix.closeFd readIn
                        Posix.closeFd writeIn
                        Posix.closeFd readOut
                        Posix.closeFd writeOut
                        Posix.closeFd readErr
                        Posix.closeFd writeErr
                     )
      let
         toolTitle = 
            case (toolname parms,splitName path) of
               (Just toolTitle,_) -> toolTitle
               (Nothing,Just (dir,toolTitle)) -> toolTitle
               (Nothing,Nothing) -> path

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
            Exception.catch
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
                              ++toolTitle++" failed")
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
                           ++ "  --uni-option=" ++
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
                     case Exception.errorCalls exception of
                        Just mess -> 
                           do
                              putStrLn mess
                              error "Tool start failed"
                        Nothing ->
                           do
                              putStrLn ("Mysterious exception: "
                                 ++show exception)
                              Exception.throw exception
                  ) 
      return newChild

getStatus :: PosixTypes.ProcessID -> IO ToolStatus
-- Immediately return Nothing if tool hasn't yet finished, or if
-- it finished too long ago and the system has forgotten its
-- status; otherwise return Just (its exit status).
getStatus pid = 
   do
      ans <- try(PosixProcess.getProcessStatus False True pid)
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
         res <- try(PosixSignals.signalProcess 
		    PosixSignals.sigKILL (processID child))
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

{- line mode readMsg has been changed so it doesn't do a Posix.fdRead
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

readChunk :: PosixTypes.ByteCount -> PosixTypes.Fd -> IO String
-- read a chunk of characters, waiting until at least one is available.
readChunk size fd =
   do
      waitForInputFd fd
      fdReadOpt <- catchEOF (Posix.fdRead fd size)
      case fdReadOpt of
         Nothing -> 
            error "ChildProcess : fdRead returned EOF"
         Just (input,count) ->
            if count <= 0 
               then 
                  raise (userError "ChildProcess: input error")
               else
                  return input

readChunkFixed :: PosixTypes.ByteCount -> PosixTypes.Fd -> IO String
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
      countByteCount <- Posix.fdWrite writeTo str
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
sendMsgRaw :: ChildProcess -> CString.CStringLen -> IO ()
sendMsgRaw (child@ChildProcess{writeTo = writeTo}) cStringLen =
   do
      if isDebug
         then
               do
                  str <- CString.peekCStringLen cStringLen
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

displayStdErr :: FilePath -> PosixTypes.Fd -> IO ()
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

readLine :: PosixTypes.Fd -> String -> IO String
-- Read a line from the Fd, and returns it without the final "\n".  
-- Raises error
-- if we get to EOF first or there is an IO error.  
-- The second argument should be "".  (It's an accumulating parameter)
readLine fd buf =
   do
      (inp,count) <- Posix.fdRead fd 1 -- at most one char
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
writeLine :: PosixTypes.Fd -> String -> IO ()
writeLine fd str =
   do
      countByteCount <- Posix.fdWrite fd msg
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

waitForInputFd :: PosixTypes.Fd -> IO()
waitForInputFd fd  = threadWaitRead(fromIntegral fd)

