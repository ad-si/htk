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
   workingdir,     -- :: FilePath -> Config PosixProcess           
   chunksize,      -- :: Int-> Config PosixProcess
   -- the maximal size of one "chunk" of characters read from the 
   -- child process at one time (default 1000).
   standarderrors, -- :: Bool -> Config PosixProcess
   -- if standarderrors is true, we send stderr to the childprocesses
   -- out channel (of which there is only one).  Otherwise we
   -- display them, with the name of the responsible process,
   -- on our stderr.

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

import qualified IO
import qualified Posix
import Posix(ProcessID,ProcessStatus(..),Fd,fdRead,fdWrite)
import ByteArray
import qualified CString

import Computation
import Object
import Debug(debug,debugString)
import Thread
import FileNames

import Concurrent
import Maybes
import ExtendedPrelude

import Destructible

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
      stderr          :: Bool -- include stderr
     }

defaultPosixProcess :: PosixProcess
defaultPosixProcess = 
   PosixProcess {
      args = [], 
      env = Nothing, 
      wdir = Nothing, 
      chksize = 1000,
      lmode = True,
      stderr = True
      }


linemode        :: Bool -> Config PosixProcess
arguments       :: [String] -> Config PosixProcess
appendArguments :: [String] -> Config PosixProcess
environment     :: [(String,String)] -> Config PosixProcess
workingdir      :: FilePath -> Config PosixProcess
chunksize       :: Int-> Config PosixProcess
standarderrors  :: Bool -> Config PosixProcess

linemode lm' parms = return parms{lmode = lm'}
arguments args' parms = return parms{args = args'}
environment env' parms = return parms{env = Just env'}
workingdir wdir' parms = return parms{wdir = Just wdir'}
chunksize size' parms = return parms{chksize= size' }
standarderrors err' parms = return parms{stderr = err'}

appendArguments args' parms = return parms{args = (args parms) ++ args'}

-- -------------------------------------------------------------------------
-- Data Declaration
-- -------------------------------------------------------------------------

data ChildProcess = 
   ChildProcess {
      childObjectID :: ObjectID, 
      lineMode :: Bool,-- if True readMsg returns lines, otherwise
                       -- it returns the first input that's available.
      writeTo :: Fd,   -- to write to the process
      readFrom :: Fd,  -- to read from the process
      
      closeAction :: IO (), -- action when we closeChildProcessFd's.

      processID :: Posix.ProcessID,
                       -- process id of child
      bufferVar :: (MVar String),
                       -- bufferVar of previous characters (only relevant
                       -- for line mode)
#ifdef DEBUG
      toolTitle :: String,
                       -- Title of the tool, derived from the file name,
                       -- used in the debugging file.
#endif
      chunkSize :: Int -- max. size of one "chunk" of characters read
                       -- at one time
      }

-- -------------------------------------------------------------------------
-- Constructor
-- -------------------------------------------------------------------------

newChildProcess :: FilePath -> [Config PosixProcess] -> IO ChildProcess
newChildProcess path confs  =
   do                     -- (write,read)
      parms <- configure defaultPosixProcess confs

      debug("newChildProcess:")
      debug(path:(args parms))

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

      mprocessID <- Posix.forkProcess 
      connect writeIn readIn writeOut readOut readWriteErr mprocessID parms
   where
      -- We send an initial character over the Child Process output pipe
      -- before doing anything else.  I don't know why, but this
      -- seems to stop the first character of the child's output being
      -- lost.
      connect writeIn readIn writeOut readOut readWriteErr 
            (Just processID) parms = 
         do -- parent process
            waitForInputFd readOut
            result <- fdRead readOut 1
            if (result /= ("#",1))
               then
                  raise(userError ("ChildProcess.newChildProcess bug 1"))
               else
                  done

            childObjectID <- newObject
            bufferVar <- newMVar ""

            Posix.fdClose readIn
            Posix.fdClose writeOut
            -- Disable sigPIPE.  This means that the whole program
            -- won't crash when the tool exits.  Unfortunately there
            -- doesn't seem to be another way of doing this.
            Posix.installHandler Posix.sigPIPE Posix.Ignore Nothing

            closeAction <-
               case readWriteErr of
                  Nothing ->
                     return (
                        do
                           Posix.fdClose writeIn
                           Posix.fdClose readOut
                        )
                  Just (readErr,writeErr) ->
                     do
                        displayProcess <- 
                           forkIO (goesQuietly(displayStdErr path readErr))
                        return (
                           do
                              killThread displayProcess

                              Posix.fdClose readErr
                              Posix.fdClose writeIn
                              Posix.fdClose readOut
                           )
#ifdef DEBUG
            let
               toolTitle = case splitName path of
                  Just (dir,toolTitle) -> toolTitle
                  Nothing -> path
#endif
            return (ChildProcess {
               childObjectID = childObjectID,
               lineMode = lmode parms,
               writeTo = writeIn,
               readFrom = readOut,
               processID = processID,
               bufferVar = bufferVar,
	       chunkSize = chksize parms,
#ifdef DEBUG
               toolTitle = toolTitle,
#endif
               closeAction = closeAction
               })
      connect writeIn readIn writeOut readOut readWriteErr Nothing parms =
         do -- child process
            Posix.dupTo readIn Posix.stdInput
            Posix.dupTo writeOut Posix.stdOutput
            case readWriteErr of
               Nothing ->
                  Posix.dupTo writeOut Posix.stdError
               Just (_,writeErr) ->
                  Posix.dupTo writeErr Posix.stdError
 
            nbytes <- Posix.fdWrite writeOut "#"
            if(nbytes/=1) 
               then
                  raise (userError ("ChildProcess.newChildProcess bug 2"))
               else
                  done

            maybeChangeWd (wdir parms)
            Posix.executeFile path True (args parms) (env parms)
            raise (userError ("could not establish client: " ++ path))  
            
      
      maybeChangeWd Nothing = done
      maybeChangeWd (Just wd) = Posix.changeWorkingDirectory wd

getStatus :: Posix.ProcessID -> IO ToolStatus
-- Immediately return Nothing if tool hasn't yet finished, or if
-- it finished too long ago and the system has forgotten its
-- status; otherwise return Just (its exit status).
getStatus pid = 
   do
      ans <- try(Posix.getProcessStatus False True pid)
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
         res <- try(Posix.signalProcess Posix.sigKILL (processID child))
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
#ifdef DEBUG
      debugRead child (result++"\n")
#endif
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
#ifdef DEBUG
      debugRead child (result++"\n")
#endif
      return result

readChunk :: Int -> Fd -> IO String
-- read a chunk of characters, waiting until at least one is available.
readChunk size fd =
   do
      waitForInputFd fd
      (input,count) <- Posix.fdRead fd size
      if count <= 0 
         then 
            raise (userError "ChildProcess: input error")
         else
            return input

sendMsg :: ChildProcess -> String -> IO ()
sendMsg (child @ ChildProcess{lineMode = True,writeTo = writeTo}) str  = 
   do
      debugWrite child (str++"\n")
      writeLine writeTo str
sendMsg (child @ (ChildProcess{lineMode = False,writeTo = writeTo})) str  = 
   do 
      debugWrite child (str ++ "\n") 
      count <- Posix.fdWrite writeTo str
      -- see man -s 2 write for when write() returns 0.
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
#ifdef DEBUG
      str <- CString.peekCStringLen cStringLen
      debugWrite child str
#endif
      fdWritePrim writeTo cStringLen

closeChildProcessFds  :: ChildProcess -> IO ()
closeChildProcessFds (ChildProcess{closeAction = closeAction}) = 
   closeAction

-- -------------------------------------------------------------------------
-- Displaying stdErr output
-- -------------------------------------------------------------------------

displayStdErr :: FilePath -> Posix.Fd -> IO ()
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

readLine :: Posix.Fd -> String -> IO String
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
writeLine :: Posix.Fd -> String -> IO ()
writeLine fd str =
   do
      count <- Posix.fdWrite fd msg
      if count < 0
      -- see man -s 2 write for when write() returns 0.
         then
            raise writeLineError
         else if count /= (length msg) 
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

waitForInputFd :: Posix.Fd -> IO()
waitForInputFd fd  = threadWaitRead(Posix.fdToInt fd)

-- -------------------------------------------------------------------------
-- Writing debugging information to a file
-- -------------------------------------------------------------------------

-- NB - these functions expect a newline to be at the end of the string.
debugWrite :: ChildProcess -> String -> IO ()
debugRead :: ChildProcess -> String -> IO ()

#ifdef DEBUG

debugWrite childProcess str =
   debugString (toolTitle childProcess++">"++str)

debugRead childProcess str =
   debugString (toolTitle childProcess++"<"++str)

#else

debugWrite _ _ = done
debugRead _ _ = done

#endif
