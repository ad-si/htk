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
   standarderrors, -- :: Bool -> Config PosixProcess
   -- if standarderrors is true, we send stderr to the childprocesses
   -- out channel (of which there is only one).
   pollinterval,   -- :: Maybe Duration -> Config PosixProcess
   -- pollinterval is how often we attempt to get the status of the child
   -- process to see if it's finished yet.  Too long, and maybe the
   -- OS will have forgotten about the child process.
   -- If Nothing we won't check at all, which can be awkward; in particular
   -- it means we won't be able to get a destructible event when/if the
   -- tool closes.

   -- the settings encoded in (parms::[Config PosixProcess]) can be
   -- decoded, with a standard set of defaults, using
   -- Computation.configure defaultPosixProcess parms 
   defaultPosixProcess, -- :: PosixProcess
   
   newChildProcess,
   sendMsg,
   readMsg,
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

import Object
import qualified Posix
import Posix(ProcessID,ProcessStatus(..),Fd,fdRead,fdWrite)
import Concurrency
import Concurrent
import Interaction
import WatchDog
import Maybes
import ExtendedPrelude
import ThreadWait
import SIMClasses

import Debug(debug)

-- --------------------------------------------------------------------------
--  Posix Tool Parameters
-- --------------------------------------------------------------------------

data PosixProcess = 
   PosixProcess {
      args            :: [String], 
      env             :: Maybe [(String, String)],
      wdir            :: Maybe FilePath,
      lmode           :: Bool, -- line mode
      stderr          :: Bool, -- include stderr
      pinterval       :: Maybe Duration
     }

defaultPosixProcess :: PosixProcess
defaultPosixProcess = 
   PosixProcess {
      args = [], 
      env = Nothing, 
      wdir = Nothing, 
      lmode = True,
      stderr = True,
      pinterval = Just (secs 2)
      }


linemode        :: Bool -> Config PosixProcess
arguments       :: [String] -> Config PosixProcess
appendArguments :: [String] -> Config PosixProcess
environment     :: [(String,String)] -> Config PosixProcess
workingdir      :: FilePath -> Config PosixProcess
standarderrors  :: Bool -> Config PosixProcess
pollinterval    :: Maybe Duration -> Config PosixProcess

linemode lm' parms = return parms{lmode = lm'}
arguments args' parms = return parms{args = args'}
environment env' parms = return parms{env = Just env'}
workingdir wdir' parms = return parms{wdir = Just wdir'}
standarderrors err' parms = return parms{stderr = err'}
pollinterval err' parms = return parms{pinterval = err'}

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
      processID :: Posix.ProcessID,
                       -- process id of child
      watchStatus :: (Maybe (WatchDog ProcessStatus)), 
                       -- indicates exit code when process finishes
      toolStatus :: (PVar ToolStatus), 
                       -- ditto
      bufferVar :: (MVar String) 
                       -- bufferVar of previous characters (only relevant
                       -- for line mode)
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
      mprocessID <- Posix.forkProcess 
      connect writeIn readIn writeOut readOut mprocessID parms
   where
      -- We send an initial character over the Child Process output pipe
      -- before doing anything else.  I don't know why, but this
      -- seems to stop the first character of the child's output being
      -- lost.
      connect writeIn readIn writeOut readOut (Just processID) parms = 
         do -- parent process
            waitForInputFd readOut
            result <- fdRead readOut 1
            if (result /= ("#",1))
               then
                  raise(userError ("ChildProcess.newChildProcess bug 1"))
               else
                  done

            childObjectID <- newObject
            toolStatus <- newPVar Nothing
            watchStatus <- 
               terminationWatchDog (pinterval parms) toolStatus processID
            bufferVar <- newMVar ""

            Posix.fdClose readIn
            Posix.fdClose writeOut
            return (ChildProcess {
               childObjectID = childObjectID,
               lineMode = lmode parms,
               writeTo = writeIn,
               readFrom = readOut,
               processID = processID,
               watchStatus = watchStatus,
               toolStatus = toolStatus,
               bufferVar = bufferVar
               })
      connect writeIn readIn writeOut readOut Nothing parms =
         do -- child process
            Posix.dupTo readIn Posix.stdInput
            Posix.dupTo writeOut Posix.stdOutput
      
            nbytes <- Posix.fdWrite writeOut "#"
            if(nbytes/=1) 
               then
                  raise (userError ("ChildProcess.newChildProcess bug 2"))
               else
                  done

            when (stderr parms) (Posix.dupTo writeOut Posix.stdError)
            maybeChangeWd (wdir parms)
            Posix.executeFile path True (args parms) (env parms)
            raise (userError ("could not establish client: " ++ path))  
            
      
      maybeChangeWd Nothing = done
      maybeChangeWd (Just wd) = Posix.changeWorkingDirectory wd
      
      terminationWatchDog Nothing toolStatusPVar pid = 
         return Nothing
      terminationWatchDog (Just t) toolStatusPVar pid = 
         newWatchdog t (getStatus toolStatusPVar pid) >>= return . Just
                

getStatus :: PVar ToolStatus -> Posix.ProcessID -> IO ToolStatus
-- Immediately return Nothing if tool hasn't yet finished (or if no
-- watchdog is running for it and we didn't call getStatus in time), 
-- otherwise return Just (its exit status).
getStatus toolStatusPVar pid = 
   do
      ans <- try(Posix.getProcessStatus False True pid)
      -- translation: call waitpid on process specifying
      -- (1) set WNOHANG - don't block if status isn't available,
      --     instead return immediately with Nothing.
      -- (2) set WUNTRACED -- also report stopped
      --     childprocesses if they haven't previously
      --     been reported.
      case ans of
         Left e -> getVar toolStatusPVar
         -- Error.  Process must have terminated and disappeared.
         -- Use the last reported value (or Nothing if getStatus wasn't
         -- called in time).
         Right status ->
         -- success
            do
               setVar toolStatusPVar status 
               return status

-- --------------------------------------------------------------------------
--  Tool Instance
-- --------------------------------------------------------------------------

instance Object ChildProcess where
   objectID = childObjectID

instance Destructible ChildProcess where
   destroy child = 
      do
         res <- try(Posix.signalProcess Posix.sigKILL (processID child))
         case res of
            Left error -> 
               debug "ChildProcess.destroy failed; destruction anticipated?"
            _ -> return ()

   -- We can only wait for destruction if we set up a watchdog.
   destroyed child = 
      let
         Just watchDog = watchStatus child
      in 
         receive watchDog |>> done 


instance Tool ChildProcess where
   getToolStatus 
         (ChildProcess {processID = processID,toolStatus = toolStatus}) = 
      getStatus toolStatus processID


instance UnixTool ChildProcess where
   getUnixProcessID child = return(processID child)


-- -------------------------------------------------------------------------
-- Commands
-- -------------------------------------------------------------------------

{- line mode readMsg has been changed so it doesn't do a Posix.fdRead
   on every character. -}
readMsg :: ChildProcess -> IO String
readMsg (ChildProcess 
      {lineMode = True, readFrom = readFrom, bufferVar = bufferVar}) = 
   do
      buffer <- takeMVar bufferVar 
      (newBuffer,result) <- readWithBuffer readFrom buffer []
      putMVar bufferVar newBuffer
      return result
   where
      readWithBuffer readFrom [] acc = 
      -- we use an accumulating parameter since I don't want a
      -- non-tail-recursive action.
         do
            nextChunk <- readChunk readFrom
            readWithBuffer readFrom nextChunk acc
      readWithBuffer readFrom ('\n' : rest) acc =
         return (rest,reverse acc)
      readWithBuffer readFrom (other : rest) acc =
         readWithBuffer readFrom rest (other : acc)
readMsg (ChildProcess {lineMode = False,readFrom = readFrom}) = 
   readChunk readFrom

readChunk :: Fd -> IO String
-- read a chunk of characters, waiting until at least one is available.
readChunk fd =
   do
      waitForInputFd fd
      (input,count) <- Posix.fdRead fd 1000
      debug ("readChunk read " ++ input)
      if count <= 0 
         then 
            raise (userError "ChildProcess: input error")
         else
            return input


sendMsg :: ChildProcess -> String -> IO ()
sendMsg (ChildProcess{lineMode = True,writeTo = writeTo}) str  = 
   writeLine writeTo str
sendMsg (child @ (ChildProcess{lineMode = False,writeTo = writeTo})) str  = 
   do 
      debug("sendMsg sending " ++ str)
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


closeChildProcessFds  :: ChildProcess -> IO ()
closeChildProcessFds (ChildProcess{writeTo = writeTo,readFrom = readFrom}) = 
   do 
      Posix.fdClose writeTo
      Posix.fdClose readFrom

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
