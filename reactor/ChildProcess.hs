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

-- -------------------------------------------------------------------------
-- Data Declaration
-- -------------------------------------------------------------------------

data ChildProcess = 
   ChildProcess 
      ObjectID        
      Bool                                    -- line oriented?
      Fd                                      -- write end 
      Fd                                      -- read end
      Posix.ProcessID 
      (Maybe (WatchDog ProcessStatus))
      (PVar ToolStatus)


-- -------------------------------------------------------------------------
-- Constructor
-- -------------------------------------------------------------------------

newChildProcess :: FilePath -> [Config PosixProcess] -> IO ChildProcess
newChildProcess path confs  =
   do                     -- (write,read)
      parms <- configure defaultPosixProcess confs
      oid <- newObject
      (readIn,writeIn) <- Posix.createPipe 
      -- Pipe to send things to child
      (readOut,writeOut) <- Posix.createPipe 
      -- Pipe to read things back from child.
      mpid <- Posix.forkProcess 
      connect oid writeIn readIn writeOut readOut mpid parms
   where
      -- We send an initial character over the Child Process output pipe
      -- before doing anything else.  I don't know why, but this
      -- seems to stop the first character of the child's output being
      -- lost.
      connect oid writeIn readIn writeOut readOut (Just pid) parms = 
         do -- parent process
            waitForInputFd readOut
            result <- fdRead readOut 1
            if (result /= ("#",1))
               then
                  raise(userError ("ChildProcess.newChildProcess bug 1"))
               else
                  done

            toolStatusPVar <- newPVar Nothing
            d <- terminationWatchDog (pinterval parms) toolStatusPVar pid
            Posix.fdClose readIn
            Posix.fdClose writeOut
            return (ChildProcess oid (lmode parms) writeIn readOut pid d toolStatusPVar)
 
      connect oid writeIn readIn writeOut readOut Nothing parms =
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
   objectID (ChildProcess oid  _ _ _ _ _ _) = oid

instance Destructible ChildProcess where
   destroy (ChildProcess  _ _ _ _ pid _ _) = 
      Posix.signalProcess Posix.sigKILL pid
   -- we can only wait for destruction if we set up a watchdog.
   destroyed (ChildProcess _ _ _ _ _ (Just d) _) = receive d |>> done 
   destroyed _ = inaction


instance Tool ChildProcess where
   getToolStatus (ChildProcess _ _ _ _  pid _ toolStatusPVar) = getStatus toolStatusPVar pid


instance UnixTool ChildProcess where
   getUnixProcessID (ChildProcess _ _ _ _ pid _ _) = return pid 


-- -------------------------------------------------------------------------
-- Commands
-- -------------------------------------------------------------------------

-- Note on blocking and Posix.fdRead.  This is essentially equivalent
-- to the Posix read function.  This will block until at least 1
-- byte is available and then return all that are available up to
-- the user specified limit.  Since that blocks the entire process
-- (not just this thread) this means we should technically only
-- call Posix.fdRead if we already know there is stuff on the 
-- channel (e.g. via waitForInputFd).  readLine breaks this restriction
-- and so line-mode input can block everything if only half a line is written.
readMsg :: ChildProcess -> IO String
readMsg (ChildProcess _ True w r pid _ _) = 
-- return 1 line of input
   do 
      waitForInputFd r
      readLine r ""
readMsg (ChildProcess _ False w r pid _ _) = 
-- return next wodge of input, up to limit of 1000 chars.
   do 
      waitForInputFd r
      (inp,count) <- Posix.fdRead r 1000
      debug ("readMsg read " ++ inp)
      if count <= 0 
         then 
            raise (userError "ChildProcess: input error")
         else
            return inp                      

sendMsg :: ChildProcess -> String -> IO ()
sendMsg (ChildProcess _ True w r pid _ _) str  = writeLine w str
sendMsg (chp @ (ChildProcess _ False w r pid _ _)) str  = 
   do 
      debug("sendMsg sending " ++ str)
      count <- Posix.fdWrite w str
      -- see man -s 2 write for when write() returns 0.
      if count < 0 
         then
            raise writeLineError
         else if count < l then
            sendMsg chp (drop count str)
         else
            return ()
   where l = length str


closeChildProcessFds  :: ChildProcess -> IO ()
closeChildProcessFds (ChildProcess _ _ w r _ _ _) = 
   do 
      Posix.fdClose w
      Posix.fdClose r

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
