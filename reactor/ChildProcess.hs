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
        arguments,      -- :: [String] -> Config PosixProcess           
        environment,    -- :: [(String,String)] -> Config PosixProcess  
        workingdir,     -- :: FilePath -> Config PosixProcess           
        standarderrors, -- :: Bool -> Config PosixProcess
        pollinterval,   -- :: Maybe Duration -> Config PosixProcess
        -- the settings encoded in (parms::[Config PosixProcess]) can be
        -- decoded, with a standard set of defaults, using
        -- Computation.configure defaultPosixProcess parms 
        defaultPosixProcess, -- :: PosixProcess

        newChildProcess,
        sendMsg,
        readMsg,
        closeChildProcessFds,

        ProcessStatus(..),
        ToolStatus(..),
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
                lmode           :: Bool,                -- line mode
                stderr          :: Bool,                -- include stderr
                pinterval       :: Maybe Duration
                }

defaultPosixProcess :: PosixProcess
defaultPosixProcess = 
        PosixProcess {args = [], 
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
      (r1,w1) <- Posix.createPipe   -- in
      (r2,w2) <- Posix.createPipe  -- out
      mpid <- Posix.forkProcess 
      av <- newPVar Nothing
      connect oid w1 r1 w2 r2 av mpid parms
   where
      connect oid w1 r1 w2 r2 av (Just pid) parms = 
         do -- parent process
--                Posix.fdClose r1;
--                Posix.fdClose w2;

            waitForInputFd r2
            result <- fdRead r2 1
            if (result /= ("#",1))
               then
                  raise(userError ("Posix.fdRead mystery"))
               else
                  done

            d <- terminationWatchDog (pinterval parms) av pid
            return (ChildProcess oid (lmode parms) w1 r2 pid d av)
 
      connect oid w1 r1 w2 r2 _ Nothing parms =
         do -- child process
            setLogFile Nothing;
--              Posix.fdClose w1;
--              Posix.fdClose r2        ;
            Posix.dupTo r1 Posix.stdInput
            Posix.dupTo w2 Posix.stdOutput
      
            nbytes <- Posix.fdWrite w2 "#"
            if(nbytes/=1) 
               then
                  raise (userError ("Posix.fdWrite mystery"))
               else
                  done
            when (stderr parms) (Posix.dupTo w2 Posix.stdError)
            maybeChangeWd (wdir parms)
            Posix.executeFile path True (args parms) (env parms)
            raise (userError ("could not establish client: " ++ path))  
            
      
      maybeChangeWd Nothing = done
      maybeChangeWd (Just wd) = Posix.changeWorkingDirectory wd
      
      terminationWatchDog Nothing av pid = 
         return Nothing
      terminationWatchDog (Just t) av pid = 
         newWatchdog t (getStatus av pid) >>= return . Just
                

getStatus av pid = do {
        ans <- try(Posix.getProcessStatus False True pid);
        case ans of
                Left e -> getVar av 
                (Right status) -> do {
                        setVar av status; 
                        return status
                        }
}
                

-- --------------------------------------------------------------------------
--  Tool Instance
-- --------------------------------------------------------------------------

instance Object ChildProcess where
        objectID (ChildProcess oid  _ _ _ _ _ _) = oid


instance Destructible ChildProcess where
        destroy (ChildProcess  _ _ wp rp pid _ _) = 
                Posix.signalProcess Posix.sigKILL pid
        destroyed (ChildProcess _ _ wp rp pid (Just d) av) = receive d |>> done 
        destroyed _ = inaction


instance Tool ChildProcess where
        getToolStatus (ChildProcess _ _ wp rp pid _ av) = getStatus av pid


instance UnixTool ChildProcess where
        getUnixProcessID (ChildProcess _ _ wp rp pid _ _) = return pid 


-- -------------------------------------------------------------------------
-- Commands
-- -------------------------------------------------------------------------

readMsg :: ChildProcess -> IO String
readMsg (ChildProcess _ True w r pid _ _) = 
   do 
      waitForInputFd r
      readLine r ""
readMsg (ChildProcess _ False w r pid _ _) = 
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
      if count < 0 
         then
            raise writeLineError
         else if count < l then
            sendMsg chp (drop count str)
         else
            return ()
   where l = length str




closeChildProcessFds  :: ChildProcess -> IO ()
closeChildProcessFds (ChildProcess _ _ w r _ _ _) = do {
        Posix.fdClose w;
        Posix.fdClose r;
        }



-- -------------------------------------------------------------------------
-- Reading and Writing Lines from Channels
-- -------------------------------------------------------------------------

readLine :: Posix.Fd -> String -> IO String
readLine c buf =
        Posix.fdRead c 1                                        >>= \ (inp,count) ->
        if count /= 1 then 
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

           next x = readLine c (x ++ buf)

writeLine :: Posix.Fd -> String -> IO ()
writeLine c str =
        Posix.fdWrite c msg                             >>= \ count ->
        if count < 0 then
                raise writeLineError
        else if count /= (length msg) then
                writeLine c (drop count str)
        else
                return ()
        where msg = str ++ "\n" 


readLineError :: IOError
readLineError = userError "read line error"

writeLineError :: IOError
writeLineError = userError "write line error"
