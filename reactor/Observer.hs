{- #########################################################################

MODULE        : Observer
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Generic uncoupled Tool. Similar to the ChildProcess 
                tool except that it does not establish any pipes
                for the communication between the Haskell process and
                the child process.
                


   ######################################################################### -}


module Observer (
        ToolStatus,

        Destructible(..),
        Tool(..),

        PosixProcess(..),
        defaultPosixProcess,

        Observer,
        newObserver
        ) where

import Concurrency
import Interaction
import Object
import SIMClasses
import qualified Posix
import WatchDog
import ChildProcess(PosixProcess(..), defaultPosixProcess)
import Debug(debug)


-- --------------------------------------------------------------------------
-- Semantic Domains
-- --------------------------------------------------------------------------

data Observer   = 
        Observer 
                ObjectID
                Posix.ProcessID 
                (WatchDog ProcessStatus)
                (PVar ToolStatus)

                
-- --------------------------------------------------------------------------
--  Commands
-- --------------------------------------------------------------------------

newObserver :: FilePath -> [Config PosixProcess] -> IO Observer
newObserver tool confs = do {
        parms <- configure defaultPosixProcess confs;
        oid <- newObject;                                               
        tst <- newPVar Nothing; 
        pid <- startProcess tool (args parms) (env parms) (wdir parms); 
        d <- newWatchdog (secs 2) (getStatus tst pid);
        return (Observer oid pid d tst)
} where getStatus av pid = do {
                status <- Posix.getProcessStatus False True pid;
                case status of
                        Nothing -> return Nothing
                        s -> do {setVar av s; return s}
                }
                



-- --------------------------------------------------------------------------
--  Tool Instance
-- --------------------------------------------------------------------------

instance Object Observer where
        objectID (Observer oid _ _ _) = oid

instance Destructible Observer where
        destroy ctool @ (Observer _ pid _ sts) = 
                changeVar sts terminate
                where terminate Nothing = do {
                        Posix.signalProcess Posix.sigKILL pid;
                        return (Just (Posix.Terminated Posix.sigKILL))
                        }
                      terminate sts = return sts 
        destroyed (Observer _ _ d sts) = receive d |>> done 


instance Tool Observer where
        getToolStatus (Observer _ _ _ sts) = getVar sts


instance UnixTool Observer where
        getUnixProcessID (Observer _ pid _ _) = return pid


-- --------------------------------------------------------------------------
--  Foreign Process
-- --------------------------------------------------------------------------

startProcess :: FilePath                                -- Command
           -> [String]                                  -- Arguments
           -> Maybe [(String,String)]                   -- Environment
           -> Maybe FilePath                            -- Working Dir
           -> IO Posix.ProcessID
startProcess path args env wd  = do {
        pid <- Posix.forkProcess;
        case pid of
          (Just pid) -> return pid              -- parent process
          Nothing  -> do {                      -- child process
                maybeChangeWd wd;
                Posix.executeFile path True args env;
                raise (userError ("could not establish client: " ++ path)) 
                } 
        } where maybeChangeWd Nothing = done
                maybeChangeWd (Just wd) = Posix.changeWorkingDirectory wd



