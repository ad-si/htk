{- #########################################################################

MODULE        : Interpreter
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Generic, loosely coupled interpreter tool.
                


   ######################################################################### -}


module Interpreter (
        ToolStatus(..),

        Destructible(..),
        Tool(..),
        CommandTool(..),

        Interpreter,
        newInterpreter,

        commandFailed
        ) where

import Concurrency
import ChildProcess
import SIMClasses
import Object
import Debug(debug)

-- --------------------------------------------------------------------------
-- Semantic Domains
-- --------------------------------------------------------------------------

data Interpreter        = 
        Interpreter 
                ObjectID
                ReplyAddr
                ChildProcess
                (Interpreter -> IO ())          -- finalizer command
                (PVar ())

type ReplyAddr  = MVar (Answer String)  -- mvar of blocking client

                
-- --------------------------------------------------------------------------
--  Interpreter Interpreter
-- --------------------------------------------------------------------------

newInterpreter :: 
        FilePath ->                                     -- toolname
        [Config PosixProcess] ->
        (Interpreter -> IO ()) ->                       -- finilizer
        (String -> Interpreter -> IO ()) ->             -- handler
        IO Interpreter
newInterpreter tool confs fincmd handler = do {
        av <- newEmptyMVar;
        oid <- newObject;
        mtx <- newPVar ();                                              
        child <- newChildProcess tool confs;
        intrp <- return (Interpreter oid av child fincmd mtx);
        forkIO(dispatcher intrp handler);
        return intrp
        }


-- --------------------------------------------------------------------------
--  Tool Instance
-- --------------------------------------------------------------------------

instance Object Interpreter where
        objectID (Interpreter oid _ _ _ _) = oid


instance Destructible Interpreter where
        destroy ctool @ (Interpreter _ av _ fincmd _) = fincmd ctool
        destroyed (Interpreter _ _ t _ _) = destroyed t >>> done


instance Tool Interpreter where
        getToolStatus (Interpreter _ _ t _ _) = getToolStatus t


instance UnixTool Interpreter where
        getUnixProcessID (Interpreter _ _ t _ _) = getUnixProcessID t



-- --------------------------------------------------------------------------
--  Command Tool Instance
-- --------------------------------------------------------------------------

instance CommandTool Interpreter where 
        execCmd cmd t = do {evalCmd cmd t; done}
        evalCmd cmd (Interpreter oid av t _ pv) = withVar pv (\() -> do {
                writeLog (">>" ++ cmd);
                sendMsg t cmd;
                res <- takeMVar av;
                propagate res
                })                              
        execOneWayCmd cmd (Interpreter _ av t _ pv) = withVar pv (\() -> do {
                writeLog (">>" ++ cmd);
                sendMsg t cmd
                })


-- --------------------------------------------------------------------------
--  Reactive Command Tool Instance
-- --------------------------------------------------------------------------

instance ReactiveCommandTool Interpreter where
        sendReply ans (Interpreter _ av _ _ _) = putMVar av ans 


-- --------------------------------------------------------------------------
-- IOError
-- --------------------------------------------------------------------------

commandFailed :: IOError
commandFailed = userError "command failed with exception"


-- --------------------------------------------------------------------------
-- Event Dispatching 
-- --------------------------------------------------------------------------

dispatcher :: Interpreter -> (String -> Interpreter -> IO ()) -> IO ()
dispatcher intrp @ (Interpreter _ _ child _ _) handler = do {
        msg <- readMsg child;
        writeLog ("<<" ++ msg);
        handler msg intrp;
        dispatcher intrp handler
        }

