{- #########################################################################

MODULE        : Dispatcher
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Dispatcher tool, i.e. a combination of the interpreter
                tool and the event-reactor tool.
                

   ######################################################################### -}


module Dispatcher (
        ToolStatus(..),

        Destructible(..),
        Tool(..),
        UnixTool(..),
        CommandTool(..),
        Adaptor(..),

        Dispatcher,
        newDispatcher,

        EventDesignator(..),
        EventID,

        commandFailed
        ) where

import Dynamics
import Concurrency
import Interaction
import FiniteMap
import ChildProcess
import Interpreter
import SIMClasses
import EventBroker
import Object
import Debug(debug)


-- --------------------------------------------------------------------------
-- Semantic Domains
-- --------------------------------------------------------------------------

data Typeable a => Dispatcher a = Dispatcher Interpreter (EventBroker a)

                
-- --------------------------------------------------------------------------
--  Dispatcher Interpreter
-- --------------------------------------------------------------------------

newDispatcher :: Typeable a =>  
        FilePath ->                                     -- toolname
        [Config PosixProcess] ->
        (Dispatcher a -> IO ()) ->                      -- finilizer
        (String -> Dispatcher a -> IO ()) ->            -- dispatcher
        IO (Dispatcher a)
newDispatcher tool confs fincmd dispatch = do {
        reactor <- newEventBroker;
        intrp <- newInterpreter 
                tool 
                confs
                (\ct -> fincmd (Dispatcher ct reactor))
                (\s -> \ct -> dispatch s (Dispatcher ct reactor));
        return (Dispatcher intrp reactor) 
} where wrap f pv = \ct -> f (Dispatcher ct pv)
        

-- --------------------------------------------------------------------------
--  Tool Instance
-- --------------------------------------------------------------------------

instance Object (Dispatcher a) where
        objectID (Dispatcher intrp _) = objectID intrp


instance Destructible (Dispatcher a) where
        destroy (Dispatcher intrp _) = destroy intrp
        destroyed (Dispatcher intrp _) = destroyed intrp
        

instance Tool (Dispatcher a) where
        getToolStatus (Dispatcher intrp _) = getToolStatus intrp


instance UnixTool (Dispatcher a) where
        getUnixProcessID (Dispatcher intrp _) = getUnixProcessID intrp  
        

instance CommandTool (Dispatcher a) where 
        execCmd cmd (Dispatcher intrp _) = execCmd cmd intrp
        evalCmd cmd (Dispatcher intrp _) = evalCmd cmd intrp        
        execOneWayCmd cmd (Dispatcher intrp _) = execOneWayCmd cmd intrp 


instance ReactiveCommandTool (Dispatcher a) where
        sendReply ans (Dispatcher intrp _) = sendReply ans intrp


instance Adaptor Dispatcher where
        register (Dispatcher _ reactor) ev dmode cmd iact =
                register reactor ev dmode cmd iact
        deregister (Dispatcher _ reactor) ev cmd iact =
                deregister reactor ev cmd iact
        dispatch (Dispatcher _ reactor) ev val  = 
                dispatch reactor ev val 
