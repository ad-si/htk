{- #########################################################################

MODULE        : Dispatcher
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Dispatcher tool, i.e. a combination of the interpreter
                tool and the event-reactor tool.
This is a very loose-coupling indeed, since the interpreter and
event-reactor don't really talk to each other.  It's just a convenient
bundling of two separate things together.  See for example 
htk/kernel/GUIWish.hs for a use.
                

   ######################################################################### -}


module Dispatcher (
   ToolStatus,
   
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


data Typeable a => Dispatcher a = Dispatcher Interpreter (EventBroker a)
-- It has to be typeable to make the EventBroker
                
-- --------------------------------------------------------------------------
--  Dispatcher Interpreter
-- --------------------------------------------------------------------------

newDispatcher :: Typeable a =>  
   FilePath ->                  -- toolname
   [Config PosixProcess] ->     -- configurations
   (Dispatcher a -> IO ()) ->   -- finalizer
   (String -> Dispatcher a -> IO ()) ->  
                                -- dispatcher
   IO (Dispatcher a)
newDispatcher toolName confs finaliser dispatch = 
   do
      reactor <- newEventBroker
      interpreter <- 
         newInterpreter 
            toolName 
            confs
            (\ interpreter -> 
               finaliser (Dispatcher interpreter reactor))
            (\ str interpreter -> 
               dispatch str (Dispatcher interpreter reactor))
      return (Dispatcher interpreter reactor)   

-- --------------------------------------------------------------------------
--  Tool Instances.  These all trivially inherit from either the
--  interpreter or from the event broker.
-- --------------------------------------------------------------------------

instance Object (Dispatcher a) where
   objectID (Dispatcher interpreter _) = objectID interpreter

instance Destructible (Dispatcher a) where
   destroy (Dispatcher interpreter _) = destroy interpreter
   destroyed (Dispatcher interpreter _) = destroyed interpreter
        

instance Tool (Dispatcher a) where
    getToolStatus (Dispatcher interpreter _) = getToolStatus interpreter


instance UnixTool (Dispatcher a) where
    getUnixProcessID (Dispatcher interpreter _) = 
       getUnixProcessID interpreter  
        

instance CommandTool (Dispatcher a) where 
    evalCmd cmd (Dispatcher interpreter _) = evalCmd cmd interpreter        
    execOneWayCmd cmd (Dispatcher interpreter _) = 
       execOneWayCmd cmd interpreter 


instance ReactiveCommandTool (Dispatcher a) where
    sendReply ans (Dispatcher interpreter _) = sendReply ans interpreter


instance Adaptor Dispatcher where
    register (Dispatcher _ reactor) ev dmode cmd iact =
       register reactor ev dmode cmd iact
    deregister (Dispatcher _ reactor) ev cmd iact =
       deregister reactor ev cmd iact
    dispatch (Dispatcher _ reactor) ev val  = 
       dispatch reactor ev val 




