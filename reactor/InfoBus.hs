{- #########################################################################

MODULE        : InfoBus
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : The InfoBus provides services for registering
                and unregistering tools/adaptors.
 
                Each registered tool provide a cleanup command with the
                execution manager which will be invoked when the tool
                unregisters.

                Communication is done using a single global 
                EventBroker called "infobus".  This is not exported to
                anywhere.
   ######################################################################### -}


module InfoBus (        
   registerTool,
   deregisterTool,
   shutdown
  
-- The following names don't seem to be used anywhere by anything outside
-- this file. 
--   InfoBus,
--   interactionIB,
--   dispatchIB,     
   
   ) where


import Concurrency
import Dynamics
import EventBroker
import Object
import FiniteMap
import SIMClasses
import Interaction(EventID(..),interaction,EventDesignator(..),IA,EventListener(..))
import qualified IOExts(unsafePerformIO,performGC)
import Debug(debug)

#if 0

-- --------------------------------------------------------------------------
--  InfoBus
-- --------------------------------------------------------------------------

data InfoBus = InfoBus (EventBroker Dyn) ObjectID

interactionIB :: (EventDesignator e, Typeable a) => e -> DispatchMode -> IA a
interactionIB eventId mode = 
   interaction eId registerIB deregisterIB >>>= coerce
   -- Create an IA for some type of event going into the infobus. 
   where   
      eId = toEventID eventId
      registerIB   = register infobroker eventId mode done 
      deregisterIB = deregister infobroker eventId done


dispatchIB :: (EventDesignator e, Typeable a) => e -> a -> IO ()
dispatchIB eventId value = dispatch infobroker eventId (toDyn value) done   
   -- feed something into the infobus.                

infobroker :: EventBroker Dyn
infobroker = let (InfoBus brk _) = infobus in brk
   -- the EventBroker in the global InfoBus

infobus :: InfoBus
   -- the global infobus.
infobus =  
   IOExts.unsafePerformIO (
      do  
         broker <- newEventBroker
         oid <- newObject
         bus <- return (InfoBus broker oid)
         registerTool bus
         return bus
      )


instance SingleInstanceTool InfoBus where
   -- as far as I can discover, this isn't used anywhere.
   getToolInstance = return infobus

instance Object InfoBus where
   objectID (InfoBus _ oid) = oid

instance EventDesignator (InfoBus,String) where
   toEventID (ib,str) = EventID (objectID ib) str

instance Destructible InfoBus where
   destroy ib   = dispatchIB (ib,"DESTROY") ()
   destroyed ib = interactionIB (ib,"DESTROY") Notice

#endif

-- --------------------------------------------------------------------------
--  Tool Manager State
-- --------------------------------------------------------------------------

type ToolManager = PVar Tools

type Tools = FiniteMap ObjectID (IO ())


-- --------------------------------------------------------------------------
--  Fetch Tool Manager State
-- --------------------------------------------------------------------------

toolmanager :: ToolManager
toolmanager = IOExts.unsafePerformIO (newPVar emptyFM)


-- --------------------------------------------------------------------------
--  Client Commands
-- --------------------------------------------------------------------------

registerTool :: (Object t, Destructible t) => t -> IO ()
registerTool t = do 
        changeVar' toolmanager (\ts -> addToFM ts (objectID t) (destroy t))
        done          


deregisterTool :: (Object t) => t -> IO ()
deregisterTool t =  do 
        oid <- return (objectID t)
        cmd <- updVar' toolmanager (\ts -> 
                (delFromFM ts oid, lookupWithDefaultFM ts done oid))
        try cmd
        done


shutdown :: IO ()
shutdown = do
        cmds <- updVar' toolmanager (\ts -> (emptyFM, eltsFM ts))
        foreach cmds (\cmd -> try cmd)
        IOExts.performGC







