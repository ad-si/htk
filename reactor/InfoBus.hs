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


   ######################################################################### -}


module InfoBus (        
        InfoBus,
        interactionIB,
        dispatchIB,     

        registerTool,
        deregisterTool,
        shutdown

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

-- --------------------------------------------------------------------------
--  InfoBus
-- --------------------------------------------------------------------------

data InfoBus = InfoBus (EventBroker Dyn) ObjectID


interactionIB :: (EventDesignator e, Typeable a) => e -> DispatchMode -> IA a
interactionIB e m = interaction eid registerIB deregisterIB >>>= coerce
        where   eid = toEventID e
                registerIB   = register infobroker eid m done 
                deregisterIB = deregister infobroker eid done


dispatchIB :: (EventDesignator e, Typeable a) => e -> a -> IO ()
dispatchIB e v = dispatch infobroker e (toDyn v) done   
                

infobroker :: EventBroker Dyn
infobroker = let (InfoBus brk _) = infobus in brk

infobus :: InfoBus
infobus =  IOExts.unsafePerformIO (do {  
                brk <- newEventBroker;
                oid <- newObject;
                bus <- return (InfoBus brk oid);
                registerTool bus;
                return bus
                })


instance SingleInstanceTool InfoBus where
        getToolInstance = return infobus

instance Object InfoBus where
        objectID (InfoBus _ oid) = oid

instance EventDesignator (InfoBus,String) where
        toEventID (ib,str) = EventID (objectID ib) str

instance Destructible InfoBus where
        destroy ib   = dispatchIB (ib,"DESTROY") ()
        destroyed ib = interactionIB (ib,"DESTROY") Notice


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

