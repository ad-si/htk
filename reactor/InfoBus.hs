{- #########################################################################

MODULE        : InfoBus
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : 
   InfoBus implements the shutdown command.  This destroys all the
   things registered via registerTool and not
   subsequently registered via deregisterTool.  Tools are identified
   by ObjectId's.


   ######################################################################### -}


module InfoBus (        
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
registerTool t = 
   do 
      changeVar' toolmanager (\ts -> addToFM ts (objectID t) (destroy t))
      done          


deregisterTool :: (Object t) => t -> IO ()
deregisterTool t =  
   do 
      let oid = objectID t
      try( -- ignore exceptions if they occur.  I don't see how they can
           -- actually.
         updVar' 
            toolmanager 
            ( \ts -> 
               (delFromFM ts oid, lookupWithDefaultFM ts done oid))
         )
      done

shutdown :: IO ()
shutdown = 
   do
      cmds <- updVar' toolmanager (\ts -> (emptyFM, eltsFM ts))
      foreach cmds (\cmd -> try cmd)
      IOExts.performGC







