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
   shutdown,
   registerDestroyAct,
   ) where


import Concurrent
import FiniteMap
import qualified IOExts(unsafePerformIO,performGC)

import Computation
import Dynamics
import Object
import Debug(debug)

import Destructible

-- --------------------------------------------------------------------------
--  Tool Manager State
-- --------------------------------------------------------------------------

type ToolManager = MVar Tools

type Tools = FiniteMap ObjectID (IO ())


-- --------------------------------------------------------------------------
--  Fetch Tool Manager State
-- --------------------------------------------------------------------------

toolmanager :: ToolManager
toolmanager = IOExts.unsafePerformIO (newMVar emptyFM)
{-# NOINLINE toolmanager #-}


-- --------------------------------------------------------------------------
--  Client Commands
-- --------------------------------------------------------------------------

registerTool :: (Object t, Destroyable t) => t -> IO ()
registerTool t = 
   do 
      map <- takeMVar toolmanager 
      putMVar toolmanager (addToFM map (objectID t) (destroy t))
      done          


deregisterTool :: (Object t) => t -> IO ()
deregisterTool t =  
   do 
      let oid = objectID t
      try( -- ignore exceptions if they occur.  I don't see how they can
           -- actually.
         do
            map <- takeMVar toolmanager
            putMVar toolmanager (delFromFM map oid)
         )
      done

shutdown :: IO ()
shutdown = 
   do
      map <- takeMVar toolmanager
      let cmds = eltsFM map
      putMVar toolmanager emptyFM
      foreach cmds (\cmd -> try cmd)
      IOExts.performGC

-- --------------------------------------------------------------------------
-- Simple interface allowing us to register something to be done without
-- having to create special instances for it.
-- --------------------------------------------------------------------------


---
-- register the given action to be done at shutdown.  The returned action
-- cancels the registration (without performing the given action).
registerDestroyAct :: IO () -> IO (IO ())
registerDestroyAct act =
   do
      oID <- newObject
      let
         simpleTool = SimpleTool {
            oID = oID,
            destroyAct = act
            }

      registerTool simpleTool
      return (deregisterTool simpleTool)

data SimpleTool = SimpleTool {
   oID :: ObjectID,
   destroyAct :: IO ()
   }

instance Object SimpleTool where
   objectID simpleTool = oID simpleTool

instance Destroyable SimpleTool where
   destroy simpleTool = destroyAct simpleTool






