-- | InfoBus implements the 'shutdown' command.  This destroys all the
-- things registered via 'registerTool' and not
-- subsequently registered via 'deregisterTool'.  Tools are identified
-- by 'ObjectId'.
module InfoBus (        
   registerTool,
   registerToolDebug,
   deregisterTool,   
   shutdown,
   registerDestroyAct,
   encapsulateWaitTermAct,
   ) where


import Control.Concurrent.MVar
import Data.FiniteMap
import System.IO.Unsafe
import System.Mem(performGC)

import Computation
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
toolmanager = unsafePerformIO (newMVar emptyFM)
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

registerToolDebug :: (Object t, Destroyable t) => String -> t -> IO ()
registerToolDebug title t = 
   do 
      map <- takeMVar toolmanager 
      putMVar toolmanager (addToFM map (objectID t) (destroy t))
      debug ("registerTool " ++ title,objectID t)
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
      debug ("deregisterTool ",oid)
      done

shutdown :: IO ()
shutdown = 
   do
      map <- takeMVar toolmanager
      let toShutDown = fmToList map
      putMVar toolmanager emptyFM
      foreach toShutDown 
         (\ (oid,cmd) -> 
            do
               debug ("Shutting down ",oid)
               try cmd
            )
      performGC

-- --------------------------------------------------------------------------
-- Simple interface allowing us to register something to be done without
-- having to create special instances for it.
-- --------------------------------------------------------------------------


-- | register the given action to be done at shutdown.  The returned action
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

-- | encapsulate an action such that shutdown waits for its termination
encapsulateWaitTermAct :: IO () -> IO ()
encapsulateWaitTermAct act =
   do sync <- newEmptyMVar
      registerDestroyAct (readMVar sync)
      act
      putMVar sync ()


data SimpleTool = SimpleTool {
   oID :: ObjectID,
   destroyAct :: IO ()
   }

instance Object SimpleTool where
   objectID simpleTool = oID simpleTool

instance Destroyable SimpleTool where
   destroy simpleTool = destroyAct simpleTool






