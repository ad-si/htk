-- | InfoBus implements the 'shutdown' command.  This destroys all the
-- things registered via 'registerTool' and not
-- subsequently registered via 'deregisterTool'.  Tools are identified
-- by 'ObjectId'.
module Reactor.InfoBus (
   registerTool,
   registerToolDebug,
   deregisterTool,
   shutdown,
   registerDestroyAct,
   encapsulateWaitTermAct,
   ) where


import Control.Exception
import Control.Concurrent.MVar
import qualified Data.Map as Map
import System.IO.Unsafe
import System.Mem(performGC)

import Util.Computation
import Util.Object
import Util.Debug(debug)

import Events.Destructible

-- --------------------------------------------------------------------------
--  Tool Manager State
-- --------------------------------------------------------------------------

type ToolManager = MVar Tools

type Tools = Map.Map ObjectID (IO ())


-- --------------------------------------------------------------------------
--  Fetch Tool Manager State
-- --------------------------------------------------------------------------

toolmanager :: ToolManager
toolmanager = unsafePerformIO (newMVar Map.empty)
{-# NOINLINE toolmanager #-}


-- --------------------------------------------------------------------------
--  Client Commands
-- --------------------------------------------------------------------------

registerTool :: (Object t, Destroyable t) => t -> IO ()
registerTool t =
   do
      map <- takeMVar toolmanager
      putMVar toolmanager (Map.insert (objectID t) (destroy t) map)
      done

registerToolDebug :: (Object t, Destroyable t) => String -> t -> IO ()
registerToolDebug title t =
   do
      map <- takeMVar toolmanager
      putMVar toolmanager (Map.insert (objectID t) (destroy t) map)
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
            putMVar toolmanager (Map.delete oid map)
         ) :: IO (Either SomeException ())
      debug ("deregisterTool ",oid)
      done

shutdown :: IO ()
shutdown =
   do
      map <- takeMVar toolmanager
      let toShutDown = Map.toList map
      putMVar toolmanager Map.empty
      foreach toShutDown
         (\ (oid,cmd) ->
            do
               debug ("Shutting down ",oid)
               try cmd :: IO (Either SomeException ())
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
      _ <- registerDestroyAct (readMVar sync)
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






