{- This implements a SinkSource with keyed changes. -}
module KeyedChanges(
   KeyedChanges,

   -- The idea is to keep track of changes to which a key is attached.
   -- When a new sink is attached, only the most recent changes for each
   -- key are returned.

   -- Producer's interface
   newKeyedChanges,
      -- :: Ord key => IO (KeyedChanges key delta)
   sendKeyedChanges,
      -- :: Ord key => key -> delta -> KeyedChanges key delta -> IO ()

   -- Used for sending changes which restore the situation to its default.
   -- If there is no entry for the key, nothing is done.  Otherwise the
   -- given delta is sent, and the entry is deleted.
   deleteKeyedChange,
      -- :: Ord key => key -> delta -> KeyedChanges key delta -> IO ()

   -- Consumer's interface
   -- instance Ord key => CanAddSinks (KeyedChanges key delta) [delta] delta
   ) where

import Concurrent
import FiniteMap

import Computation(done)
import qualified Broadcaster (processClients)
import Sink

newtype KeyedChanges key delta = 
   KeyedChanges (MVar (FiniteMap key delta,[Sink delta]))

-- ------------------------------------------------------------------------
-- Producer's interface
-- ------------------------------------------------------------------------

newKeyedChanges :: Ord key => IO (KeyedChanges key delta)
newKeyedChanges =
   do
      mVar <- newMVar (emptyFM,[])
      return (KeyedChanges mVar)

sendKeyedChanges :: Ord key => key -> delta -> KeyedChanges key delta -> IO ()
sendKeyedChanges key delta (KeyedChanges mVar) =
   do
      (map0,sinks0) <- takeMVar mVar
      let
         map1 = addToFM map0 key delta
      sinks1 <- Broadcaster.processClients sinks0 delta
      putMVar mVar (map1,sinks1)

      -- the seq hopefully avoids a long list of unevaluated thunks at the 
      -- front of the map.
      seq (sizeFM map1) done 

deleteKeyedChange :: Ord key => key -> delta -> KeyedChanges key delta -> IO ()
deleteKeyedChange key delta (KeyedChanges mVar) =
   do
      (contents @ (map0,sinks0)) <- takeMVar mVar
      case lookupFM map0 key of
         Nothing -> putMVar mVar contents
         Just _ ->
            do
               sinks1 <- Broadcaster.processClients sinks0 delta
               let
                  map1 = delFromFM map0 key
               putMVar mVar (map1,sinks1)
               seq (sizeFM map1) done

-- ------------------------------------------------------------------------
-- Consumer's interface
-- ------------------------------------------------------------------------

instance Ord key => CanAddSinks (KeyedChanges key delta) [delta] delta where
   addOldSink (KeyedChanges mVar) sink =
      do
         (map0,sinks0) <- takeMVar mVar         
         putMVar mVar (map0,sink:sinks0)
         return (eltsFM map0)
   readContents (KeyedChanges mVar) =
      do
         (map0,_) <- readMVar mVar
         return (eltsFM map0)
