-- | This implements a SinkSource with keyed changes. 
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
   -- instance Ord key => HasSource (KeyedChanges key delta) [delta] delta
   ) where

import Control.Concurrent
import Data.FiniteMap

import Computation(done)

import Sources
import Broadcaster

newtype KeyedChanges key delta 
   = KeyedChanges (Broadcaster (FiniteMap key delta) delta)

-- ------------------------------------------------------------------------
-- Producer's interface
-- ------------------------------------------------------------------------

newKeyedChanges :: Ord key => IO (KeyedChanges key delta)
newKeyedChanges =
   do
      broadcaster <- newBroadcaster emptyFM
      return (KeyedChanges broadcaster)

sendKeyedChanges :: Ord key => key -> delta -> KeyedChanges key delta -> IO ()
sendKeyedChanges key delta (KeyedChanges broadcaster) =
   applyUpdate broadcaster (\ map -> (addToFM map key delta,[delta]))

deleteKeyedChange :: Ord key => key -> delta -> KeyedChanges key delta -> IO ()
deleteKeyedChange key delta (KeyedChanges broadcaster) =
   applyUpdate broadcaster (\ map -> case lookupFM map key of
      Nothing -> (map,[])
      Just _ -> (delFromFM map key,[delta])
      )

-- ------------------------------------------------------------------------
-- Consumer's interface
-- ------------------------------------------------------------------------

instance Ord key => HasSource (KeyedChanges key delta) [delta] delta where
   toSource (KeyedChanges broadcaster) = map1 eltsFM (toSource broadcaster)
