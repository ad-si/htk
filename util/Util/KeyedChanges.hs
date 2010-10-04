{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This implements a SinkSource with keyed changes.
module Util.KeyedChanges(
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

import qualified Data.Map as Map

import Util.Sources
import Util.Broadcaster

newtype KeyedChanges key delta
   = KeyedChanges (Broadcaster (Map.Map key delta) delta)

-- ------------------------------------------------------------------------
-- Producer's interface
-- ------------------------------------------------------------------------

newKeyedChanges :: Ord key => IO (KeyedChanges key delta)
newKeyedChanges =
   do
      broadcaster <- newBroadcaster Map.empty
      return (KeyedChanges broadcaster)

sendKeyedChanges :: Ord key => key -> delta -> KeyedChanges key delta -> IO ()
sendKeyedChanges key delta (KeyedChanges broadcaster) =
   applyUpdate broadcaster (\ map -> (Map.insert key delta map,[delta]))

deleteKeyedChange :: Ord key => key -> delta -> KeyedChanges key delta -> IO ()
deleteKeyedChange key delta (KeyedChanges broadcaster) =
   applyUpdate broadcaster (\ map -> case Map.lookup key map of
      Nothing -> (map,[])
      Just _ -> (Map.delete key map,[delta])
      )

-- ------------------------------------------------------------------------
-- Consumer's interface
-- ------------------------------------------------------------------------

instance Ord key => HasSource (KeyedChanges key delta) [delta] delta where
   toSource (KeyedChanges broadcaster) = map1 Map.elems (toSource broadcaster)
