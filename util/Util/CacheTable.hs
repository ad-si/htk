{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

-- | We implement in this module a general cache strategy for lookup
-- tables, using weak pointers.
module Util.CacheTable(
   -- Class describing table lookup operations
   LookupTable(..),
   -- Type which lifts up a table to include a cache.
   CachedTable,

   ) where

import System.Mem.Weak
import Util.DeprecatedFiniteMap
import Control.Concurrent

import Util.Computation(done)

-- -------------------------------------------------------------------
-- Class describing a lookup table
-- -------------------------------------------------------------------

class LookupTable table key value | table -> key,table -> value where
   openTable :: IO table
   putTable :: table -> value -> IO key
   getTable :: table -> key -> IO value
   flushTable :: table -> IO ()

-- -------------------------------------------------------------------
-- A primitive cache
-- -------------------------------------------------------------------

type Cache key value = MVar (FiniteMap key (Weak value))

-- Cache functions.  NB - these use lazy evaluation so that
-- the MVar will be tied up as little as possible.  This is particularly
-- important for deleteFromCache, which is invoked inside garbage collection

-- Create a new empty cache
newCache :: IO (Cache key value)
newCache = newMVar emptyFM

-- Adds a value to the cache, or if it's already there replaces it.
addToCache :: Ord key => Cache key value -> key -> value -> IO ()
addToCache cache key value =
   do
      weak <- mkWeakPtr value (Just (deleteFromCache cache key))
      map <- takeMVar cache
      let newMap = addToFM map key weak
      putMVar cache newMap
      -- we evaluate sizeFM to force the new map to be evaluated, so that
      -- hopefully old keys are flushed.
      seq (sizeFM newMap) done

-- deletes an item from the cache.  It should be harmless if the
-- value is already deleted.
deleteFromCache :: Ord key => Cache key value -> key -> IO ()
deleteFromCache cache key =
   do
      map <- takeMVar cache
      putMVar cache (delFromFM map key)

-- getFromCache retrieves a value from the cache.  If it hasn't been inserted,
-- it invokes the supplied action to get it, then inserts it.
-- The action may possibly get invoked more than once, if there are two
-- uses of getFromCache on the same key running at the same time.
getFromCache :: Ord key => Cache key value -> key -> IO value -> IO value
getFromCache cache key getValue =
   do
      map <- readMVar cache
      valueOpt <- case lookupFM map key of
         Nothing -> return Nothing
         Just weak -> deRefWeak weak
      case valueOpt of
         Nothing ->
            do
               value <- getValue
               addToCache cache key value
               return value
         Just value -> return value

-- -------------------------------------------------------------------
-- Adding a cache to a lookup table
-- -------------------------------------------------------------------

data CachedTable table key value = CachedTable {
   table :: table,
   cache :: Cache key value
   }

instance (LookupTable table key value,Ord key)
   => LookupTable (CachedTable table key value) key value where

   openTable =
      do
         table <- openTable
         cache <- newCache
         return (CachedTable {table = table,cache = cache})

   putTable (CachedTable {table = table,cache = cache}) value =
      do
         key <- putTable table value
         addToCache cache key value
         return key

   getTable (CachedTable {table = table,cache = cache}) key =
      getFromCache cache key (getTable table key)

   flushTable (CachedTable {table = table}) = flushTable table
