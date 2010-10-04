-- | The Cache module allows us to cache results of expensive stateful
-- computations in memory.
-- Possible improvements -
--    (1) use hashing instead
module Util.Cache(
   Cache,    -- a cache (a stateful object).  Takes parameters key and elt.
             -- key must be an instance of Ord.
   newCache, -- :: Ord key => (key -> IO elt) -> IO(Cache key elt)
   getCached -- :: Ord key => Cache key elt -> key -> IO elt
   ) where

import qualified Data.Map as Map
import Control.Concurrent

data Ord key =>
   Cache key elt = Cache (MVar(Map.Map key (MVar elt))) (key -> IO elt)

newCache :: Ord key => (key -> IO elt) -> IO(Cache key elt)
newCache getAct =
   do
      cacheMVar <- newMVar Map.empty
      return (Cache cacheMVar getAct)

{- We do this in two stages so as not to hold up the whole
   cache at once. -}
getCached :: Ord key => Cache key elt -> key -> IO elt
getCached (Cache cacheMVar getAct) key =
   do
      cacheMap <- takeMVar cacheMVar
      case Map.lookup key cacheMap of
         Just mVar ->
            do
               putMVar cacheMVar cacheMap
               readMVar mVar
         Nothing ->
            do
               mVar <- newEmptyMVar
               putMVar cacheMVar (Map.insert key mVar cacheMap)
               value <- getAct key
               putMVar mVar value
               return value


