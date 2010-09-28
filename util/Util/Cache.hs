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

import Util.DeprecatedFiniteMap
import Control.Concurrent

import Util.Computation

data Ord key =>
   Cache key elt = Cache (MVar(FiniteMap key (MVar elt))) (key -> IO elt)

newCache :: Ord key => (key -> IO elt) -> IO(Cache key elt)
newCache getAct =
   do
      cacheMVar <- newMVar emptyFM
      return (Cache cacheMVar getAct)

{- We do this in two stages so as not to hold up the whole
   cache at once. -}
getCached :: Ord key => Cache key elt -> key -> IO elt
getCached (Cache cacheMVar getAct) key =
   do
      cacheMap <- takeMVar cacheMVar
      case lookupFM cacheMap key of
         Just mVar ->
            do
               putMVar cacheMVar cacheMap
               readMVar mVar
         Nothing ->
            do
               mVar <- newEmptyMVar
               putMVar cacheMVar (addToFM cacheMap key mVar)
               value <- getAct key
               putMVar mVar value
               return value


