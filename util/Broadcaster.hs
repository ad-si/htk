{- A Broadcaster x delta is a cell containing a value x which is repeatedly
   changed by giving it values "delta" to be applied (with a function) to
    -}
module Broadcaster(
   Broadcaster,

   newBroadcaster,
   newGeneralBroadcaster,
   updateBroadcaster,
   anyUpdateBroadcaster,

   processClients,
   ) where

import Monad

import Concurrent
import FiniteMap

import Computation (done)
import Object
import Thread
import Sink

-- -------------------------------------------------------------------------
-- The Datatype
-- -------------------------------------------------------------------------

data Broadcaster x delta = Broadcaster {
   apply :: x -> delta -> Maybe x,
   -- If it returns Nothing, it means don't update, and don't broadcast
   -- the change.
   mVar :: MVar (x,[Sink delta])
   }


-- -------------------------------------------------------------------------
-- MVar-like operations for creating, updating and reading the MVar.
-- -------------------------------------------------------------------------

---
-- Make a new one.
newGeneralBroadcaster :: (x -> delta -> Maybe x) -> x 
   -> IO (Broadcaster x delta)
newGeneralBroadcaster apply x =
   do
      mVar <- newMVar (x,[])
      return (Broadcaster {
         apply = apply,
         mVar = mVar
         })

---
-- Like newGeneralBroadcaster, but apply function always updates.
newBroadcaster :: (x -> delta -> x) -> x -> IO (Broadcaster x delta)
newBroadcaster apply' =
   let
      apply x delta = Just (apply' x delta)
   in
      newGeneralBroadcaster apply


---
-- a general update function
updateBroadcaster :: Broadcaster x delta -> delta -> IO ()
updateBroadcaster (Broadcaster {apply = apply,mVar = mVar}) delta =
   do
      (x0,clients0) <- takeMVar mVar
      let 
         x1opt = apply x0 delta
      case x1opt of
         Nothing -> putMVar mVar (x0,clients0)
         Just x1 ->
            do
               clients1 <- processClients clients0 delta
               putMVar mVar (x1,clients1)

---
-- an even more general update function, which gives the caller complete
-- control over how x the value is changed and what deltas are sent.
anyUpdateBroadcaster :: Broadcaster x delta -> (x -> (x,[delta])) 
   -> IO ()
anyUpdateBroadcaster (Broadcaster {mVar = mVar}) updateFn =
   do
      (x0,clients0) <- takeMVar mVar
      let
         (x1,deltas) = updateFn x0
      clients1 <- processClientsMultiple clients0 deltas
      putMVar mVar (x1,clients1)

---
-- Utility function which sends a message to the list of clients,
-- returning those which have not already been invalidated.
processClients :: [Sink delta] -> delta -> IO [Sink delta]
processClients sinks0 delta =
   foldM
      (\ sinks1 sink ->
         do
            interested <- putSink sink delta
            return (if interested then sink:sinks1 else sinks1)
         )
      []
      sinks0
              

---
-- Like processClients but taking a list of deltas.
processClientsMultiple :: [Sink delta] -> [delta] -> IO [Sink delta]
processClientsMultiple sinks0 deltas =
   foldM
      (\ sinks1 sink ->
         do
            interested <- putSinkMultiple sink deltas
            return (if interested then sink:sinks1 else sinks1)
         )
      []
      sinks0
              
-- -------------------------------------------------------------------------
-- Adding sinks
-- -------------------------------------------------------------------------

instance CanAddSinks (Broadcaster x delta) x delta where
   addOldSink (Broadcaster{mVar = mVar}) sink =
      do
         (x,clients0) <- takeMVar mVar
         putMVar mVar (x,sink:clients0)
         return x

   readContents (Broadcaster {mVar = mVar}) =
      do
         (x,_) <- readMVar mVar
         return x



