{- Very primitive concurrency, this implements a sink, which passes messages
   along until the receiver is no longer interested. -}
module Sink(
   HasInvalidate(..),

   SinkID,
   newSinkID,

   Sink,
   newSink,
   newSinkGeneral,

   putSink,
   putSinkMultiple,
   coMapSink,
   coMapSink',
   coMapIOSink',

   CanAddSinks(..),
   ) where

import Monad

import Concurrent
import IOExts

import Computation
import Object

-- -------------------------------------------------------------------------
-- The HasInvalidate
-- -------------------------------------------------------------------------

---
-- The HasInvalidate class represents information sources which can be told
-- "No more, I'm not interested."
class HasInvalidate source where
   invalidate :: source -> IO ()

-- -------------------------------------------------------------------------
-- SinkID
-- -------------------------------------------------------------------------

--
-- A SinkID identifies the consumer and whether the consumer is still 
-- interested.
data SinkID = SinkID {
   oID :: ObjectID,
   interested :: IORef Bool
   }

newSinkID :: IO SinkID
newSinkID =
   do
      oID <- newObject
      interested <- newIORef True
      return (SinkID {
         oID = oID,
         interested = interested
         })

---
-- Returns True if sink is still interested
isInterested :: SinkID -> IO Bool
isInterested sinkID = readIORef (interested sinkID)

instance HasInvalidate SinkID where
   invalidate sinkID = writeIORef (interested sinkID) False

instance Eq SinkID where
   (==) sinkID1 sinkID2 = (oID sinkID1) == (oID sinkID2)

instance Ord SinkID where
   compare sinkID1 sinkID2 = compare (oID sinkID1) (oID sinkID2)


-- -------------------------------------------------------------------------
-- Sinks
-- -------------------------------------------------------------------------

data Sink x = Sink {
   sinkID :: SinkID,
   action :: x -> IO ()
   }

-- -------------------------------------------------------------------------
-- The consumer's interface
-- -------------------------------------------------------------------------

---
-- Creates a new sink with its own SinkID
newSink :: (x -> IO ()) -> IO (Sink x)
newSink action =
   do
      sinkID <- newSinkID
      newSinkGeneral sinkID action

---
-- Creates a new sink with a given SinkID.  This allows us to
-- invalidate lots of sinks just by invalidating one sinkID.
newSinkGeneral :: SinkID -> (x -> IO ()) -> IO (Sink x)
newSinkGeneral sinkID action = return (Sink {sinkID = sinkID,action = action})

---
-- Or we can do so with HasInvalidate
instance HasInvalidate (Sink x) where
   invalidate sink = invalidate (sinkID sink)

-- -------------------------------------------------------------------------
-- The provider's interface
-- -------------------------------------------------------------------------

---
-- Put a value into the sink, returning False if the sink id has been 
-- invalidated.
putSink :: Sink x -> x -> IO Bool
putSink sink x =
   do
      interested <- isInterested (sinkID sink)
      if interested then (action sink x) else done
      return interested
---
-- Put a list of values into the sink, returning False if the sink id has been 
-- invalidated
putSinkMultiple :: Sink x -> [x] -> IO Bool
putSinkMultiple sink [] = return True
putSinkMultiple sink (x:xs) =
   do
      interested <- putSink sink x
      if interested
         then 
            putSinkMultiple sink xs
         else
            return interested

---
-- Convert a sink from one type to another
coMapSink :: (y -> x) -> Sink x -> Sink y
coMapSink fn (Sink {sinkID = sinkID,action = action}) =
   Sink {sinkID = sinkID,action = action . fn}

---
-- Another version which allows a transformation function to filter
-- certain elements
coMapSink' :: (y -> Maybe x) -> Sink x -> Sink y
coMapSink' fn (Sink {sinkID = sinkID,action = action}) =
   let
      action' y = case fn y of
         Nothing -> done
         Just x -> action x
   in
      Sink {sinkID = sinkID,action = action'}

---
-- A version which allows an IO action, which had better not take too long.
coMapIOSink' :: (y -> IO (Maybe x)) -> Sink x -> Sink y
coMapIOSink' actFn (Sink {sinkID = sinkID,action = action}) =
    let
       action' y =
          do
             xOpt <- actFn y
             case xOpt of
                Nothing -> done
                Just x -> action x
    in
       Sink {sinkID = sinkID,action = action'}
                   
-- -------------------------------------------------------------------------
-- The CanAddSinks class.
-- -------------------------------------------------------------------------

---
-- A class for things (in particular Source and SimpleSource) that can
-- output via sinks.  Each sink source is supposed to have a unique
-- x, containing a representation of the current value, and delta,
-- containing the (incremental) updates which are put in the sink.
-- Only the addOrdSink function must be defined by instances.
class CanAddSinks sinkSource x delta | sinkSource -> x,sinkSource -> delta 
      where
   ---
   -- Create and add a new sink containing the given action.
   addNewSink :: sinkSource -> (delta -> IO ()) -> IO (x,Sink delta)
   addNewSink sinkSource action = addNewQuickSink sinkSource
      (\ delta ->
         do
            forkIO (action delta)
            done
         )

   ---
   -- Like addNewSink, but use the supplied SinkID
   addNewSinkGeneral :: sinkSource -> (delta -> IO ()) -> SinkID 
      -> IO (x,Sink delta)
   addNewSinkGeneral sinkSource action sinkID = 
      addNewQuickSinkGeneral 
         sinkSource 
         (\ delta ->
            do
               forkIO (action delta)
               done
            )
         sinkID

   ---
   -- Like addNewSink, but the action is guaranteed to terminate quickly
   -- and normally.
   addNewQuickSink :: sinkSource -> (delta -> IO ()) -> IO (x,Sink delta)
   addNewQuickSink sinkSource action =
      do
         sink <- newSink action
         x <- addOldSink sinkSource sink
         return (x,sink)

   ---
   -- Get contents without adding a sink
   readContents :: sinkSource -> IO x
   readContents sinkSource =
      do
         (x,sink) <- addNewQuickSink sinkSource (\ _ -> done)
         invalidate sink
         return x
     
   ---
   -- Like addNewQuickSink, but use the supplied SinkID
   addNewQuickSinkGeneral :: sinkSource -> (delta -> IO ()) -> SinkID 
      -> IO (x,Sink delta)
   addNewQuickSinkGeneral sinkSource action sinkID =
      do
         sink <- newSinkGeneral sinkID action
         x <- addOldSink sinkSource sink
         return (x,sink)

   ---
   -- Adds a pre-existing sink.
   addOldSink :: sinkSource -> Sink delta -> IO x

