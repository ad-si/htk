{- PrimGuardedChannels implements PrimEvent's as defined in EventClasses,
   given instances of the classes in GuardBasics.
   -}
module PrimGuardedChannels(
   HasReceivePrim(..),
   HasSendPrim(..),

   GuardedChannel,
      -- parameterised on the value of the channel,
      --  
      -- instance of HasReceivePrim, HasSendPrim
   newGuardedChannel, -- :: IO GuardedChannel
   ) where

import IO

import Concurrent

import Computation (done)

import Toggle

import EventClasses
import GuardBasics

-- ----------------------------------------------------------------------
-- Classes Channels must implement.
-- We will implement these based on instances of the classes in GuardBasics.
--
-- "sneakPrim" and "replacePrim" were not in Einar's original channels,
-- but correspond to multiple operations on the same channel; they do however
-- have the advantage that no other operations can be performed on channel
-- inbetween.  An obvious generalisation would be
-- updatePrim :: channel -> guard -> (value -> IO value) -> 
--   PrimEvent value
-- but we bar this because we don't want to risk tying up the channel while
-- the IO action is performed.
-- ----------------------------------------------------------------------

class HasReceivePrim channel value guard where
   receivePrim :: channel -> guard -> PrimEvent value
   -- event corresponding to listening on a channel with a particular guard
   -- and taking the value from it when we get it.

   -- sneakPrim and replacePrim both return immediately and will return
   -- Nothing and do nothing should there be no value already in the
   -- channel.
   sneakPrim :: channel -> guard -> PrimEvent (Maybe value)
   -- sneakPrim is like receivePrim, but immediately puts the value back
   -- in the channel so it doesn't remain empty.
   replacePrim :: channel -> guard -> value 
      -> PrimEvent (Maybe value)
   -- replacePrim channel value
   -- corresponds to receiving a value from the channel with a null guard
   -- (the value is returned) followed by immediately sending the same value.

class HasSendPrim channel value where
   sendPrim :: channel -> value -> PrimEvent ()
   -- event corresponding to acknowledgement of the supplied value.

-- ---------------------------------------------------------------
-- Datatypes and Creation
-- We put a number of strictness annotations here, in the hope that
-- it'll help the speed.
-- ---------------------------------------------------------------

data OneGuard value = OneGuard !Toggle !(value -> IO ())

data OneValue = OneValue !Toggle !(() -> IO ())

type Guards guardQueue value = guardQueue (OneGuard value)

type Values valueQueue = valueQueue OneValue

data Contents value guardQueue valueQueue = 
   Contents !(Guards guardQueue value) !(Values valueQueue)

newtype (AddValueQueue valueQueue value,GuardQueue guardQueue value)
   => GuardedChannel value guardQueue valueQueue =
   GuardedChannel (MVar (Contents value guardQueue valueQueue)) 
-- Concurrency Notes.  An MVar is "locked" by a thread when that
-- thread has emptied the MVar and not yet put anything back.
-- We obey the following rules:
-- (1) No thread puts anything into a channel MVar unless it has locked
--     that MVar.
-- (2) No thread may own locks to MVars in more than one channel at a time.

lockChannel :: GuardedChannel value guardQueue valueQueue 
   -> IO (Guards guardQueue value,Values valueQueue)
lockChannel (GuardedChannel mVar) =
   do
      Contents guardQueue valueQueue <- takeMVar mVar
      return (guardQueue,valueQueue)
{-# INLINE lockChannel #-}

unlockChannel :: GuardedChannel value guardQueue valueQueue 
   -> Guards guardQueue value -> Values valueQueue -> IO ()
unlockChannel (GuardedChannel mVar) guardQueue valueQueue =
   putMVar mVar (Contents guardQueue valueQueue)
{-# INLINE unlockChannel #-}

newGuardedChannel :: (HasEmptyQueue guardQueue,HasEmptyQueue valueQueue,
      AddValueQueue valueQueue value,GuardQueue guardQueue value)
   => IO (GuardedChannel value guardQueue valueQueue)
newGuardedChannel =
   do
      mVar <- newMVar (Contents emptyQueue emptyQueue)
      return (GuardedChannel mVar)

-- ---------------------------------------------------------------
-- Instances for primitive events.
-- ---------------------------------------------------------------

-- newtype PrimEvent a = PrimEvent (Toggle -> (a -> IO ()) -> IO Result)
-- data Result = Immediate  | Awaiting (IO ())

instance (ValueQueue valueQueue guard value,AddGuardQueue guardQueue guard,
   GuardQueue guardQueue value)
   => HasReceivePrim (GuardedChannel value guardQueue valueQueue) value guard 
      where
   receivePrim channel guard = synthesise channel (pureReceive guard)
   sneakPrim channel guard = synthesise channel (pureSneak id guard)
   replacePrim channel guard newValue = 
      synthesise channel (pureSneak (const newValue) guard)

instance (AddValueQueue valueQueue value,GuardQueue guardQueue value)
   => HasSendPrim (GuardedChannel value guardQueue valueQueue) value where
   sendPrim channel value = synthesise channel (pureSend value)

-- ---------------------------------------------------------------
-- The basic functions
-- The pureXXX functions take a (guardQueue,valueQueue) and update
-- it in some way; hence the channel is locked throughout for each 
-- of them.
-- synthesise and channelOp are used to turn the pureXXX functions and
-- actual PrimEvent's.
-- ---------------------------------------------------------------

synthesise ::
   GuardedChannel value guardQueue valueQueue
   -> (Toggle -> (valueOut -> IO ()) 
      -> Guards guardQueue value -> Values valueQueue 
      -> IO (Guards guardQueue value,Values valueQueue,Result)
      )
   -> PrimEvent valueOut
synthesise channel pureOp =
   PrimEvent (
      \ toggle continuation ->
         channelOp channel (pureOp toggle continuation)
         )

channelOp :: 
   GuardedChannel value guardQueue valueQueue 
   -> (Guards guardQueue value -> Values valueQueue
      -> IO (Guards guardQueue value,Values valueQueue,Result)
         )
   -> IO Result
channelOp guardedChannel toDo =
   do
      (guardQueue,valueQueue) <- lockChannel guardedChannel
      result' <- try (toDo guardQueue valueQueue)
      case result' of
         Left error ->
            do
               unlockChannel guardedChannel guardQueue valueQueue
               ioError error
         Right (guardQueue,valueQueue,result) ->
            do
               unlockChannel guardedChannel guardQueue valueQueue
               return result
{-# INLINE channelOp #-}

pureSend :: (GuardQueue guardQueue value,AddValueQueue valueQueue value)
   => value -> Toggle -> (() -> IO ()) 
   -> Guards guardQueue value -> Values valueQueue
   -> IO (Guards guardQueue value,Values valueQueue,Result)
pureSend value tog1 continuation1 guardQueue valueQueue =
   do
      let
         isNeeded = lookupValue guardQueue value
      case isNeeded of
         Just (OneGuard tog2 continuation2,guardQueue2) ->
            do
               acquired <- toggle2 tog1 tog2
               case acquired of
                  Nothing -> -- we're in business
                     do
                        continuation2 value
                        continuation1 ()
                        return (guardQueue2,valueQueue,Immediate)
                  Just (True,False) -> 
                     -- second one is already done.  Repeat
                     pureSend value tog1 continuation1 guardQueue2 valueQueue
                  Just (False,True) ->
                     -- Someone got to this event before we did.  Return
                     -- original guardQueue.
                     return (guardQueue,valueQueue,Immediate)
                  Just (False,False) ->
                     -- both of the above.
                     return (guardQueue2,valueQueue,Immediate)
         Nothing ->
            -- no-one is waiting for a value right now, so queue it.
            do
               (valueQueue2,invalidateAct) <- addValue valueQueue value
                  (OneValue tog1 continuation1)              
               return (guardQueue,valueQueue2,Awaiting invalidateAct)    

pureReceive :: 
   (ValueQueue valueQueue guard value,AddGuardQueue guardQueue guard)
   => guard -> Toggle -> (value -> IO ()) 
   -> Guards guardQueue value -> Values valueQueue
   -> IO (Guards guardQueue value,Values valueQueue,Result)
pureReceive guard tog1 continuation1 guardQueue valueQueue =
   do
      let
         isNeeded = lookupGuard valueQueue guard 
      case isNeeded of
         Just (OneValue tog2 continuation2,value,valueQueue2) ->
            do
               acquired <- toggle2 tog1 tog2
               -- for comments see pureSend, which is almost identical to
               -- this function.  We annotate differences
               case acquired of
                  Nothing ->
                     do
                        -- it shouldn't make any difference, but seems slightly
                        -- more sensible to always have the receiver 
                        -- continuation before the sender continuation.
                        continuation1 value
                        continuation2 ()
                        return (guardQueue,valueQueue2,Immediate)
                  Just (True,False) ->
                     pureReceive guard tog1 continuation1 guardQueue valueQueue
                  Just (False,True) ->
                     return (guardQueue,valueQueue,Immediate)
                  Just (False,False) ->
                     return (guardQueue,valueQueue2,Immediate)
         Nothing -> 
            -- no value is available, so queue the guard
            do
               (guardQueue2,invalidateAct) <- addGuard guardQueue guard
                  (OneGuard tog1 continuation1)
               return (guardQueue2,valueQueue,Awaiting invalidateAct)

pureSneak :: 
   (ValueQueue valueQueue guard value,GuardQueue guardQueue value,
      AddGuardQueue guardQueue guard)
   => (value -> value) -> guard -> Toggle -> (Maybe value -> IO ()) 
   -> Guards guardQueue value -> Values valueQueue
   -> IO (Guards guardQueue value,Values valueQueue,Result)
-- The first function specifies the update function, and so is either
-- the identity or a constant.
pureSneak updateFn guard tog1 continuation guardQueue valueQueue =
   do
      putHere <- newEmptyMVar
      let
         getResult =
            do
               result <- takeMVar putHere
               return (Just result)
      (guardQueue,valueQueue,result) <-
         pureReceive guard tog1 (putMVar putHere) guardQueue valueQueue
      valueOpt <- -- attempt to extract a value
         case result of
            Immediate -> getResult
            Awaiting invalidate ->
               do
                  stillNotDone <- toggle1 tog1
                  if stillNotDone
                     then
                        do
                           invalidate
                           return Nothing
                     else
                        getResult
      continuation valueOpt 
         -- we must do this now, so it gets done before
         -- any receive continuations triggered by the send we are about to do.
      (guardQueue,valueQueue) <-
         case valueOpt of -- send the value found, if any
            Just value ->
               do
                  tog2 <- newToggle
                  (guardQueue,valueQueue,_) <- 
                     pureSend (updateFn value) tog2 (const done) 
                        guardQueue valueQueue
                  return (guardQueue,valueQueue)
            Nothing -> return (guardQueue,valueQueue)
      return (guardQueue,valueQueue,Immediate)
    


