{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | GuardedEvents implements guarded events for channels.
module Events.GuardedChannels(
   GuardedChannel,
      -- parameterised on the guard and the value,
      --
      -- instance of HasSend, HasListen (and hence automatically HasReceive)

   GQ,VQ, -- Type abbreviations, used in the next declaration.
   newGuardedChannel,
      -- :: HasGuardedChannel guardQueue valueQueue guard value
      -- => GQ guardQueue value -> VQ valueQueue
      --    IO GuardedChannel guard value
      -- the guardQueue and valueQueue are not read, and just provide
      -- the types to use.

   sneak,
      -- :: ( .. context .. )
      -- => GuardedChannel guard value
      -- -> GuardedEvent guard (Maybe value)
   replace,
      -- :: ( .. context .. )
      -- => GuardedChannel guard value -> value
      -- -> GuardedEvent guard (Maybe value)

   -- Classes the user should instance to construct different sorts of
   -- queue.  Actually you only need to instance HasEmpty, HasRemove and
   -- HasAdd, since the others just
   HasEmpty(..),
   HasRemove(..),
   HasAdd(..),

   CanSendX,
   HasGuardedChannel,
   ) where

import Control.Concurrent

import Util.Computation (done)

import Events.Toggle

import Events.Events
import Events.GuardedEvents

-- ---------------------------------------------------------------
-- Guarded Channels and their creation.
-- ---------------------------------------------------------------

data GuardedChannel guard value =
   forall guardQueue valueQueue .
      HasGuardedChannel guardQueue valueQueue guard value
   => GuardedChannel (MVar (Contents guardQueue valueQueue value))

data Contents guardQueue valueQueue value =
   Contents !(guardQueue (GuardInfo value)) !(valueQueue ValueInfo)

-- GuardInfo and ValueInfo give toggles + continuations
type GuardInfo value = ToggledData (IO value -> IO ())
type ValueInfo = ToggledData (IO () -> IO ())

type GQ guardQueue value = guardQueue (GuardInfo value)
type VQ valueQueue = valueQueue ValueInfo

newGuardedChannel :: HasGuardedChannel guardQueue valueQueue guard value
   => GQ guardQueue value -> VQ valueQueue
   -> IO (GuardedChannel guard value)
newGuardedChannel
      (_ :: guardQueue (GuardInfo value)) (_ :: valueQueue ValueInfo) =
   do
      (emptyGuardQueue :: guardQueue (GuardInfo value)) <- newEmpty
      (emptyValueQueue :: valueQueue ValueInfo) <- newEmpty
      mVar <- newMVar (Contents emptyGuardQueue emptyValueQueue)
      return (GuardedChannel mVar)

-- ---------------------------------------------------------------
-- Implementing the channel events
-- ---------------------------------------------------------------

instance HasListen GuardedChannel where
   listen (GuardedChannel mVar) =
      GuardedEvent
         (\ guard -> Event (
            \ toggle guardContinuation ->
               do
                  (Contents guardQueue valueQueue) <- takeMVar mVar
                  (guardQueue2,valueQueue2,sendResult) <- sendX
                     guardQueue valueQueue toggle guard guardContinuation
                  putMVar mVar (Contents guardQueue2 valueQueue2)
                  -- Now perform the continuations if any and return.
                  case sendResult of
                     Anticipated -> return Immediate
                     Queued invalidate -> return (Awaiting invalidate)
                     Matched value valueContinuation ->
                        do
                           valueContinuation (return ())
                           guardContinuation (return value)
                           return Immediate
               )
            )
         nullGuard

instance Guard guard => HasReceive (GuardedChannel guard) where
   receive = toEvent . listen

instance HasSend (GuardedChannel guard) where
   send (GuardedChannel mVar :: GuardedChannel guard value)
      (value :: value)  =
      Event (
         \ toggle valueContinuation ->
            do
               (Contents guardQueue valueQueue) <- takeMVar mVar
               (valueQueue2,guardQueue2,sendResult)
                  <- sendX valueQueue guardQueue toggle value valueContinuation
               putMVar mVar (Contents guardQueue2 valueQueue2)
               -- Now perform the continuations if any and return.
               case sendResult of
                  Anticipated -> return Immediate
                  Queued invalidate -> return (Awaiting invalidate)
                  Matched (guard :: guard) guardContinuation ->
                        do
                           valueContinuation (return ())
                           guardContinuation (return value)
                           return Immediate
         )

atomicUpdate :: Guard guard => (value -> value) -> GuardedChannel guard value
   -> GuardedEvent guard (Maybe value)
-- atomicUpdate updateFn
-- is like listen except (a) it doesn't wait, instead returning
-- Nothing if it can't match immediately; (b) if it can match immediately,
-- it computes a new value and puts it back into the queue (at the end),
-- without leaving a gap, so that even if someone attempts to poll the
-- channel at this moment they won't see a point when it's empty;
-- it also returns the original value.
atomicUpdate updateFn (GuardedChannel mVar :: GuardedChannel guard value) =
   GuardedEvent (
      \ (guard :: guard) -> Event (
         \ toggle guardContinuation ->
            do
               (Contents guardQueue valueQueue) <- takeMVar mVar
               (guardQueue2,valueQueue2,
                     sendResult :: (SendResult value (IO () -> IO ())))
                  <- sendX guardQueue valueQueue toggle guard
                     (\ valueAct -> guardContinuation
                        (valueAct >>= (return . Just)))
               case sendResult of
                  Anticipated ->
                     do
                        putMVar mVar (Contents guardQueue2 valueQueue2)
                        return Immediate
                  Queued invalidate ->
                     do
                        putMVar mVar (Contents guardQueue2 valueQueue2)
                        -- force the event to happen anyway
                        resultNothing <- toggle1 toggle
                        if resultNothing
                           then
                              do
                                 invalidate
                                 guardContinuation (return Nothing)
                           else
                              done
                        return Immediate
                  Matched value valueContinuation ->
                     do
                        let newValue = updateFn value
                        toggle' <- newToggle
                        (valueQueue3,guardQueue3,
                           sendResult :: SendResult guard (IO value -> IO()))
                           <- sendX valueQueue2 guardQueue2 toggle' newValue
                              (\ _ -> return ())
                        putMVar mVar (Contents guardQueue3 valueQueue3)
                        -- execute all the continuations we have, and return.
                        valueContinuation (return ())
                        guardContinuation (return (Just value))
                        case sendResult of
                           Queued invalidate -> return Immediate
                           -- We never invalidate this event.
                           Matched (guard :: guard) guardContinuation ->
                              do
                                 guardContinuation (return newValue)
                                 return Immediate
                           -- Anticipated should be impossible here.
            )
         )
      nullGuard



sneak :: Guard guard => GuardedChannel guard value
   -> GuardedEvent guard (Maybe value)
sneak guardedChannel = atomicUpdate id guardedChannel

replace :: Guard guard => GuardedChannel guard value -> value
   -> GuardedEvent guard (Maybe value)
replace guardedChannel newValue = atomicUpdate (const newValue) guardedChannel

-- ---------------------------------------------------------------
-- The classes the user should instance.
-- ---------------------------------------------------------------

class HasEmpty xQueue where
   newEmpty :: IO (xQueue xData)

class HasRemove yQueue x y where
   remove :: yQueue yData -> x ->
      IO (Maybe (y,yData,IO (yQueue yData)),yQueue yData)
   -- remove yQueue x attempts to match an x with a value in yQueue.
   -- It returns a pair.
   -- If there is no match, we get (Nothing,newQueue)
   -- If there is a match, we get (Just(y,yData,restoreQueue),newQueue)
   -- where newQueue is the queue with the match removed, and
   -- restoreQueue is an action which restores the queue to the way it
   -- was before.

class HasAdd xQueue x where
   add :: xQueue xData -> x -> xData -> IO (xQueue xData,IO ())

class (HasRemove yQueue x y,HasAdd xQueue x) =>
   CanSendX xQueue yQueue x y

instance (HasRemove yQueue x y,HasAdd xQueue x) =>
   CanSendX xQueue yQueue x y

class (Guard guard,HasEmpty guardQueue,HasEmpty valueQueue,
   CanSendX guardQueue valueQueue guard value,
   CanSendX valueQueue guardQueue value guard)
   => HasGuardedChannel guardQueue valueQueue guard value

instance (Guard guard,HasEmpty guardQueue,HasEmpty valueQueue,
   CanSendX guardQueue valueQueue guard value,
   CanSendX valueQueue guardQueue value guard)
   => HasGuardedChannel guardQueue valueQueue guard value

-- ---------------------------------------------------------------
-- Implementing searching for matching events.
-- ---------------------------------------------------------------

data ToggledData continuation = ToggledData !Toggle continuation

data SendResult y yContinuation =
      Matched y yContinuation
      -- the event has been matched with this y + Continuation.
   |  Queued (IO ())
      -- the event has been queued; the supplied action may be used to
      -- cancel it, once the toggle for this synchronisation has been
      -- toggled by someone.
   |  Anticipated
      -- The toggle for the synchronisation has already been toggled by
      -- someone else.

sendX :: (CanSendX xQueue yQueue x y)
   => xQueue (ToggledData xContinuation) -> yQueue (ToggledData yContinuation)
   -> Toggle -> x -> xContinuation
   -> IO (xQueue (ToggledData xContinuation),
         yQueue (ToggledData yContinuation),(SendResult y yContinuation))
sendX xQueue yQueue xToggle x xContinuation =
   do
      (match,yQueue2) <- remove yQueue x
      case match of
         Nothing ->
         -- no matching event.  Add x to xQueue.
            do
               (xQueue2,invalidate) <-
                  add xQueue x (ToggledData xToggle xContinuation)

               return (xQueue2,yQueue2,Queued invalidate)
         Just (y,ToggledData yToggle yContinuation,getYQueue0) ->
         -- matching event found.  Attempt to handle it
            do
               toggled <- toggle2 xToggle yToggle
               case toggled of
                  Nothing -> -- toggle successful
                     return (xQueue,yQueue2,Matched y yContinuation)
                  Just (True,False) ->
                     -- toggle failed because the matching event has been
                     -- done.  Repeat with remaining queue.
                     sendX xQueue yQueue2 xToggle x xContinuation
                  Just (False,True) ->
                     -- toggle failed because the event we are synchronising
                     -- on has been done.  So put the item back on the yQueue
                     -- and return
                     do
                        yQueue0 <- getYQueue0
                        return (xQueue,yQueue0,Anticipated)
                  Just (False,False) ->
                     -- both of the above . . .
                     return (xQueue,yQueue2,Anticipated)
                  Just (True,True) ->
                     -- toggle failed because we are synchronising
                     -- a send and listen operation on the same channel.
                     do
                        (matchRest @ (xQueue3,yQueue3,success)) <-
                           sendX xQueue yQueue2 xToggle x xContinuation
                        case success of
                           Queued _ ->
                              -- the xToggle event was added to xQueue3,
                              -- so put the event we just rejected back on
                              -- the yQueue
                              do
                                 yQueue0 <- getYQueue0
                                 return (xQueue3,yQueue0,success)
                           _ ->
                              -- Otherwise the containing synchronisation has
                              -- been satisfied, and thus the original matched
                              -- event, which is also part of that
                              -- synchronisation, can be thrown away.  This is
                              -- good, because otherwise I don't know what
                              -- we'd do with it.
                              return matchRest


