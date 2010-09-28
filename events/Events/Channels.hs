-- | This is a bare-bones implementation of CML-style channels, IE no
-- guards.  Why not use NullGuardChannel you might ask?  Because all the
-- gunge we add to do guards makes it too inefficient.
--
-- To avoid memory-leaks we need to clean out superannuated registrations
-- occasionally, as otherwise we will gradually run out of memory if the
-- user continually polls a receive channel event, but no-one is sending
-- anything.  (The memory lost is potentially quite big, since it includes
-- all the continuations we will never need.)
--
-- Although this is not expressed by the type, there are three possible states
-- for the channel
-- (1) we have >=0 queued send events and no queued receive events.
-- (2) we have >=0 queued receive events and no queued send events.
-- (3) we have both send and receive events queued, but they all come
--     from the same synchronisation.
-- When we have a new send event, and there are queued receive events
-- not from the same synchronisation, we can match.  Otherwise the
-- send event must be queued.  For receive events the situation is exactly
-- the same in reverse.
--
-- Our quick and dirty strategy is to maintain an integer counter for the
-- channel.  This is initially 0 and on each send or receive registration
-- changes as follows:
-- 1) If we match an event set counter to 0.
-- 2) If we try to match an event, but fail because the event was already
--    matched by someone else (Anticipated), leave counter as it is.
-- 3) If finally we have to queue an event, look at counter.  If it
--    exceeds 10, clean the queue and set counter to 0, otherwise increment it.
-- \"cleaning\" means removing all items from the front of the queue which
-- have flipped toggles.
module Events.Channels(
   Channel,
   newChannel, -- :: IO Channel a
   -- A Channel is an instance of HasSend and HasReceive.
   ) where

import Control.Concurrent

import Util.Computation(done)
import Util.Queue


import Events.Toggle
import Events.Events

-- | A synchronous channel
newtype Channel a = Channel (MVar (Queue (Toggle,a,IO () -> IO ()),
   Queue (Toggle,IO a -> IO ()),Int))

data Res a = None | Anticipated | Found a


cleanPar :: Int -- this is how high the counter has to get before we clean.
cleanPar = 10

-- | Create a new channel
newChannel :: IO (Channel a)
newChannel =
   do
      mVar <- newMVar (emptyQ,emptyQ,0)
      return (Channel mVar)


instance HasSend Channel where
   send (Channel mVar) value = Event (
      \ toggle continuation ->
         do
            (sQueue,rQueue,counter) <- takeMVar mVar
            (rQueueOut,res) <- matchSend toggle rQueue
            case res of
               None ->
                  do
                     let
                        sQueue2 = insertQ sQueue (toggle,value,continuation)
                     (sQueue3,counter) <-
                        if counter>=cleanPar
                           then
                              do
                                 sQueue3 <- cleanSends sQueue2
                                 return (sQueue3,0)
                           else
                              return (sQueue2,counter+1)
                     putMVar mVar (sQueue3,rQueueOut,counter)
                     return(Awaiting done)
               Anticipated ->
                  do
                     putMVar mVar (sQueue,rQueueOut,counter)
                     return Immediate
               Found acontinuation ->
                  do
                     putMVar mVar (sQueue,rQueueOut,0)
                     continuation (return ())
                     acontinuation (return value)
                     return Immediate)


cleanSends :: Queue (Toggle,a,IO () -> IO ())
   -> IO (Queue (Toggle,a,IO () -> IO()))
cleanSends queue =
   case removeQ queue of
      Nothing -> return emptyQ
      Just (sendReg@(toggle,_,_),rest) ->
         do
            peek <- peekToggle toggle
            if peek
               then
                  return (insertAtEndQ rest sendReg)
               else
                  cleanSends rest

matchSend :: Toggle -> Queue (Toggle,IO a -> IO ())
   -> IO (Queue (Toggle,IO a -> IO ()),Res (IO a -> IO ()))
matchSend sendToggle queueIn =
   case removeQ queueIn of
      Nothing -> return (queueIn,None)
      Just (rc@(receiveToggle,continuation),queueOut) ->
         do
            tog <- toggle2 sendToggle receiveToggle
            case tog of
               Nothing -> return (queueOut,Found continuation)
               Just(True,True) ->
                  do
                     match2 <- matchSend sendToggle queueOut
                     case match2 of
                        (queueOut,None) ->
                           return (insertAtEndQ queueOut rc,None)
                        (queueOut,Anticipated) ->
                           return (queueOut,Anticipated)
                        (queueOut,found) ->
                           return (queueOut,found)
               Just(True,False) -> matchSend sendToggle queueOut
               Just(False,True) ->
                  return (insertAtEndQ queueOut rc,Anticipated)
               Just(False,False) -> return (queueOut,Anticipated)

instance HasReceive Channel where
   receive (Channel mVar) = Event (
      \ toggle acontinuation ->
         do
            (sQueue,rQueue,counter) <- takeMVar mVar
            (sQueueOut,res) <- matchReceive toggle sQueue
            case res of
               None ->
                  do
                     let
                        rQueue2 = insertQ rQueue (toggle,acontinuation)
                     (rQueue3,counter) <-
                        if counter>=cleanPar
                           then
                              do
                                 rQueue3 <- cleanReceives rQueue2
                                 return (rQueue3,0)
                           else
                              return (rQueue2,counter+1)

                     putMVar mVar (sQueueOut,rQueue3,counter)
                     return(Awaiting done)
               Anticipated ->
                  do
                     putMVar mVar (sQueueOut,rQueue,counter)
                     return Immediate
               Found (value,continuation) ->
                  do
                     putMVar mVar (sQueueOut,rQueue,counter)
                     continuation (return ())
                     acontinuation (return value)
                     return Immediate
      )


matchReceive :: Toggle -> Queue (Toggle,a,IO () -> IO ())
   -> IO (Queue (Toggle,a,IO () -> IO ()),Res (a,IO () -> IO ()))
matchReceive receiveToggle queueIn =
   case removeQ queueIn of
      Nothing -> return (queueIn,None)
      Just (rc@(sendToggle,value,continuation),queueOut) ->
         do
            tog <- toggle2 receiveToggle sendToggle
            case tog of
               Nothing -> return (queueOut,Found (value,continuation))
               Just(True,True) ->
                  do
                     match2 <- matchReceive receiveToggle queueOut
                     case match2 of
                        (queueOut,None) ->
                           return (insertAtEndQ queueOut rc,None)
                        (queueOut,Anticipated) ->
                           return (queueOut,Anticipated)
                        (queueOut,found) ->
                           return (queueOut,found)
               Just(True,False) -> matchReceive receiveToggle queueOut
               Just(False,True) ->
                  return (insertAtEndQ queueOut rc,Anticipated)
               Just(False,False) -> return (queueOut,Anticipated)
cleanReceives :: Queue (Toggle,IO a -> IO ())
   -> IO (Queue (Toggle,IO a -> IO ()))
cleanReceives queue =
   case removeQ queue of
      Nothing -> return emptyQ
      Just (receiveReg@(toggle,_),rest) ->
         do
            peek <- peekToggle toggle
            if peek
               then
                  return (insertAtEndQ rest receiveReg)
               else
                  cleanReceives rest




