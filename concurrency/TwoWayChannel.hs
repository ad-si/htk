{- A TwoWayChannel is a generalisation of Einar Karlsen's
   channels.  We now define channels in terms of TwoWayChannels; this
   is because TwoWayChannels are themselves symmetric, and so easier
   to define. 

   A TwoWayChannel has two types associated with it: the Left type and
   the Right type.  Two sorts of event are implemented, a Left event and
   a Right event.  A Left event is constructed from a Left type value and
   returns (when it happens) one of the Right type.  And vice-versa.
   The events "happen" when a Left event and a Right event are paired off.
   This should happen in FIFO order.

   A Channel as Einar defined it is a TwoWayChannel with either the
   Left or the Right type set to ().

   Efficiency Note.  We use Einar's naive functional implementation
   of queues in the Queue module for storing unpaired left/right
   events.  This has excellent amortised behaviour if (of course)
   the queue is used in a single-threaded way.  A potential problem
       amortised may not be good enough.  Suppose there are lots of unpaired
       left events and a right event comes along.  It may then be necessary
       to reverse the whole queue of left events to get the first queued one
       and pair it off with the right event.
   This can be fixed by replacing
   the Queue model by one of the more sophisticated functional
   queue implementations (I expect Chris Okosaki has probably
   implemented one), for example one of the ones that implements
   a tree of queues of bounded size, thus spreading out the work of
   list reversal.

   Bug Warning.  We dodge the issue of what happens if a Left and
   Right event for the same channel occur in the same syncPrimEvents
   list.  An attempt to pair off left and right events in the same
   sync will cause Fail (see Toggle.hs).  It is hard to know what else
   to do - sync should only handle one event at once.  If UniForM does
   turn out to need this, it only really makes sense within an interactor
   (a repeated sync on an event with null result) and the best solution
   would I think be to define interactors separately for PrimEvents,
   BaseEvents and Events.  (This will also be a more efficient approach.) 
   -}
module TwoWayChannel(
   TwoWayChannel,
   newTwoWayChannel, -- create a new channel
   leftEvent, -- create a left event
   rightEvent -- create a right event
   ) where

import qualified Concurrent

import Queue
import PrimEvent
import Toggle
import Debug(debug,newId,(@:))

newTwoWayChannel :: IO (TwoWayChannel leftType rightType)
newTwoWayChannel = 
   do
      mVar <- Concurrent.newMVar (Left emptyQ)
      return (TwoWayChannel mVar)

leftEvent :: TwoWayChannel leftType rightType -> leftType -> PrimEvent rightType
-- Sorry - the implementation of this and rightEvent are identical except 
-- that the strings "left" and "right" are interchanged (as are "Left" 
-- and "Right")
leftEvent (TwoWayChannel mVar) valL = 
   let
      syncFun (PrimEventArg (toggle,continuation)) =
         do
            let candidate = ChannelEventArg valL toggle continuation
            getChannel <- Concurrent.takeMVar mVar
            (newChannel,result) <- case getChannel of
               Left lQueue -> -- join this item to the existing queue of
                  -- unhandled left items
                  return(Left(insertQ lQueue 
                     (ChannelEventArg valL toggle continuation)),Awaiting)
               Right rQueue -> -- attempt to pair this item with one of
                  -- the queue of unhandled right items.
                  do
                     trial <- tryToPair candidate rQueue
                     case trial of
                        Success newRQueue ->
                           return (Right newRQueue,Immediate)
                        Anticipated newRQueue ->
                           return (Right newRQueue,Immediate)
                        NoMatch newLQueue ->
                           return (Left newLQueue,Awaiting)
            "60" @: Concurrent.putMVar mVar newChannel
            return result
   in
      PrimEvent syncFun

rightEvent :: TwoWayChannel leftType rightType -> rightType -> PrimEvent leftType
-- Sorry - the implementation of this and leftEvent are identical except 
-- that the strings "right" and "left" are interchanged (as are "Right" 
-- and "Left")
rightEvent (TwoWayChannel mVar) valL = 
   let
      syncFun (PrimEventArg (toggle,continuation)) =
         do
            let candidate = ChannelEventArg valL toggle continuation
            getChannel <- Concurrent.takeMVar mVar
            (newChannel,result) <- case getChannel of
               Right lQueue -> -- join this item to the existing queue of
                  -- unhandled right items
                  return(Right(insertQ lQueue 
                     (ChannelEventArg valL toggle continuation)),Awaiting)
               Left rQueue -> -- attempt to pair this item with one of
                  -- the queue of unhandled left items.
                  do
                     trial <- tryToPair candidate rQueue
                     case trial of
                        Success newRQueue ->
                           return (Left newRQueue,Immediate)
                        Anticipated newRQueue ->
                           return (Left newRQueue,Immediate)
                        NoMatch newLQueue ->
                           return (Right newLQueue,Awaiting)
            "61" @: Concurrent.putMVar mVar newChannel
            return result
   in
      PrimEvent syncFun

data TwoWayChannel leftType rightType = 
   TwoWayChannel 
     (Concurrent.MVar(
         Either
            (Queue (ChannelEventArg leftType rightType)) 
            -- unpaired left-event syncs
            (Queue (ChannelEventArg rightType leftType)) -- ditto right
         )
      )
-- If there are no unpaired event queues this may either be 
-- Left emptyQ or Right emptyQ.  These two must appear to be equivalent to
-- all functions.

-- ChannelEventArg contains channel data for a registered event.
data ChannelEventArg eventIn eventOut =
   ChannelEventArg eventIn Toggle (eventOut -> IO())
-- this is basically a value A paired with a PrimEventArg B.

-- The following function attempts to pair one of the current list of
-- (left or right) syncs with a new (right or left) sync, executing
-- the continuation functions if it succeeds.
tryToPair :: 
   ChannelEventArg candidate queued -> 
   (Queue (ChannelEventArg queued candidate)) ->
   IO(TryToPairResult queued candidate)
tryToPair(channelEventArgC@(ChannelEventArg valC toggleC continuationC))
      queue =
 -- in variable names C == "candidate"; Q == "queued"
   case removeQ queue of
      Nothing -> 
         return(NoMatch(singletonQ channelEventArgC))
      Just(channelEventArgQ@(ChannelEventArg valQ toggleQ continuationQ),
            newQueue) -> 
         do
            toggleResult <- toggle2(toggleC,toggleQ)
            case toggleResult of
               Nothing ->
                  do -- success
                     continuationQ valC
                     continuationC valQ
                     return(Success newQueue)
               Just(True,False) -> -- queued event has already been done
                  tryToPair channelEventArgC newQueue
               Just(False,True) -> 
                  -- candidate event has been done, the queued event hasn't.
                  -- But don't waste work already done in popping queue!
                  return(Anticipated(insertAtEndQ newQueue channelEventArgQ))
               Just(False,False) -> -- Both events already done.
                  return(Anticipated newQueue)
-- definition: first argument is "candidate event"; second argument is 
-- "queued events"
data TryToPairResult queued candidate =
      Success (Queue (ChannelEventArg queued candidate)) 
      -- pairing successful, this is new queue of queued events 
   |  Anticipated (Queue (ChannelEventArg queued candidate))
      -- pairing unsuccessful because the candidate event was done before we
      -- got there.
   |  NoMatch (Queue (ChannelEventArg candidate queued))
      -- All the queued events turned out to have already been done,
      -- so we make a new queue for the candidate event.



