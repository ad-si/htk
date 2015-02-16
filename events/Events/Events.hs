-- |
-- Description: Higher-order Events
--
-- 'Event's and combinators for them.
module Events.Events(
   Result(..),
   Event(..),
      -- The event type.  Instance of HasEvent and Monad.
   HasEvent(..), -- things which can be lifted to an Event

   never, -- the event which never happens
   always, -- the event which always happens

   sync, poll,  -- synchronises or polls an event
   (>>>=), (>>>), -- wraps Events
   (+>), -- choice between Events

   choose, -- chooses between many Events.
   tryEV, -- Replaces an event by one which checks for errors in the
          -- continuations.
   computeEvent, -- Allows you to compute the event with an IO action.
   wrapAbort,
      -- Allows you to specify pre- and post-registration actions.
      -- The post-registration action is executed when the pre-registration
      -- was, and some other event is registered.

   noWait, -- :: Event a -> Event ()
      -- Execute event asynchronously and immediately return.
   HasSend(..), -- overloaded send function
   HasReceive(..), -- overloaded receive function
   -- functions to send and receive without going via events.
   sendIO, -- :: HasSend chan => chan a -> a -> IO ()
   receiveIO, -- :: HasReceive chan => chan a -> IO a


   allowWhile,
      -- :: Event () -> Event a -> Event a
      -- Allow one event to happen while waiting for another.

   Request(..),
      -- Datatype encapsulating server calls which get a delayed
      -- response.
   request,
      -- :: Request a b -> a -> IO b
      -- Simple use of Request.
   doRequest, -- :: Request a b -> a -> IO (Event b,IO ())
      -- More complicated use
   spawnEvent, -- :: Event () -> IO (IO ())
   -- spawnEvent syncs on the given event in a thread.
   -- the returned action should be executed to kill the thread.

   getAllQueued, -- :: Event a -> IO [a]
   -- getAllQueued synchronises on the event as much as possible
   -- without having to wait.

   -- Functions for monadic events.  (Don't use these directly, they
   -- are only here so GHC can export the inlined versions of them . . .)
   thenGetEvent, -- :: Event a -> (a -> Event b) -> Event b
   thenEvent, -- :: Event a -> Event b -> Event b
   doneEvent, -- :: a -> Event a

   syncNoWait

   ) where

import Control.Exception
import Control.Concurrent
import Control.Applicative
import Control.Monad

import Util.Computation

import Events.Toggle
import Events.Spawn

data Result = Immediate  | Awaiting (IO ()) | AwaitingAlways (IO ())

-- ----------------------------------------------------------------------
-- Events and the HasEvent class.
-- ----------------------------------------------------------------------

newtype Event a = Event (Toggle -> (IO a -> IO ()) -> IO Result)
-- The function inside an Event registers that event for the synchronisation
-- associated with this toggle.  The three results
-- can be interpreted as follows:
-- Immediate can occur in two cases.  Either
--    (1) the event was immediately matched and we performed the provided
--        action fun with an action returning an a.
--    (2) the event was not immediately matched because someone else had
--        already flipped the toggle.
--    In both cases, the event is not registered after the function returns.
-- Awaiting action means that the event was registered.
--    The caller should always ensure that the action is executed after the
--    synchronisation has succeeded.
-- AwaitingAlways action means that the event must be done after the
--    synchronisation whether or not the action succeeds.

-- | HasEvent represents those event-like things which can be converted to
-- an event.
class HasEvent eventType where
   ---
   -- converts to an event.
   toEvent :: eventType a -> Event a

instance HasEvent Event where
   toEvent = id

-- ----------------------------------------------------------------------
-- Three trivial events.
-- ----------------------------------------------------------------------


-- | The event that never happens
never :: Event a
never = Event (\ toggle aActSink -> return (Awaiting done))

-- | The event that always happens, immediately
always :: IO a -> Event a
always aAction = Event (
   \ toggle aActSink ->
      do
         ifToggle toggle (aActSink aAction)
         return Immediate
      )

-- ----------------------------------------------------------------------
-- Continuations
-- ----------------------------------------------------------------------

-- | Attach an action to be done after the event occurs.
(>>>=) :: Event a -> (a -> IO b) -> Event b
(>>>=) (Event registerFn) continuation = Event (
   \ toggle bActionSink ->
      registerFn toggle (
         \ aAction ->
            bActionSink (
               do
                  a <- aAction
                  continuation a
               )
         )
   )
infixl 2 >>>=

-- | Attach an action to be done after the event occurs.
(>>>) :: Event a -> IO b -> Event b
(>>>) event continuation = event >>>= (const continuation)
infixl 2 >>>

{-# INLINE (>>>) #-}

-- ----------------------------------------------------------------------
-- Choice
-- ----------------------------------------------------------------------

-- | Choose between two events.  The first one takes priority.
(+>) :: Event a -> Event a -> Event a
(+>) (Event registerFn1) (Event registerFn2) = Event (
   \ toggle aActSink ->
      do
         status1 <- registerFn1 toggle aActSink
         let
            doSecond postAction1 =
               do
                  let
                     doThird postAction2 =return (AwaitingAlways (
                        do
                           postAction1
                           postAction2
                        ))
                  status2 <- registerFn2 toggle aActSink
                  case status2 of
                     Immediate ->
                        do
                           postAction1
                           return Immediate
                     Awaiting postAction2 -> doThird postAction2
                     AwaitingAlways postAction2 -> doThird postAction2
         case status1 of
            Immediate -> return Immediate
            Awaiting postAction1 -> doSecond postAction1
            AwaitingAlways postAction1 -> doSecond postAction1
      )

infixl 1 +>

-- | Choose between a list of events.
choose :: [Event a] -> Event a
choose [] = never
choose nonEmpty = foldr1 (+>) nonEmpty

-- ----------------------------------------------------------------------
-- Catching Errors
-- ----------------------------------------------------------------------

-- | Catch an error if it occurs during an action attached to an event.
tryEV :: Event a -> Event (Either SomeException a)
tryEV (Event registerFn) = Event (
   \ toggle errorOraSink ->
      registerFn toggle (\ aAct ->
         errorOraSink (Control.Exception.try aAct)
         )
      )

-- ----------------------------------------------------------------------
-- Allowing an event to vary
-- ---------------------------------------------------------------------

-- | Construct a new event using an action which is called at each
-- synchronisation
computeEvent :: IO (Event a) -> Event a
computeEvent getEvent = Event (
   \ toggle aActSink ->
      do
         (Event registerFn) <- getEvent
         registerFn toggle aActSink
      )

-- ----------------------------------------------------------------------
-- Getting information about when an event is aborted.
-- ---------------------------------------------------------------------

-- | When we synchronise on wrapAbort preAction
-- preAction is evaluated to yield (event,postAction).
-- Then exactly one of the following:
-- (1) thr event is satisfied, and postAction is not done.
-- (2) some other event in this synchronisation is satisfied
-- (so this one isn\'t), and postAction is done.
-- (3) no event is satisfied (and so we will deadlock).
wrapAbort :: IO (Event a,IO ()) -> Event a
wrapAbort preAction  =
   computeEvent (
      do
         postDone <- newSimpleToggle
         (Event registerFn,postAction) <- preAction
         let doAfter = ifSimpleToggle postDone postAction
         return (Event (
            \ toggle aActSink ->
               do
                  status <- registerFn toggle
                     (\ aAct ->
                        do
                           simpleToggle postDone
                           aActSink aAct
                        )
                  case status of
                     -- Even with Immediate we must do doAfter, as
                     -- the toggle may have been flipped by someone else.
                     Immediate -> (doAfter >> return Immediate)
                     Awaiting action -> return (Awaiting (doAfter >> action))
                     AwaitingAlways action ->
                        return (AwaitingAlways (doAfter >> action))
               ))
      )

-- ----------------------------------------------------------------------
-- Synchronisation and Polling.
-- Sigh.  Because GHC makes takeMVar/putMVar interruptible, I don't
-- know how to ensure that the postAction will get done if an
-- asynchronous exception is raised.
-- ---------------------------------------------------------------------

-- | Synchronise on an event, waiting on it until it happens, then returning
-- the attached value.
sync :: Event a -> IO a
sync (Event registerFn) =
   do
      toggle <- newToggle
      aActMVar <- newEmptyMVar
      status <- registerFn toggle (\ aAct -> putMVar aActMVar aAct)
      aAct <- takeMVar aActMVar
      case status of
         AwaitingAlways postAction -> postAction
         _ -> done
      aAct

-- | Synchronise on an event, but return immediately with Nothing if it
-- can\'t be satisfied at once.
poll :: Event a -> IO (Maybe a)
poll event =
   sync (
         (event >>>= (\ a -> return (Just a)))
      +> (always (return Nothing))
      )

-- ----------------------------------------------------------------------
-- The noWait combinator
-- ----------------------------------------------------------------------

-- | Turns an event into one which is always satisfied at once but registers
-- the value to be done later.  WARNING - only to be used with events without
-- actions attached, as any actions will not get done.  noWait is typically
-- used with send events, where we don\'t want to wait for someone to pick up
-- the value.
noWait :: Event a -> Event ()
noWait (Event registerFn) = Event (
   \ toggle unitActSink ->
      do
         ifToggle toggle (
            do
               toggle' <- newToggle
               registerFn toggle' (const done)
               unitActSink (return ())
               done
            )
         return Immediate
   )

-- | Register an event as synchronised but don\'t wait for it to complete.
-- WARNING - only to be used with events without
-- actions attached, as any actions will not get done.  noWait is typically
-- used with send events, where we don\'t want to wait for someone to pick up
-- the value.
-- synchronise on something without waiting
syncNoWait :: Event a -> IO ()
syncNoWait (Event registerFn) =
   do
      toggle <- newToggle
      registerFn toggle (const done)
      done

{-# RULES
"syncNoWait" forall event . sync (noWait event) = syncNoWait event
"syncNoWait2"
   forall event continuation . sync ((noWait event) >>>= continuation) =
      (syncNoWait event >> continuation ())
  #-}


-- ----------------------------------------------------------------------
-- The HasSend and HasReceive classes
-- ----------------------------------------------------------------------

-- | HasSend represents things like channels on which we can send values
class HasSend chan where
   ---
   -- Returns an event which corresponds to sending something on a channel.
   -- For a synchronous channel (most channels are synchronous) this event
   -- is not satisfied until someone accepts the value.
   send :: chan a -> a -> Event ()

-- | HasReceive represents things like channels from which we can take values.
class HasReceive chan where
   ---
   -- Returns an event which corresponds to something arriving on a channel.
   receive :: chan a -> Event a

-- Two handy abbreviations

-- | Send a value along a channel (as an IO action)
sendIO :: HasSend chan => chan a -> a -> IO ()
sendIO chan msg = sync (send chan  msg)

-- | Get a value from a channel (as an IO action)
receiveIO :: HasReceive chan => chan a -> IO a
receiveIO chan = sync (receive chan)

-- ----------------------------------------------------------------------
-- Monadic Events
-- We include some extra GHC magic here, so that using "always"
-- in monadic events is not especially inefficient.
-- ----------------------------------------------------------------------

instance Monad Event where
   (>>=) = thenGetEvent
   (>>) = thenEvent
   return = doneEvent

   fail str = always (ioError (userError str))

instance Applicative Event where
   pure = return
   (<*>) = ap

instance Functor Event where
   fmap  = liftM

thenGetEvent :: Event a -> (a -> Event b) -> Event b
thenGetEvent event1 getEvent2 = event1 >>>= (\ val -> sync(getEvent2 val))

thenEvent :: Event a -> Event b -> Event b
thenEvent event1 event2 = event1 >>> (sync(event2))

doneEvent :: a -> Event a
doneEvent val = always (return val)

{-# INLINE thenGetEvent #-}
{-# INLINE thenEvent #-}
{-# INLINE doneEvent #-}

-- Rules allowing us to use "always" in monadic events efficiently.
{-# RULES
"always1" forall action . sync (always action) = action
"always" forall action continuation .
         (>>>=) (always action) continuation = always (action >>= continuation)
   #-}

-- ----------------------------------------------------------------------
-- Other miscellaneous event functions.
-- ----------------------------------------------------------------------

-- | allowWhile event1 event2 waits for event2, while handling event1.
allowWhile :: Event () -> Event a -> Event a
allowWhile event1 event2 =
      event2
   +>(do
         event1
         allowWhile event1 event2
     )

data Request a b = Request (a -> IO (Event b,IO ()))
-- A Request operation represents a call to a server to evaluate
-- a function :: a->b.  The Event b is activated with the result.
-- The client should call the supplied action if the event is
-- no longer needed.

request :: Request a b -> a -> IO b
request rq a =
   do
      (event,_) <- doRequest rq a
      sync event

doRequest :: Request a b -> a -> IO (Event b,IO ())
doRequest (Request rqFn) request = rqFn request

-- | Synchronise on an event in a different thread.
-- The kill action it returns is unsafe since it can cause deadlocks if
-- it occurs at an awkward moment.  To avoid this use spawnEvent, if possible.
spawnEvent :: Event () -> IO (IO ())
spawnEvent reactor = spawn (sync reactor)

-- | get all we can get from the event without waiting.
getAllQueued :: Event a -> IO [a]
getAllQueued event = gAQ event []
   where
      gAQ event acc =
         do
            maybeA <- poll event
            case maybeA of
               Nothing -> return (reverse acc)
               Just a -> gAQ event (a:acc)

