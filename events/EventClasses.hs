{- EventClasses defines the various classes of events -}
module EventClasses(
   Result(..),
   PrimEvent(..), -- the very lowest class of events.
      -- Instances IsBaseEvent, IsEvent, HasTrivial
   BaseEvent(..), -- PrimEvent's extended to be an instance of HasContinuation
      -- Also instances IsBaseEvent, IsEvent, HasTryEV, HasTrivial, HasSync.
   IsBaseEvent(..), -- Things which can be turned into a BaseEvent
   Event(..), -- BaseEvent's extended to be an instance of HasChoice
      -- Also instances IsEvent, HasTryEV, HasContinuation, HasTrivial, 
      -- HasSync.
   IsEvent(..), -- things which can be lifted to an Event
   HasChoice(..), -- Things for which (+>) works.
   HasContinuation(..), -- Things for which (>>>=) and (>>>) work.
   (>>>), 
   HasTryEV(..), -- Things for which tryEV works.  (Allows you to wrap
                  -- an error handler to deal with errors in an events 
                  -- continuation.)
   HasTrivial(..), -- events which always succeed and execute the given
                  -- action
   HasSync(..), -- Has the sync action, which waits on an event.
   poll, -- The poll function, which is like sync except it doesn't wait
         -- if there isn't an immediate answer. 
   ) where

import Concurrent

import Computation

import Toggle

data Result = Immediate  | Awaiting (IO ())

-- ----------------------------------------------------------------------
-- Primitive Events.  We build all other events up from these.
-- ----------------------------------------------------------------------

newtype PrimEvent a = PrimEvent (Toggle -> (a -> IO ()) -> IO Result)
-- A PrimEvent a is a source of values of type a.  We register an
-- action function of type (a -> IO ()) to be performed when this value
-- becomes available, if we can succeed in toggling the Toggle. 
-- In return we get one of two things: either
-- Immediate, indicating that the value has already been made available
-- (not necessarily from this event), and the action performed, or 
-- Awaiting action.  The supplied action can be used later to invalidate
-- the registration, which means the event implementation is entitled to
-- forget the registration.  This is only an optimisation - the action
-- won't be performed unless the toggle is successfully toggled.

-- Here is a trivial event
trivialPrimEvent :: IO a -> PrimEvent a
trivialPrimEvent action = PrimEvent(
   \ toggle continuation ->
      do
         toggled <- toggle1 toggle
         if toggled
            then
               do
                  value <- action
                  continuation value
            else 
               done
         return Immediate
   )

-- Here are some registration functions.
register :: PrimEvent a -> Toggle -> (a -> IO ()) -> IO Result
register (PrimEvent registerFn) toggle continuation =
   registerFn toggle continuation
{-# INLINE register #-}

-- We will implement
-- a more general poll function but we implement this now partly to
-- get used to handling PrimEvents.
pollPrim :: PrimEvent a -> IO (Maybe a)
pollPrim (PrimEvent registerFn) =
   do
      putHere <- newEmptyMVar
      let
         getResult =
            do
               result <- takeMVar putHere
               return (Just result)
      toggle <- newToggle
      success <- registerFn toggle (putMVar putHere)
      case success of
         Immediate -> getResult
         Awaiting invalidate ->
            do
               stillNotDone <- toggle1 toggle
               if stillNotDone
                  then
                     do
                        invalidate
                        return Nothing
                  else
                     getResult
 
-- ----------------------------------------------------------------------
-- Base Events and the new Event type.
-- ----------------------------------------------------------------------

data BaseEvent a = forall b . BaseEvent !(PrimEvent b) !(b -> IO a)

data Event a = Event [BaseEvent a]

 
-- ----------------------------------------------------------------------
-- The IsBaseEvent class.
-- ----------------------------------------------------------------------

class IsBaseEvent eventType where
   toBaseEvent :: eventType value -> BaseEvent value

instance IsBaseEvent PrimEvent where
   toBaseEvent primEvent = BaseEvent primEvent return

instance IsBaseEvent BaseEvent where
   toBaseEvent baseEvent = baseEvent

-- ----------------------------------------------------------------------
-- The IsEvent class.
-- ----------------------------------------------------------------------

class IsEvent eventType where
   toEvent :: eventType value -> Event value

instance IsBaseEvent eventType => IsEvent eventType where
   toEvent event = Event [toBaseEvent event]

instance IsEvent Event where
   toEvent event = event

 
-- ----------------------------------------------------------------------
-- The HasChoice class
-- ----------------------------------------------------------------------

class HasChoice eventType where
   choose :: eventType value -> Event value -> Event value

infixr 0 +>

(+>) :: (HasChoice eventType1,IsEvent eventType2) 
   => eventType1 value -> eventType2 value -> Event value
(+>) event1 event2 = event1 `choose` (toEvent event2)

instance HasChoice BaseEvent where
   choose baseEvent (Event events) = Event (baseEvent : events)

instance HasChoice Event where
   choose (Event events1) (Event events2) = Event (events1 ++ events2)

 
-- ----------------------------------------------------------------------
-- The HasContinuation class
-- ----------------------------------------------------------------------

class HasContinuation eventType where
   (>>>=) :: eventType value1 -> (value1 -> IO value2) -> Event value2
-- actually we'd like BaseEvent's just to go to BaseEvents, but
-- we also need GuardedEvent's to be an instance.

(>>>) :: (HasContinuation eventType)
   => eventType value1 -> (IO value2) -> Event value2
(>>>) event continuation = event >>>= (const continuation)

infixr 1 >>>= 
infixr 1 >>>

infixr 1 %>>>=
(%>>>=) :: BaseEvent value1 -> (value1 -> IO value2) -> BaseEvent value2
(%>>>=) (BaseEvent primEvent cont1) cont2 =
  BaseEvent primEvent 
     (\ val0 -> 
        do
           val1 <- cont1 val0
           cont2 val1
        )


instance HasContinuation BaseEvent where
   (>>>=) baseEvent cont2 =
      toEvent (baseEvent %>>>= cont2)

instance HasContinuation Event where
   (>>>=) (Event events) cont =
      Event (map (%>>>= cont) events)

-- ----------------------------------------------------------------------
-- tryEV
-- ----------------------------------------------------------------------

class HasTryEV eventType where
   tryEV :: eventType value -> eventType (Answer value)

instance HasTryEV BaseEvent where
   tryEV (BaseEvent primEvent continuation) =
      BaseEvent primEvent (\ value -> tryM (continuation value))

instance HasTryEV Event where
   tryEV (Event events) = Event (map tryEV events)

-- ----------------------------------------------------------------------
-- HasTrivial
-- ----------------------------------------------------------------------

class HasTrivial eventType where
   trivial :: IO value -> eventType value

instance HasTrivial PrimEvent where
   trivial act = trivialPrimEvent act

instance HasTrivial BaseEvent where
   trivial act = toBaseEvent (trivialPrimEvent act)

instance HasTrivial Event where
   trivial act = toEvent (trivialPrimEvent act)

-- ----------------------------------------------------------------------
-- Synchronisation
-- ----------------------------------------------------------------------

class HasSync eventType where
   sync :: eventType value -> IO value

instance HasSync Event where
   sync (Event events :: Event value) =
      do
         toggle <- newToggle
         -- write the continuation action
         (continuationMVar :: MVar (IO value)) <- newEmptyMVar         
         let
            registerAndGet :: [BaseEvent value] -> IO () -> IO value  
            -- arg1 is remaining base events to register
            -- arg2 is an action which invalidates all the registrations.
            registerAndGet [] invalidateAct = pickup invalidateAct
            registerAndGet 
                  ((BaseEvent primEvent continuation) : events)
                  invalidateAct =
               do
                  registrationResult <- register primEvent toggle
                     (\ value -> putMVar continuationMVar (continuation value))
                  case registrationResult of
                     Immediate -> pickup invalidateAct
                     Awaiting invalidateAct2 ->
                        registerAndGet events (invalidateAct2 >> invalidateAct)
            -- pickup collects the action and executes it, also doing the
            -- invalidation.
            pickup :: IO () -> IO value
            pickup invalidateAct =
               do
                  resultAct <- takeMVar continuationMVar
                  invalidateAct
                  resultAct
         registerAndGet events done 

-- for efficiency reasons, we provide a sync function for the case of
-- one event.  We don't have to invalidate as this is the channel 
-- implementation will presumably forget the event after returning it.
instance HasSync BaseEvent where 
   sync (BaseEvent primEvent continuation) =
      do
         toggle <- newToggle
         -- write the result in this MVar
         continuationMVar  <- newEmptyMVar
         register primEvent toggle 
            (\ value -> putMVar continuationMVar (continuation value))
         resultAct <- takeMVar continuationMVar
         resultAct


poll :: (HasContinuation eventType) => eventType value -> IO (Maybe value)
poll (event :: eventType value) =
   do
      let
         event1 = event >>>= (\ value -> return (Just value))
      sync (event1 +> (trivial (return Nothing) :: Event (Maybe value)))

-- ----------------------------------------------------------------------
-- Monadic Events
-- ----------------------------------------------------------------------

instance Monad Event where
   (>>=) event1 getEvent2 =
      event1 >>>= (\ val -> sync(getEvent2 val))
   -- We let (>>) be inferred automatically.
   return val = trivial (return val)
   fail str = trivial (ioError (userError str))
