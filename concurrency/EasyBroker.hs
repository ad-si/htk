{- The EasyBroker is a light-weight version of EventBroker in which
   there is only one sort of event, and all incoming messages are
   dispatched to all listeners currently registered.
   -}
module EasyBroker(
   EasyBroker,
   newEasyBroker,       -- :: IO (EasyBroker message)
   dispatchMessage,     -- :: EasyBroker message -> message -> IO ()
   EventSink,           -- similar to a Listener but without acknowledgement
   newEventSink,        -- :: (message -> EV ()) -> IO(EventSink message) 
   registerEventSink,   -- :: EasyBroker message -> EventSink message -> IO ()
   deregisterEventSink  -- :: EasyBroker message -> EventSink message -> IO ()
   -- (if an EventSink is registered multiple times, the message is
   -- transmitted once for each registration)
   ) where

import Object
import Debug(debug)

import Thread
import Selective

-- --------------------------------------------------------------------------
--  EventSink
-- --------------------------------------------------------------------------

data EventSink message = EventSink ObjectID (message -> EV ())

newEventSink :: (message -> EV ()) -> IO (EventSink message) 
newEventSink sinkFn =
   do
      objectID <- newObject
      return (EventSink objectID sinkFn)

-- --------------------------------------------------------------------------
--  EasyBroker
-- --------------------------------------------------------------------------

data EasyBroker message = EasyBroker {
   registrationChanges :: 
      Channel (EasyBrokerData message -> EasyBrokerData message),
   inQueue :: Channel message
   } 

registerEventSink :: EasyBroker message -> EventSink message -> IO ()
registerEventSink 
      (EasyBroker {registrationChanges=registrationChanges}) eventSink =
   sendIO registrationChanges (registerFn eventSink)

deregisterEventSink :: EasyBroker message -> EventSink message -> IO ()
deregisterEventSink
      (EasyBroker {registrationChanges=registrationChanges}) eventSink =
   sendIO registrationChanges (deregisterFn eventSink)

dispatchMessage :: EasyBroker message -> message -> IO ()
dispatchMessage (EasyBroker{inQueue=inQueue}) message =
   sendIO inQueue message

-- --------------------------------------------------------------------------
--  EasyBrokerData
-- --------------------------------------------------------------------------


data EasyBrokerData message = EasyBrokerData [EventSink message]

emptyEasyBrokerData :: EasyBrokerData message
emptyEasyBrokerData = EasyBrokerData []

registerFn :: EventSink message -> EasyBrokerData message -> 
      EasyBrokerData message
registerFn eventSink (EasyBrokerData eventSinkList) =
   (EasyBrokerData (eventSink:eventSinkList))

deregisterFn (EventSink objectID _) (EasyBrokerData eventSinkList) =
      EasyBrokerData (deleteFirst eventSinkList)
   where
      deleteFirst (first@(EventSink objectID2 _):rest) 
         | (objectID == objectID2) = rest
         | True = first : deleteFirst rest
      -- patternmatch here means deregister of something not previously
      -- registered

broadcast :: message -> [EventSink message] -> EVSet () -> EVSet ()
broadcast message [] evSet = evSet
broadcast message ((EventSink _ messFn) : rest) evSet =
   broadcast message rest (addEVSet (messFn message) evSet)

-- --------------------------------------------------------------------------
--  EasyBroker thread
-- --------------------------------------------------------------------------

newEasyBroker :: IO (EasyBroker message)
newEasyBroker =
   do
      registrationChanges <- newChannel
      inQueue <- newChannel
      let 
         broker = EasyBroker{
            registrationChanges = registrationChanges,
            inQueue = inQueue
            }   
      forkIO (easyBrokerThread broker emptyEasyBrokerData emptyEVSet)
      return broker

easyBrokerThread :: EasyBroker message -> EasyBrokerData message 
   -> EVSet () -> IO ()
-- see EventBroker.eventBroker
easyBrokerThread 
      (broker@
         EasyBroker{registrationChanges=registrationChanges,inQueue=inQueue}
         ) 
      (easyBrokerData@(EasyBrokerData eventSinkList))
      evSet =
   sync(
         (receive registrationChanges) >>>=
            (\ registrationChange -> 
               easyBrokerThread 
                  broker (registrationChange easyBrokerData) evSet
               )
      +> (getEVSetEvent evSet) >>>=
            (\ ((),newEVSet) ->
               easyBrokerThread broker easyBrokerData newEVSet
               )
      +> (receive inQueue) >>>=
            (\ message ->
               easyBrokerThread broker easyBrokerData
                  (broadcast message eventSinkList evSet)
               )
      )
           
      






