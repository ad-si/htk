{- #########################################################################

MODULE        : EventBroker
AUTHOR        : Einar W. Karlsen, George 
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : 1.0
DESCRIPTION   : This is a standard adaptor, which sits in between some
                external reactive tool (event source), and the network
                of interactors (event listeners).

                The adaptor maintains the set of event registrations that the
                interactors have made. Events can be send to the adaptor,

                and are delegated to the interactors as requests or notices. 

EventBroker's are (in theory) documented in Appendix C of Einar's thesis.

For a typical use see Signal.hs.

   ######################################################################### -}


module EventBroker (
   EventDesignator(..),
   DispatchMode(..),

   EventBroker,    

   Adaptor(..),

   newEventBroker,

   awaitEvent
   
   ) where

import Concurrency
import FiniteMap
import Dynamics
import SIMClasses
import Interaction(EventDesignator(..),EventID,EventListener(..),Listener,
                interaction,IA)
import Debug(debug)

-- --------------------------------------------------------------------------
--  Data Type
-- --------------------------------------------------------------------------

data Typeable a => EventBroker a = 
   EventBroker 
      (MsgQueue (Message a)) 
      (SAP NR ())
-- External values come into the EventBroker via the MsgQueue.
-- Registration and deregistration instructions are sent via the
-- SAP channel.

type NR = Registrations -> IO Registrations
-- NR values are provided by registerListener and
-- deregisterListener.  I suppose NR stands for "new registration".

data Message a = Message EventID a (IO ()) 
-- not to be confused with the type Message in Listener!
-- The dispatch function sends a Message along the broker's
-- event queue.  The work of handling events is done by
-- the eventbroker function in a separate thread.
-- The third argument is the "ack" computation, passed from the
-- dispatch function (see SIMClasses.Actor).
-- Note that Message is not exported.

-- --------------------------------------------------------------------------
--  Create EventBroker
-- --------------------------------------------------------------------------

newEventBroker :: Typeable a => IO (EventBroker a)
newEventBroker = 
   do
      inQueue <- newMsgQueue
      registrationMessages <- newSAP
      let result = EventBroker inQueue registrationMessages
      forkIO (eventbroker result emptyFM emptyEVSet emptyEVSet)
      return result

-- --------------------------------------------------------------------------
--  Instantiations
-- --------------------------------------------------------------------------

instance Adaptor EventBroker where
   dispatch (EventBroker inQueue _) eventId val ack = 
      do 
         let eId = toEventID eventId
         debugEId "dispatching " eId
         sendIO inQueue (Message eId val ack)
         debugEId "dispatched " eId
   register (EventBroker _ registrationMessages) eventId mode ack listener = 
      callIO registrationMessages 
         (registerListener eventId mode ack listener)
   deregister (EventBroker _ registrationMessages) eventId ack listener = 
      callIO registrationMessages
          (deregisterListener eventId ack listener)
 
-- --------------------------------------------------------------------------
--  Listening to Events
-- --------------------------------------------------------------------------

awaitEvent :: (EventDesignator e, Typeable a) 
          => EventBroker a -> e -> DispatchMode -> IO () -> IO () -> IA a
awaitEvent broker eventId mode regAck deregAck = 
      interaction eId registerBroker deregisterBroker
-- awaitEvent turns an EventBroker plus a mode and regAck and deregAck's
-- (see SIMClasses.actor for when acks' are performed) into a friendly 
-- interaction.
   where  
      eId = toEventID eventId
      registerBroker = register broker eventId mode regAck
      deregisterBroker = deregister broker eventId deregAck


-- --------------------------------------------------------------------------
--  EventBroker Behaviour
-- --------------------------------------------------------------------------

type EventBrokerLoop a =  
   EventBroker a -> Registrations -> EVSet(EV ()) -> EVSet () -> 
   IO ()
-- eventbroker broker registrations evSetEV evSet
-- syncs repeatedly on a complex combination of events.  There are five
-- possible things that can happen.  This is complicated because of the
-- need to always be able to handle everything.
-- (1) a registration event, IE a registration change request arrives along
--     the registration change channel.  eventbroker uses registrationChanged
--     to alter registration and recurses.
-- (2) An incoming Notice event.  This is a notice which should be
--     accepted and then broadcast to all listeners waiting on that event.
--     So we ack it and then add listener send requests to evSet, then
--     recurse.
-- (3) An incoming Request/OneWay event.  We should let only one listener
--     accept it, then wait for the listener to ack it, and then ack it.
--     We add the listener accept event to evSetEV, doctored to return
--     an additional event which will be added to evSet, to wait for the
--     listener to complete handling, and then ack.
-- (4) An incoming evSet event.  We accept it and then take the new set
--     (which will be the same as the old one, with that deleted.)
-- (5) An incoming evSetEV event.  (This must come from a listener Request,
--     which needs a two-stage acceptance.)  Take the returned event and
--     then add it to the evSet queue; also accept the new evSetEV.
eventbroker :: Typeable a => EventBrokerLoop a
eventbroker broker@(EventBroker inQueue registrationMessages) registrations 
      evSetEV evSet =
   do
      debug "event broker ready to dispatch"
      sync (
            registrationChanged broker registrations >>>= 
               (\ newRegistrations -> 
                  eventbroker broker newRegistrations evSetEV evSet)
         +>  
            receive inQueue >>>= 
                (\ msg -> 
                   delegateEvent msg broker registrations evSetEV evSet)
         +>
            (getEVSetEvent evSetEV) >>>= 
                (\ (newEvent,newEVSetEV) -> 
                   eventbroker broker registrations newEVSetEV 
                      (addEVSet newEvent evSet)
                   )
         +>
            (getEVSetEvent evSet) >>>=
               (\  ((),newEVset) ->
                  eventbroker broker registrations evSetEV newEVset
                  )
         )

delegateEvent :: Typeable a => Message a -> EventBrokerLoop a
delegateEvent msg@(Message eId _ _ ) broker registrations evSetEV evSet = 
   do 
      debugEId "event broker delegating: " eId
      let (mode,listenerList) = getListeners eId registrations
      debug ("to listeners: " ++ 
         concat (map (\i -> show i ++ " ") listenerList))
      case mode of
         Notice -> 
            delegateNotice msg listenerList broker registrations evSetEV evSet
         _  -> 
            delegateRequest mode msg listenerList broker registrations evSetEV evSet
  

delegateRequest :: Typeable a => 
   DispatchMode -> Message a -> [Listener] -> EventBrokerLoop a
delegateRequest mode msg@(Message eId value ack) listenerList 
      broker@(EventBroker inQueue registrationMessages) 
      registrations evSetEV evSet = 
   eventbroker broker registrations
      (addEVSet
         (choose (map tellListener listenerList) >>>=
            (\ listenerHandledEvent -> 
               return(listenerHandledEvent >>> ack)
               )
            )
         evSetEV
         )
      evSet 
   where 
      tellListener :: Listener -> EV (EV ())
      -- tellListener sends the event to the listener and returns
      -- the event corresponding to when the listener has handled it.  
      tellListener listener = 
         delegate mode listener eId value >>> 
         return (awaitReply listener)

delegateNotice :: Typeable a => Message a -> [Listener] -> EventBrokerLoop a
delegateNotice 
      msg@(Message eId value ack) listenerList broker registrations 
      evSetEV evSet = 
   do
      debug "event broker broadcasted notice"
      ack
      debug ("broadcasting to listeners: " ++ 
          concat (map (\i -> show i ++ " ") listenerList))
      let broadcastList = map tellListener listenerList
      eventbroker broker registrations evSetEV 
         (addEVSet (choose broadcastList) evSet)
   where
      tellListener :: Listener -> EV ()
      -- tellListener sends the message to the listener and returns ()
      tellListener listener = 
         delegate Notice listener eId value >>> done

registrationChanged :: EventBroker a -> Registrations -> EV Registrations
-- Listen for registration changed messages and handle them.
registrationChanged (EventBroker _ registrationMessages) registrations = 
   provide registrationMessages
      (\ registrationChange -> 
         do
            debug "event broker changing registration"
            newRegistrations <- registrationChange registrations
            debug "event broker changed registration"
            return ((),newRegistrations)
            -- the () is passed back to the callIO and so returned from
            -- register/deregister.
         )
        
-- --------------------------------------------------------------------------
--  Registration Table
-- --------------------------------------------------------------------------

type Registrations = FiniteMap EventID Listeners

type Listeners = (DispatchMode,[Listener])

registerListener :: EventDesignator e => 
   e -> DispatchMode -> IO () -> Listener -> NR 
-- registerListener eventId mode ack listener oldRegistrations
-- registers the listener for eventId with mode mode.  (This changes
-- all modes for this eventId.)
registerListener eventId mode ack listener oldRegistrations = 
   do
      debugEId "register event" eId
      if (listenerList == ([] :: [Listener]) )
         then
            do 
               try ack
               done
         else
            debug "Multiple registrations"
      return newRegistrations
   where 
      eId = toEventID eventId
      (_,listenerList) = 
         getListeners eId oldRegistrations
      newRegistrations = 
         addToFM oldRegistrations eId (mode,listener:listenerList)


deregisterListener :: EventDesignator e => 
   e -> IO () -> Listener -> NR
-- deregisterListener eventId ack listener oldRegistrations
-- deregisters Listener for the eventId.
deregisterListener eventId ack listener oldRegistrations = 
   do
      debugEId "deregistering event" eId
      if(newListenerList == []) 
         then
            do
               debug "No more listeners"
               try ack
               done
         else
            done
      debugEId "deregistered event" eId     
      return newRegistrations
   where 
      eId = toEventID eventId
      (mode,listenerList) = 
         lookupWithDefaultFM oldRegistrations (Request,[]) eId
      newListenerList = filter (/= listener) listenerList
      newRegistrations  = 
         case newListenerList of
            [] -> delFromFM oldRegistrations eId
            _  -> addToFM oldRegistrations eId (mode,newListenerList)

getListeners :: EventDesignator e => e -> Registrations -> Listeners
getListeners eventId registrations = 
   lookupWithDefaultFM registrations (Notice,[]) (toEventID eventId)

debugEId :: String -> EventID -> IO()
debugEId message eventId = debug (message ++ (show eventId))
