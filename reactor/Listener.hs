{- #########################################################################

MODULE        : Listener
AUTHOR        : Einar W. Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1998
VERSION       : 1.0
DESCRIPTION   : 


   External events communicate at the very lowest level with
   a Listener.  They do this by sending Message's (type below)
   along a message channel (using for example the
   delegate function), and may ask for an acknowledgement
   using the DispatchMode.

   ######################################################################### -}


module Listener (
   EventID(..),
   EventPatternID,
   EventDesignator(..),
   
   EventListener(..),
   Listener(..),
   Message(..),
   DispatchMode(..),

   newListener,
   request,
   oneway

   ) where

import Computation(done)
import Object
import Dynamics
import Debug(debug)

import Concurrency
import Event

-- --------------------------------------------------------------------------
--  Event Designators
-- --------------------------------------------------------------------------

data EventID = EventID ObjectID EventPatternID deriving (Eq, Ord, Show)
-- The EventID (or an EventDesignator which gives it) seems to be
-- attached to a message to help select an action based on
-- the event.  For example EventBroker contains the line:
--    type Registrations = FiniteMap EventID Listeners
-- EventStream:
--     type InterActions a = FiniteMap EventID (Action a)
-- Expect: 
--     type RST = ([Pattern],FiniteMap EventID [Listener])
type EventPatternID = String

class EventDesignator e where
   toEventID :: e -> EventID

instance EventDesignator EventID where
   toEventID = id


-- --------------------------------------------------------------------------
--  Event Listeners
-- --------------------------------------------------------------------------


-- An EventListener is something that can process values
-- using an Event like interface.  Only the toListener
-- value needs to be defined.
-- Instances:
--    Listener (this file)
--    EventStream (EventStream.hs) (built on Listener).  Also defines reply.
--    InterActor (built on EventStream).  Accepts EventStream's 
--       definition of reply.
-- Example use: Interaction.sync calls newListener to 
-- make a new listener to receive the results of external events.
class EventListener o where
     toListener :: o -> Listener
     -- get basic listener.
     delegate   :: (EventDesignator e, Typeable a) 
                => DispatchMode -> o -> e -> a -> EV ()
     -- event for sending a message to the Listener
     reply      :: o -> IO ()
     -- Function called by the Listener itself ack the message.
     -- This has to be part of the interface so that it can
     -- get overridden.  (For example EventStream uses it to
     -- block certain replies.)
     replyPending :: o -> IO ()
     -- replyPending sends a reply, if one is being waited for.

     awaitReply :: o -> EV ()
     -- get reply event (for when listener consumer acknowledges reply).
     -- Used for example by eventBroker.      
     delegate md o e a = send (msgchannel (toListener o)) msg
             where msg = Message md (toEventID e) (toDyn a)
     reply  o     = sendIO (replychannel (toListener o)) ()
     awaitReply o = receive (replychannel (toListener o))
     replyPending o = 
        do
           poll (send (replychannel (toListener o)) ())
           done

request :: (EventListener o, EventDesignator e, Typeable a) 
                => o -> e -> a -> EV (EV ())
request o e a = delegate Request o e a >>> return (awaitReply o)


oneway :: (EventListener o, EventDesignator e, Typeable a) 
                => o -> e -> a -> EV (EV ())
oneway o e a = delegate Oneway o e a >>> return (awaitReply o)


-- --------------------------------------------------------------------------
--  Listener Instance
-- --------------------------------------------------------------------------

instance EventListener Listener where
   toListener = id


-- --------------------------------------------------------------------------
--  Semantic Domains: Interactor & EventStream
-- --------------------------------------------------------------------------

data Listener = 
   Listener {
      objectid :: ObjectID,
      msgchannel :: (Channel Message),
      replychannel ::  (Channel ())
      }


-- --------------------------------------------------------------------------
-- Messages
-- --------------------------------------------------------------------------

-- The DispatchMode appears to be examined by Interaction.handleEvent
-- with the following meanings:
-- (1) A Notice message should not get a reply along the reply channel.
-- (2) A OneWay message should get an immediate reply.
-- (3) A Request message always gets a reply, once the action associated
--     with the event terminates, even if it raises an exception.  
--     (But not before)
data DispatchMode = Notice | Oneway | Request
-- The EventBroker has something to say about this too.
-- A Notice message will be broadcast to all listeners currently waiting
-- on that event.  A Request/OneWay message will be broadcast to just one
-- listener, and we block until a listener handles that message.

data Message = Message DispatchMode EventID Dyn


-- --------------------------------------------------------------------------
-- Instantiations
-- --------------------------------------------------------------------------

instance Eq Listener where
   lst1 == lst2 = (objectid lst1) == (objectid lst2)

instance Ord Listener where
   lst1 <= lst2 = (objectid lst1) <= (objectid lst2)

instance Show Listener where
   showsPrec d lst r = show (objectid lst) ++ r


-- --------------------------------------------------------------------------
-- Listener
-- --------------------------------------------------------------------------

newListener :: IO Listener 
newListener = do
   oid <- newObject
   ch <- newChannel
   chr <- newChannel
   return (Listener oid ch chr)

                        
