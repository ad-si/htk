{- #########################################################################

MODULE        : Listener
AUTHOR        : Einar W. Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1998
VERSION       : 1.0
DESCRIPTION   : 


   ######################################################################### -}


module Listener (
        EventID(..),
        EventPatternID(..),
        EventDesignator(..),
        
        EventListener(..),
        Listener(..),
        Message(..),
        DispatchMode(..),

        newListener,
        request,
        oneway

        ) where


import Concurrency
import Object
import Dynamics
import Event
import Debug(debug)

-- --------------------------------------------------------------------------
--  Event Designators
-- --------------------------------------------------------------------------

data EventID = EventID ObjectID EventPatternID deriving (Eq, Ord, Show)

type EventPatternID = String

class EventDesignator e where
        toEventID :: e -> EventID

instance EventDesignator EventID where
        toEventID = id


-- --------------------------------------------------------------------------
--  Event Listeners
-- --------------------------------------------------------------------------

class EventListener o where
        toListener :: o -> Listener
        delegate   :: (EventDesignator e, Typeable a) 
                   => DispatchMode -> o -> e -> a -> EV ()
        reply      :: o -> IO ()
        awaitReply :: o -> EV ()        
        delegate md o e a = send (msgchannel (toListener o)) msg
                where msg = Message md (toEventID e) (toDyn a)
        reply  o     = sendIO (replychannel (toListener o)) ()
        awaitReply o = receive (replychannel (toListener o))


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

data DispatchMode = Notice | Oneway | Request

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

                        
