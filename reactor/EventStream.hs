 {- #########################################################################

MODULE        : EventStream
AUTHOR        : Einar W. Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1998
VERSION       : 0.2
DESCRIPTION   : A variant of the event loop based on first class event
                values (and its funcitonal too).

NOTATION.  An "Event" in this file denotes something of type 
   IA.  Interactions is a map from EventID's to Actions.
   An EventStream is a triple of a reply flag, an Event and Interactions.
   interaction'/become changes the Event.

   Question - Einar's code is arranged to only reply once.  Surely this
   is not right??  But if we change this things block.

   Suggestion - add a non-blocking reply.


Interaction.hs implements IA's as things you sync/poll on.
EventStream.hs/InterActor.hs implements extra glue around IA's (introducing
   a finite map) and makes them things you interact on.  There is no
   real reason I can see why we don't just write an interactor using
   the code in this file, except efficiency.

   
   ######################################################################### -}

module EventStream (
   Actor(..),
   EventStream,
   newEventStream,
   HasTrigger(..), -- defined in SIMClasses
   HasBinding(..), -- defined in SIMClasses
   HasReceiveEV(..), -- defined in Channels
   HasReceiveIO(..), -- defined in Channels

   DispatchMode(..) -- defined in Listener


   ) where

import Concurrency
import Interaction
import ExternalEvent
import Listener
import SIMClasses
import FiniteMap
import Dynamics
import Debug(debug)


-- --------------------------------------------------------------------------
--  Class Actor
-- --------------------------------------------------------------------------

class Actor a f where
-- The only instance of Actor is 
-- instance Actor (EventStream a) (IA a)
-- defined later in this file.
   become :: a -> f -> IO () 

-- --------------------------------------------------------------------------
--  Event Stream Handle
-- --------------------------------------------------------------------------

data EventStream a = EventStream {
   listener      :: Listener,
   state :: (MVar (IAS a))
   }

-- --------------------------------------------------------------------------
--  Registrations
-- --------------------------------------------------------------------------

data IAS a = 
   IAS {
      fReplied       :: Bool,
      fEvents        :: (IA a), -- contains internal and external events 
      fInteractions  :: (InterActions a) -- contains data on external events
      -- including their continuations.
      }

type InterActions a = FiniteMap EventID (Action a)


-- --------------------------------------------------------------------------
--  Instantiations
-- --------------------------------------------------------------------------

instance EventListener (EventStream a) where
   toListener (EventStream listener _) = listener
   reply (EventStream lst mv) = 
      do
         replied <- updVar' mv (\ias -> (ias{fReplied = True},fReplied ias))
         unless replied (replyPending lst)

instance HasReceiveEV EventStream messageType where
   receive eventStream = 
   -- Interactors run by repeatedly synccing on this event. 
      event (
         do
            IAS {fEvents = ev@(IA internals _),fInteractions = interactions}
               <- 
               updVar' 
                  (state eventStream) 
                  (\ias -> (ias {fReplied = False},ias))
               -- unset fReplied.
            let
               newEvent =
                     receive (msgchannel listener) >>>=
                        (\ result ->
                           do
                              valueOpt <-
                                 handleEvent eventStream interactions result
                              case valueOpt of 
                                 Just value -> return value
                                 Nothing -> sync newEvent
                           )
                  +> internals done
            return newEvent
         ) where listener = toListener eventStream


instance Eq (EventStream a) where
   es1 == es2 = (toListener es1) == (toListener es2)

instance Ord (EventStream a) where
   es1 <= es2 = (toListener es1) <= (toListener es2)

instance Show (EventStream a) where
   showsPrec d es r = show (toListener es) ++ r

instance Actor (EventStream a) (IA a) where
   become = interaction'

instance HasTrigger EventStream a where
   getTrigger eventStream = withVar' (state eventStream) fEvents

instance HasBinding (EventStream a) a where
-- bind :: EventStream a -> IA a -> IO ()
   bind es e = 
      do
         e' <- getTrigger es
         become es ((e' \> e) +> e)

-- unbind :: EventStream a -> IA a -> IO ()
   unbind es e =  
      do
         e' <- getTrigger es
         become es (e' \> e)


-- --------------------------------------------------------------------------
--  Event Stream Creation
-- --------------------------------------------------------------------------

newEventStream :: IO (EventStream a)
newEventStream = do
   listener <- newListener
   mvar <- newMVar 
      (IAS {fReplied = False,fEvents = inaction,fInteractions = emptyFM}) 
   return (EventStream listener mvar)


-- --------------------------------------------------------------------------
--  Redefining Interaction Pattern
-- --------------------------------------------------------------------------

interaction' :: EventStream a -> IA a -> IO ()
-- otherwise known as become.
interaction' eventStream nEvent@(IA _ externals) =
-- (1) The nEvent gets put into the fEvents box of the IAS.
-- (2) The external events in nEvent also replace those in the fInteractions
--     box
-- (3) We first deregister and then re-register, hopefully ensuring the
--     continuing truth of the invariant that the listener is registered
--     for the events listed in the InterActions map.
   do
      let
         packEvent (x,y) = EE x y
         unpackEvent (EE x y) = (x,y)
      debug "interactions' 1"
      (oldInteractions,newInteractions) <- 
         updVar' 
            (state eventStream) 
            (register (addListToFM emptyFM (map unpackEvent externals)))
      debug "interactions' 2"
      let 
         toDeregister = fmToList (oldInteractions `minusFM` newInteractions)
         toRegister = fmToList (newInteractions `minusFM` oldInteractions)
      debug ("Deregistering "++show (length toDeregister) ++
             " registering " ++ show (length toRegister))
      deregisterEvents (map packEvent toDeregister) listener
      debug "interactions' 3"
      registerEvents (map packEvent toRegister) listener
      debug "interactions' 4"
   where  
      register newInteractions (
         IAS{fReplied = replied,fInteractions = oldInteractions}) = 
         (IAS{
            fReplied = replied,
            fEvents = nEvent,
            fInteractions=newInteractions},
         (oldInteractions,newInteractions))       
      listener = toListener eventStream


-- --------------------------------------------------------------------------
--  Event Handling
-- --------------------------------------------------------------------------

handleEvent :: EventStream a -> InterActions a -> Message -> IO (Maybe a)
handleEvent eventStream interActions (Message Notice eventId info) = 
   do
      changeVar' (state eventStream) (\ias -> ias{fReplied = True})
      execReaction interActions eventId info
handleEvent eventStream interActions (Message Oneway eventId info) = do 
        reply eventStream
        execReaction interActions eventId info
handleEvent eventStream interActions (Message Request eventId info) = do
        ans <- try (execReaction interActions eventId info)
        reply eventStream
        propagate ans

execReaction :: InterActions a -> EventID -> Dyn -> IO (Maybe a)
execReaction interActions eventId val =
   case (lookupFM interActions eventId) of
      Just (Action _ _ continuation) -> 
         do
            result <- continuation val
            return (Just result)
      Nothing -> return Nothing
