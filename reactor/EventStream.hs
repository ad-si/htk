 {- #########################################################################

MODULE        : EventStream
AUTHOR        : Einar W. Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1998
VERSION       : 0.2
DESCRIPTION   : A variant of the event loop based on first class event
                values (and its funcitonal too).


   ######################################################################### -}

module EventStream (
        Actor(..),
        HasTrigger(..),
        HasBinding(..),
        HasReceiveEV(..),
        HasReceiveIO(..),

        DispatchMode(..),

        EventStream,
        newEventStream
 
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
                fEvents        :: (IA a),
                fInteractions  :: (InterActions a)
                }

type InterActions a = FiniteMap EventID (Action a)


-- --------------------------------------------------------------------------
--  Instantiations
-- --------------------------------------------------------------------------

instance EventListener (EventStream a) where
        toListener (EventStream lst _) = lst
        reply (EventStream lst mv) = do
                replied <- updVar' mv (\ias -> 
                                (ias{fReplied = True},fReplied ias))
                unless replied (reply lst)

instance HasReceiveIO EventStream a where
        receiveIO es = sync (receive es)

instance HasReceiveEV EventStream a where
        receive es =  event (do {
                (IAS _ ev@(IA be _) hs) <- updVar' (state es) (\ias -> 
                                (ias{fReplied = False},ias));
                return (
                        receive (msgchannel lst) >>>= (handleEvent es hs)
                   +>   be done
                   )
                }) where lst = toListener es


instance Eq (EventStream a) where
        es1 == es2 = (toListener es1) == (toListener es2)

instance Ord (EventStream a) where
        es1 <= es2 = (toListener es1) <= (toListener es2)

instance Show (EventStream a) where
        showsPrec d es r = show (toListener es) ++ r

instance Actor (EventStream a) (IA a) where
        become = interaction'

instance HasTrigger EventStream a where
        getTrigger es = withVar' (state es) fEvents

instance HasBinding (EventStream a) a where
        bind es e = do
                e' <- getTrigger es
                become es ((e' \> e) +> e)
        unbind es e =  do
                e' <- getTrigger es
                become es (e' \> e)


-- --------------------------------------------------------------------------
--  Event Stream Creation
-- --------------------------------------------------------------------------

newEventStream :: IO (EventStream a)
newEventStream = do
        lst <- newListener
        mvar <- newMVar (IAS False inaction emptyFM) 
        return (EventStream lst mvar)


-- --------------------------------------------------------------------------
--  Redefining Interaction Pattern
-- --------------------------------------------------------------------------

interaction' :: EventStream a -> IA a -> IO ()
interaction' es ev@(IA _ ias) = do
        (ohs,nhs) <- updVar' (state es) (register (addListToFM emptyFM ias))
        deregisterEvents (fmToList (ohs `minusFM` nhs)) lst
        registerEvents (fmToList (nhs `minusFM` ohs)) lst
 where  register nhs (IAS lst _ ohs) = (IAS lst ev nhs, (ohs,nhs))        
        lst = toListener es


-- --------------------------------------------------------------------------
--  Event Handling
-- --------------------------------------------------------------------------

handleEvent :: EventStream a -> InterActions a -> Message -> IO a
handleEvent es hs (Message Notice eid info) = do
        changeVar' (state es) (\ias -> ias{fReplied = True})
        execReaction hs eid info
handleEvent es hs (Message Oneway eid info) = do 
        reply es
        execReaction hs eid info
handleEvent es hs (Message Request eid info) = do
        ans <- try (execReaction hs eid info)
        reply es
        propagate ans

execReaction :: InterActions a -> EventID -> Dyn -> IO a
execReaction hs eid val =
    case (lookupWithDefaultFM hs missing eid) of
        (_,_,f) -> f val
 where  reg _   = done
        missing = (reg,reg,\_ -> error "EventStream: MISSING Binding")
