{- #########################################################################

MODULE        : EventLoop
AUTHOR        : Einar W. Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : 1.0
DESCRIPTION   : Emulation of callbacks.

                A event loop is a specialised interactor handling
                a number of callbacks. Each callback is identified
                uniquely by some ordinal value. The callback server
                is parameterized by this type.

                Each callback has an associated first class event,
                specifying the interaction pattern of the callback. The
                associated event can be chng at runtime.


   ######################################################################### -}


module EventLoop (
        EV,
        ObjectID,

        EventLoop,
        newEventLoop,
        enterEventLoop,

        EventHandlerDesignator(..),
        registerEH,
        deregisterEH,
        lookupEH
        
        ) where

import Concurrency
import Interaction
import InterActor
import FiniteMap
import SIMClasses(Destructible(..))
import Debug(debug)


-- --------------------------------------------------------------------------
--  Data Type
-- --------------------------------------------------------------------------

data (Ord a) => EventLoop a = EventLoop InterActor (SAP Bool ()) (PVar (EST a))

type EST a = (Bool,Bool,FiniteMap a (IA ()),EV ())
        -- (dispatching, changed handler set, event handlers, destroyed)

type EHS a = FiniteMap a (IA ())



-- --------------------------------------------------------------------------
-- Instantiations
-- --------------------------------------------------------------------------

instance Destructible (EventLoop a) where
        destroy (EventLoop _ ch pv) = do
                sync(call ch False)
                changeVar' pv (\(act,chng,ehs,_) -> (act,chng,ehs,always ()))
        destroyed (EventLoop _ ch pv) = 
                event (getVar pv >>= \(_,_,_,dev) -> return dev) |>> done


-- --------------------------------------------------------------------------
--  EventLoop Server
-- --------------------------------------------------------------------------

newEventLoop :: Ord a => IO (EventLoop a)
newEventLoop = do
        pv <- newPVar (False,False,emptyFM,inaction)
        ch <- newSAP
        iact <- newInterActor (eventloop [] pv ch)
        return (EventLoop iact ch pv)


enterEventLoop :: Ord a => EventLoop a -> IO ()
enterEventLoop el@(EventLoop _ _ pv) = do
        changeVar' pv (\(_,chng,ehs,dev) -> (True,True,ehs,dev))
        changeEH el id


-- --------------------------------------------------------------------------
--  Binding and Unbinding Event Handlers
-- --------------------------------------------------------------------------

class Ord b => EventHandlerDesignator a b where
        toHandlerID  :: a -> IO b
        registerEH   :: a -> IA () -> EventLoop b -> IO ()
        deregisterEH :: a -> EventLoop b -> IO ()
        registerEH o e el = do 
                hid <- toHandlerID o
                changeEH el (\ehs -> addToFM ehs hid e)
        deregisterEH o el = do
                hid <- toHandlerID o
                changeEH el (\ehs -> delFromFM ehs hid)


instance EventHandlerDesignator ObjectID ObjectID where
        toHandlerID = return . id

changeEH :: Ord a => EventLoop a -> (EHS a -> EHS a) -> IO ()
changeEH cbm @ (EventLoop iact ch pv) f = do
        act <- updVar' pv (\(act,_,ehs,dev) -> ((act,True,f ehs,dev),act))
        tid <- getThreadID
        when (act && (tid /= (self iact))) (sync(call ch True))

lookupEH :: Ord a => a -> EventLoop a -> IO (Maybe (IA ()))
lookupEH oid (EventLoop _ _ pv) = withVar' pv (\(_,_,ehs,_) -> lookupFM ehs oid)


-- --------------------------------------------------------------------------
--  Underlying Event Polling Loop
-- --------------------------------------------------------------------------

eventloop :: Ord a => [IA ()] -> PVar (EST a) -> SAP Bool () -> InterActor ->  IA ()
eventloop evs pv ch iact = 
        choose evs >>>  do {checkEH iact pv ch True; done}
   +>   provide ch (checkEH iact pv ch) |>>= \msg -> unless msg (stop iact)


checkEH :: Ord a => InterActor -> PVar (EST a) -> SAP Bool () -> Bool -> IO ((),Bool)
checkEH iact pv ch msg = do
        (act,chng,ehs) <- updVar' pv (\(act,chng,ehs,dev) ->
                ((act,not act,ehs,dev),(act,chng,ehs)))
        when (act && chng) (become iact (eventloop (eltsFM ehs) pv ch iact))
        return ((),msg)
