{- #########################################################################

MODULE        : EventBroker
AUTHOR        : Einar W. Karlsen,  
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

type NR = Registrations -> IO Registrations


type Message a = (EventID, a, IO ()) 


-- --------------------------------------------------------------------------
--  Create EventBroker
-- --------------------------------------------------------------------------

newEventBroker :: Typeable a => IO (EventBroker a)
newEventBroker = do
        che <- newMsgQueue
        chr <- newSAP
        forkIO (eventbroker (EventBroker che chr) emptyFM)
        return (EventBroker che chr)


-- --------------------------------------------------------------------------
--  Instantiations
-- --------------------------------------------------------------------------

instance Adaptor EventBroker where
        dispatch (EventBroker che _) ev val c = do {
                iact <- getThreadID;
                writeLog ("dispatching " ++ show (toEventID ev));
                sendIO che (toEventID ev,val,c);
                writeLog ("dispatched " ++ show (toEventID ev));
                }
        register (EventBroker _ chr) ev md cmd iact = 
                sync (call chr (registerListener ev md cmd iact))
        deregister (EventBroker _ chr) ev cmd iact = 
                sync (call chr (deregisterListener ev cmd iact))
 


-- --------------------------------------------------------------------------
--  Listening to Events
-- --------------------------------------------------------------------------

awaitEvent :: (EventDesignator e, Typeable a) 
          => EventBroker a -> e -> DispatchMode -> IO () -> IO () -> IA a
awaitEvent brk ev md reg unreg = interaction eid registerBroker deregisterBroker
 where  eid = toEventID ev
        registerBroker     = register brk eid md reg
        deregisterBroker   = deregister brk eid unreg


-- --------------------------------------------------------------------------
--  EventBroker Behaviour
-- --------------------------------------------------------------------------

type DP a =  EventBroker a -> Registrations -> IO ()

eventbroker :: Typeable a => DP a
eventbroker brk@(EventBroker che chr) rs = do {
  writeLog "event broker ready to dispatch";
  sync (
        registrationChanged brk rs >>>= eventbroker brk
   +>   receive che >>>= \msg -> delegateEvent msg brk rs
   )
}


delegateEvent :: Typeable a => Message a -> DP a
delegateEvent msg@(eid,_,_) brk rs = do {
        writeLog ("event broker delegating: " ++ show eid);
        (mode,lsts) <- return (getListeners eid rs);
        writeLog ("to listeners: " ++ concat (map (\i -> show i ++ " ") lsts));
        case mode of
                Notice -> delegateNotice msg lsts [] brk rs
                md     -> delegateRequest md msg lsts brk rs
} 

delegateRequest :: Typeable a => DispatchMode -> Message a -> [Listener] -> DP a
delegateRequest md msg@(eid,v,c) lst brk@(EventBroker che chr) rs = sync (
     choose (map call lst)      >>>= (\ev -> awaitReply' c ev brk rs) 
 +>  registrationChanged brk rs >>>= delegateEvent msg brk
 ) where call iact = delegate md iact eid v >>> return (awaitReply iact)



awaitReply' :: Typeable a => IO () -> EV () -> DP a
awaitReply' c reply brk rs = do {
   writeLog "event broker awaiting reply";
   sync (
        reply                      >>>  do {c; eventbroker brk rs}
     +> registrationChanged brk rs >>>= awaitReply' c reply brk
     )
}


delegateNotice :: Typeable a => Message a -> [Listener] -> [Listener] -> DP a
delegateNotice msg@(eid,v,c) [] srv brk rs = do
        writeLog "event broker broadcasted notice";
        c
        eventbroker brk rs      
delegateNotice msg@(eid,v,c) lsts srv brk rs = do {
    writeLog ("broadcasting to listeners: " ++ concat (map (\i -> show i ++ " ") lsts));
    sync (
        choose (map call lsts) >>>= (\iact -> 
                delegateNotice msg (remove iact lsts) (iact:srv) brk rs) 
    +>  registrationChanged brk rs >>>= \rs' ->  
                delegateNotice msg (listeners rs') srv brk rs'
    ) 
} where  call iact = delegate Notice iact eid v >>> return iact
         remove iact = filter (/= iact)
         listeners rs' = filter (\lst -> notElem lst srv)(snd (getListeners eid rs'))


registrationChanged :: EventBroker a -> Registrations -> EV Registrations
registrationChanged (EventBroker _ chr) rs = provide chr (\f -> do {
        writeLog "event broker changing registration";
        rs' <- f rs;
        writeLog "event broker changed registration";
        return ((),rs')
        })
        
-- --------------------------------------------------------------------------
--  Registration Table
-- --------------------------------------------------------------------------

type Registrations = FiniteMap EventID Listeners

type Listeners = (DispatchMode,[Listener])

registerListener :: EventDesignator e 
        => e 
        -> DispatchMode 
        -> IO () 
        -> Listener 
        -> Registrations 
        -> IO Registrations
registerListener  ev md reg iact rs = do {
        writeLog ("register event" ++ show eid);
        when (lst == []) (do {try reg; done});
        return rs'
} where eid = toEventID ev
        (_,lst) = lookupWithDefaultFM  rs (md,[]) eid 
        rs' = addToFM rs eid (md,iact : lst)


deregisterListener :: EventDesignator e 
        => e 
        -> IO () 
        -> Listener 
        -> Registrations 
        -> IO Registrations
deregisterListener ev dereg iact rs = do {
        writeLog ("deregister event" ++ show eid);
        when (lst'' == []) (writeLog "no more listeners"); 
        when (lst'' == []) (do {try dereg; done}); 
        writeLog ("deregisted event" ++ show eid);      
        return rs'
} where eid = toEventID ev
        (md,lst) = lookupWithDefaultFM rs (Request,[]) eid
        (rs',lst'') = case filter (/= iact) lst of
                [] ->   (delFromFM rs eid,[])
                lst' -> (addToFM rs eid (md,lst'),lst')


getListeners :: EventDesignator e => e -> Registrations -> Listeners
getListeners ev rs = lookupWithDefaultFM rs (Notice,[]) (toEventID ev)


