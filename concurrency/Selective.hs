{- #########################################################################

MODULE        : Selective
AUTHOR        : Walter Norzel
                Einar W. Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : 1.0
DESCRIPTION   : Selective Communication and base channel events.


   ######################################################################### -}


module Selective (
        module Event,

        HasReceiveEV(..),
        HasSendEV(..),
        HasReceiveIO(..),
        HasSendIO(..),
        Event(..),
        
        EV,

        Channel,
        newChannel,

        MsgQueue,
        newMsgQueue,

        delay,
        timeout,
        withTimeout,
        deadlock,

        event,
        always,
        promise,

        whenGuard,
        unlessGuard
        
        ) where


import Thread
import Variable
import BSem
import Queue
import FiniteMap
import Event

import Debug(debug)

-- --------------------------------------------------------------------------
-- Message Passing Operations
-- --------------------------------------------------------------------------

class HasReceiveEV c a where
        receive          :: c a -> EV a

class HasReceiveIO c a where
        receiveIO        :: c a -> IO a

class  HasSendEV c a where
        send             :: c a -> a -> EV ()

class HasSendIO c a where
        sendIO           :: c a -> a -> IO ()   



-- --------------------------------------------------------------------------
--  Synchronization
-- --------------------------------------------------------------------------

deadlock :: IO a
deadlock = sync (inaction :: EV a)

event :: IO (EV a) -> EV a
event beh = EV [] [beh]

whenGuard :: IO Bool -> EV a -> EV a
whenGuard c e = event (do { b <- c; return (whenEV b e)})

unlessGuard :: IO Bool -> EV a -> EV a          
unlessGuard c e = event (do { b <- c; return (unlessEV b e)})


-- --------------------------------------------------------------------------
-- Promise
-- --------------------------------------------------------------------------

promise :: IO a -> IO (EV a)
promise beh = do
         ch <- newChannel
         forkIO (try beh >>= sync . (send ch))
         return (receive ch >>>= propagate)


-- --------------------------------------------------------------------------
-- Timeouts 
-- --------------------------------------------------------------------------

timeout :: Duration -> EV ()
timeout = event . promise . delay


withTimeout :: Duration -> EV a -> EV (Maybe a)
withTimeout d ev = 
        timeout d >>> return Nothing 
     +> ev >>>= return . Just


-- --------------------------------------------------------------------------
--  Always
-- --------------------------------------------------------------------------

always :: a -> EV a
always = event . promise . return



-- --------------------------------------------------------------------------
-- Instance EV
-- --------------------------------------------------------------------------

instance Functor EV where
        fmap f e  = e >>>= return . f

instance Event EV where
        inaction = EV [] []
        (>>>=) (EV stat dyn) f = EV (map (newstat f) stat) (map (newdyn f) dyn)
                where   newstat f (sync,cont) = (sync,\v -> (cont v) >>= f)
                        newdyn  f beh = beh >>= \eV -> return (eV >>>= f)
        (+>) (EV stat dyn) (EV stat' dyn') = EV (stat ++ stat') (dyn ++ dyn') 
        tryEV  (EV stat dyn) = EV (map newstat stat) (map newdyn dyn)
                where   newstat (sync,cont) = (sync,\v -> tryM (cont v))
                        newdyn  beh = beh >>= return . tryEV
        poll ev = do 
                (EV bev _) <- resolveEV (ev >>>= return . Just) 
                syncBEV bev (POLL (return Nothing))
        sync ev = do 
                (EV bev _) <- resolveEV ev
                syncBEV bev SYNC

-- --------------------------------------------------------------------------
-- Channels
-- --------------------------------------------------------------------------

instance HasReceiveEV Channel a where
        receive ch = receiveChannel ch
         
instance HasReceiveIO Channel a where
        receiveIO c = sync(receive c)
         
instance HasSendEV Channel a where
        send ch v = sendChannel ch v

instance HasSendIO Channel a where
        sendIO c = sync . (send c)


-- --------------------------------------------------------------------------
-- Message Queues
-- --------------------------------------------------------------------------

instance HasReceiveEV MsgQueue a where
        receive mq = receiveMQ mq
         
instance HasReceiveIO MsgQueue a where
        receiveIO c = sync(receive c)
         
instance HasSendIO MsgQueue a where
        sendIO = sendMQ


-- --------------------------------------------------------------------------
-- Computed event resolution
-- --------------------------------------------------------------------------

resolveEV :: EV a -> IO (EV a)
resolveEV (EV e []) = return (EV e [])
resolveEV (EV e (t : tl)) = do
        mev <- try t
        case mev of 
                Left _ -> resolveEV (EV e tl)
                Right (EV e' tl') -> resolveEV (EV (e' ++ e) (tl' ++ tl))


-- --------------------------------------------------------------------------
-- Synchronization
-- --------------------------------------------------------------------------

data SyncProp a = SYNC  | POLL (IO a)

syncBEV :: [BEV a] -> SyncProp a -> IO a
syncBEV bev sp = do
        sel <- newSelection
        syncSelection bev 0 sel [] sp


-- --------------------------------------------------------------------------
-- Selection
-- --------------------------------------------------------------------------

data Result = Immediate  | Awaiting

type Provide a = a -> IO ()

data EV a = EV [BEV a] [IO (EV a)] 

type BEV a = (Choice -> IO Result, Selection -> IO a)
        
data Selection = Selection (MVar Bool) (MVar Int) ThreadID

type Choice = (Selection,Int)

newSelection :: IO Selection
newSelection = do {
        tid <- getThreadID;
        pv <- newMVar True;             -- pending
        mv <- newEmptyMVar;
        return (Selection pv mv tid)
        }



-- --------------------------------------------------------------------------
-- Rendezvous
-- --------------------------------------------------------------------------

data Rendezvous = 
                Synced (IO ()) 
        |       Prohibited              -- cant sync with myself
        |       Rejected                -- partner is busy
        |       Obsolete Bool           -- this thread is hooked up already

rendezvous :: Choice -> Choice -> IO Rendezvous
rendezvous ((Selection rpv rmv rtid),rid) ((Selection lpv lmv ltid),lid) = do {
        if rtid == ltid then
                return Prohibited
        else if rtid > ltid then 
                id( updMVar rpv (\rp -> updVar' lpv (\lp -> 
                        let (lp',rp',c) = handshake lp rp in (lp',(rp',c)))))
        else    id(updMVar lpv (\lp -> updVar' rpv (\rp ->
                        let (lp',rp',c) = handshake lp rp in (rp',(lp',c)))))
} where syncIO = do {putMVar rmv rid; putMVar lmv lid}
        handshake True True = (False,False,Synced syncIO)  -- get ready to rumble
        handshake True False = (True,False,Rejected)  -- other side already did it
        handshake False rp = (False,rp,Obsolete rp)  -- I'm hooked up already           
 


-- --------------------------------------------------------------------------
-- Selective Communication
-- --------------------------------------------------------------------------

type Choices a = [(Int,IO a)]

syncSelection :: [BEV a] -> Int -> Selection -> Choices a -> SyncProp a -> IO a
syncSelection ((try,cont):tcs) id sel choices sp = do
        res <- try (sel,id)
        case res of
           Immediate -> cont sel
           Awaiting -> syncSelection tcs (id+1) sel ((id,cont sel):choices) sp

syncSelection [] id sel choices SYNC =  
        awaitEvent sel choices

syncSelection [] id sel choices (POLL beh) = do {
        pending <- poll sel;
        if pending then 
                beh
        else 
                awaitEvent sel choices
} where poll :: Selection -> IO Bool
        poll (Selection pv _ _) = updVar pv (\pending -> return (False,pending))

{- this will not work (another thread may meanwhile have synced it) -}

awaitEvent :: Selection -> Choices a -> IO a
awaitEvent (Selection _ mv _) choices = do {
        debug "AE1";
        client <- takeMVar mv;
        res <- (find client choices);
        debug "AE2";
        return res;
} where find client = snd . head . (filter (\(id',_) -> id' == client))



-- --------------------------------------------------------------------------
-- Channel
-- --------------------------------------------------------------------------

data Receiver  a = ReceiveSel Choice (Provide a)

data Sender a = SendSel Choice a

data Channel a = Channel (MVar (CST a)) deriving Eq

type CST  a = ([Sender a], [Receiver a])


newChannel :: IO (Channel a)
newChannel = newMVar  ([], []) >>= return . Channel
        

-- --------------------------------------------------------------------------
-- Receive Channel
-- --------------------------------------------------------------------------

receiveChannel  :: Channel a -> EV a
receiveChannel ch = EV [] [c]
 where c = do {
        var <- newEmptyMVar;
        return (EV [((selAwaitChannel ch (putMVar var)), const (takeMVar var))] [])
        }


selAwaitChannel :: Channel a -> Provide a -> Choice -> IO Result
selAwaitChannel (Channel ch) provide reader = 
        updMVar ch (tryReceive provide reader [])



tryReceive :: Provide a -> Choice -> [Sender a] -> CST a -> IO (CST a, Result)
tryReceive provide reader ownSds ([],rcs)  = 
        blockOnReceive (ReceiveSel reader provide) rcs ownSds
tryReceive provide reader ownSds ((send @(SendSel sender ct):sds),rcs) = do {
        rst <- rendezvous sender reader;
        case rst of
                (Synced sync) -> do {
                        sync; syncReceive ct provide (ownSds ++ sds,rcs)}
                Prohibited -> tryReceive provide reader (ownSds++[send]) cst'
                Rejected -> tryReceive provide reader ownSds (sds,rcs)
                (Obsolete True) -> return ((send :sds,rcs),Awaiting)
                (Obsolete False) ->  return (cst',Awaiting)
} where cst' = (sds,rcs)


syncReceive :: a -> Provide a -> CST a -> IO (CST a,Result)
syncReceive ct provide cst = do {provide ct ;return (cst,Immediate)}


blockOnReceive :: Receiver a -> [Receiver a] -> [Sender a] -> IO (CST a,Result)
blockOnReceive read rcs newSds = do {
        rcs' <- queueReceiver [] rcs read;
        return ((newSds,rcs'),Awaiting)
} where queueReceiver :: [Receiver a] -> [Receiver a] -> Receiver a -> 
                                IO [Receiver a] 
        queueReceiver newRcs [] rc = return (newRcs ++ [rc])
        queueReceiver newRcs (rc' @ (ReceiveSel (Selection pv _ _,_) _):rcs) rc = do {
                pending <- getVar pv;
                if pending then
                        queueReceiver (newRcs ++ [rc']) rcs rc
                else 
                        queueReceiver newRcs rcs rc
                }


-- --------------------------------------------------------------------------
-- Send Channel
-- --------------------------------------------------------------------------

sendChannel  :: Channel a -> a -> EV ()
sendChannel ch val = EV [(selSendChannel ch val, const done)] []


selSendChannel :: Channel a -> a -> Choice -> IO Result
selSendChannel (Channel ch) ct sender = updMVar ch (trySend ct sender [])


trySend :: a -> Choice -> [Receiver a] -> CST a -> IO (CST a,Result)
trySend ct sender ownRcs (sds,[]) = 
        blockOnSend (SendSel sender ct) sds ownRcs
trySend ct sender ownRcs (sds,(recv@(ReceiveSel reader provide):rcs))  = do { 
        rst <- rendezvous reader sender;
        case rst of
                (Synced sync) -> do {
                        sync; syncSend ct provide (sds,ownRcs ++ rcs)}  
                Prohibited -> trySend ct sender (ownRcs ++ [recv]) cst'
                Rejected -> trySend ct sender ownRcs cst'
                Obsolete True -> return ((sds,recv:rcs),Awaiting)
                Obsolete False -> return (cst',Awaiting)
} where cst' = (sds,rcs)


syncSend :: a -> Provide a -> CST a -> IO (CST a,Result)
syncSend ct provide cst = do { 
        provide ct;
        return (cst,Immediate)
        }


blockOnSend :: Sender a -> [Sender a] -> [Receiver a] -> IO (CST a,Result)
blockOnSend send sds ownRcs = do {
        sds' <- queueSender [] sds send;
        return ((sds',ownRcs),Awaiting)
} where queueSender :: [Sender a] -> [Sender a] -> Sender a -> IO [Sender a] 
        queueSender newSds [] sc = return (newSds ++ [sc])
        queueSender newSds (sd' @ (SendSel (Selection pv _ _,_) _):sds) sd = do {
                pending <- getVar pv;
                if pending then
                        queueSender (newSds ++ [sd']) sds sd
                else 
                        queueSender newSds sds sd
                }



-- --------------------------------------------------------------------------
-- Message Queue
-- --------------------------------------------------------------------------

data MsgQueue a = MsgQueue (MVar (State a)) deriving Eq

type State a = (Queue a, [Reader a])

data Reader a = Reader Choice (Provide a)


newMsgQueue :: IO (MsgQueue a)
newMsgQueue = newMVar (emptyQ, []) >>= return . MsgQueue 


-- --------------------------------------------------------------------------
-- Synchronization on Message Queue
-- --------------------------------------------------------------------------

synchronise :: Choice -> IO Bool
synchronise ((Selection pv mv tid),id) = 
        updMVar pv (\pending ->
                if pending then do {
                        putMVar mv id;      -- select choice
                        return (False,True) -- close selection, signal success  
                        }
                else 
                        return (False,False)

        )


-- --------------------------------------------------------------------------
-- Receive Message Queue
-- --------------------------------------------------------------------------

receiveMQ  :: MsgQueue a -> EV a
receiveMQ queue = EV [] [c]
 where c = do {
        var <- newEmptyMVar;
        return (EV [((selAwaitMQ queue (putMVar var)), const (takeMVar var))] [])
        }


selAwaitMQ   :: MsgQueue a -> Provide a -> Choice -> IO Result
selAwaitMQ (MsgQueue mut) provide reader =
        updMVar mut (queueOrRead provide reader)


queueOrRead ::  Provide a -> Choice -> State a -> IO (State a, Result)
queueOrRead provide reader (q,pndl) | isEmptyQ q = do {
        pndl' <- queueReader [] pndl (Reader reader provide);
        return ((q,pndl'), Awaiting)
} where queueReader pndl [] reader = return (reverse (reader : pndl))
        queueReader pndl (rd@(Reader reader' _):tail) reader = do {
        res <- synchronise reader';
        if res then
                queueReader (rd:pndl) tail reader
        else
                queueReader pndl tail reader

        }
queueOrRead provide reader (q,pndl) = do {
        res <- synchronise reader;
        if res then 
                let (Just (msg,q')) = removeQ q in do {
                        provide msg; return ((q', pndl), Immediate)}
        else
                return ((q,pndl),Awaiting)
} 


-- --------------------------------------------------------------------------
-- Send Message Queue
-- --------------------------------------------------------------------------

sendMQ :: MsgQueue a -> a -> IO ()
sendMQ (MsgQueue mut) msg = changeMVar mut (queueOrSend msg)

queueOrSend :: a -> State a -> IO (State a)
queueOrSend msg (q,[]) = return (insertQ q msg, [])
queueOrSend msg (q, (Reader reader provide) : pndl) = do {
        res <- synchronise reader; 
        if res then 
                let (Just (msg',q')) = removeQ (insertQ q msg) in do {
                        provide msg'; return (q', pndl)}
        else 
                queueOrSend msg (q, pndl)
} 

-- --------------------------------------------------------------------------
-- Aux
-- --------------------------------------------------------------------------

updMVar mv c = do {
        val <- takeMVar mv;
        (val',res) <- c val;
        putMVar mv val';
        return res
}
-- {-# INLINE updMVar #-}

changeMVar mv c = do {
        val <- takeMVar mv;
        val' <- c val;
        putMVar mv val';
}
-- {-# INLINE changeMVar #-}
