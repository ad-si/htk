module Main (
        main 
        ) 
where

import Thread
import Variable
import Queue
import Debug(debug)

main :: IO ()
main = do {
        ch <- newMsgQueue;
        forkIO(producer ch count);
        consumer ch count;
} 
  where count = 100000
        producer ch 0 = print "producer done"
        producer ch n = do {
                transmit ch (); 
                producer ch (n-1)
                }
        consumer ch 0 = print "consumer done"
        consumer ch ne = do {
                receive ch;
                consumer ch (ne-1)
                }




-- --------------------------------------------------------------------------
-- Selection
-- --------------------------------------------------------------------------

data Result = Immediate  | Awaiting

type Provide a = a -> IO ()
        
data Selection = Selection (MVar Bool) (MVar Int)

type Choice = (Selection,Int)

newSelection :: IO Selection
newSelection = do {
        pv <- newMVar True;             -- pending
        mv <- newEmptyMVar;
        return (Selection pv mv)
        }



-- --------------------------------------------------------------------------
-- Message Queue
-- --------------------------------------------------------------------------

data MsgQueue a = MsgQueue (MVar (State a))

type State a = (Queue a, [Reader a])

data Reader a = Reader Choice (Provide a)


newMsgQueue :: IO (MsgQueue a)
newMsgQueue = newMVar (emptyQ, []) >>= return . MsgQueue 


-- --------------------------------------------------------------------------
-- Synchronization on Message Queue
-- --------------------------------------------------------------------------

synchronise :: Choice -> IO Bool
synchronise ((Selection pv mv),id) = 
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

receive  :: MsgQueue a -> IO a
receive queue = do {
        var <- newEmptyMVar;
        sel <- newSelection;
        res <- selAwaitMQ queue (putMVar var) (sel,1);
        case res of
           Immediate -> takeMVar var
           Awaiting -> awaitEvent sel (takeMVar var)
        }

awaitEvent :: Selection -> IO a -> IO a
awaitEvent (Selection _ mv) read = do {
        cid <- takeMVar mv;
        read
} 


selAwaitMQ   :: MsgQueue a -> Provide a -> Choice -> IO Result
selAwaitMQ (MsgQueue mut) provide reader =
        updMVar mut (queueOrRead provide reader)


queueOrRead ::  Provide a -> Choice -> State a -> IO (State a, Result)
queueOrRead provide reader (q,pndl) | isEmptyQ q = 
        return ((q, pndl ++ [Reader reader provide]), Awaiting)
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

transmit :: MsgQueue a -> a -> IO ()
transmit (MsgQueue mut) msg = changeMVar mut (queueOrSend msg)

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

changeMVar mv c = do {
        val <- takeMVar mv;
        val' <- c val;
        putMVar mv val';
}



