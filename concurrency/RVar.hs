{- #########################################################################

MODULE        : RVar
AUTHOR        : Einar W. Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1998
VERSION       : 0.2
DESCRIPTION   : Simple, reentrant, protected variable. Simple means:
                no condition variable to wake up blocked clients.

                The big advantage however is that this kind of mutable
                variable supports recursion, so once a thread has acquired
                the variable, it may be allowed to re-enter it again
                (and again) as many times as it needs.

TO BE DONE    : Implementation should be optimized (if possible)!


   ######################################################################### -}


module RVar (
        Variable(..),
        Synchronized(..),
        Lock(..),

        RVar,
        newRVar
        
        ) where

import Concurrent(MVar,swapMVar, takeMVar, putMVar, readMVar)
import Thread
import Variable
import Mutex
import Semaphore

import Debug(debug)



-- --------------------------------------------------------------------------
-- Data Type
-- --------------------------------------------------------------------------

data RVar a = RVar Mutex (MVar a)


-- --------------------------------------------------------------------------
-- New RVar
-- --------------------------------------------------------------------------

newRVar :: a -> IO (RVar a)
newRVar val = do { 
        mtx <- newMutex; 
        mvar <- newMVar val; 
        return (RVar mtx mvar)
        }       


-- --------------------------------------------------------------------------
-- Instance: Variable
-- --------------------------------------------------------------------------

instance Eq (RVar a) where
        (RVar _ mv1) == (RVar _ mv2) = mv1 == mv2


instance Variable RVar a where
        setVar (RVar mtx mvar) val = do {
                acquire mtx; 
                takeMVar mvar; 
                putMVar mvar val;
                release mtx
                }
        getVar (RVar mtx mvar) = do {
                acquire mtx; 
                val <- readMVar mvar; 
                release mtx;
                return val
                }
        updVar rp @ (RVar mtx mvar) cmd = do {
                acquire mtx;
                val <- readMVar mvar;
                ans <- try (cmd val);
                case ans of 
                        Left e -> do {release mtx; raise e}
                        Right (val',res) -> do {
                                takeMVar mvar;
                                putMVar mvar val'; 
                                release mtx; 
                                return res
                                }
                }
        updVar' rp @ (RVar mtx mvar) f = do {
                acquire mtx;
                val <- readMVar mvar;
                let (val',res) = f val in do {
                        takeMVar mvar;
                        putMVar mvar val'; 
                        release mtx; 
                        return res
                        }
                }
        withVar rp@(RVar mtx mvar) f = 
                updVar rp (\x -> do {
                        res <- f x;
                        val <- readMVar mvar;
                        return (val,res)
                        })

instance Synchronized (RVar a) where
        synchronize (RVar mtx mvar) c = do
                acquire mtx
                ans <- try c
                release mtx
                propagate ans


instance Lock (RVar a) where
        acquire (RVar mtx mvar) = acquire mtx
        release (RVar mtx mvar) = release mtx

