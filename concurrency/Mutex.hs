{- #########################################################################

MODULE        : Mutex
AUTHOR        : Einar W. Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1998
VERSION       : 0.2
DESCRIPTION   : A reentrant lock. Once a thread has acquired the mutex,
                it may acquire it as many times as it wants. The thread
                is furthermore allowed to release the mutex exactly as many 
                times as it has acquired it. The last release will then
                make the lock available to any threads pending on a 
                acquire operation.

                Reentrant locks solves a major and inherent problem of
                MVar's.

 CAVEAT       :  Waiting threads are released according to a FIFO rather
                 than a LIFO scheme, out of performance reasons. 



   ######################################################################### -}


module Mutex (
        module Lock,

        Mutex,
        newMutex
        
        ) where

import Thread
import Variable
import Lock
import BSem

import Debug(debug)


-- --------------------------------------------------------------------------
-- Data Type
-- --------------------------------------------------------------------------

data Mutex = Mutex (MVar LST) deriving Eq

type LST = (Maybe (ThreadID,Int), [(ThreadID,BSem)])

-- --------------------------------------------------------------------------
-- New Mutex
-- --------------------------------------------------------------------------

newMutex :: IO Mutex
newMutex = do
   mvar <- newMVar (Nothing, [])
   return (Mutex mvar)     

-- --------------------------------------------------------------------------
-- Instance: Lock
-- --------------------------------------------------------------------------

instance Lock Mutex where
   acquire = acquireMutex
   release = releaseMutex

instance HasTryAcquire Mutex where
   tryAcquire = tryAcquireMutex

instance Synchronized Mutex where
   synchronize mtx cmd = 
      do
         acquireMutex mtx
         ans <- try cmd
         releaseMutex mtx
         propagate ans

-- --------------------------------------------------------------------------
-- Implementation of Locking Commands
-- --------------------------------------------------------------------------

acquireMutex :: Mutex -> IO ()
acquireMutex (Mutex mvar) = 
   do
      st <- takeMVar mvar
      current <- getThreadID
      case st of
         (Nothing,[]) -> 
            putMVar mvar (Just (current,1),[])
         (Just (holder,n),pnd) | current == holder -> 
            putMVar mvar (Just (holder,n+1),pnd)
         (Just (holder,n),pnd) -> 
            do
               bsem <- newLockedBSem
               putMVar mvar (Just (holder,n),(current,bsem):pnd)
               acquire bsem

releaseMutex :: Mutex -> IO ()
releaseMutex (Mutex mvar) = 
   do
      st <- takeMVar mvar
      current <- getThreadID;
      case st of
         (Just (holder,n),pnd) | current == holder -> 
            release' mvar holder n pnd
         _ ->
            do
               putMVar mvar st
               raise illegalLockRelease
   where 
      release' mvar _ 1 [] = putMVar mvar (Nothing,[])
      release' mvar _ 1 ((holder',sem):pnd') = 
         do
            putMVar mvar (Just (holder',1),pnd')
            release sem
      release' mvar holder n pnd = putMVar mvar (Just (holder,n-1),pnd)

tryAcquireMutex :: Mutex -> IO Bool
tryAcquireMutex (Mutex mvar) = 
   do
      st <- takeMVar mvar
      current <- getThreadID
      case st of
         (Nothing,[]) -> 
            do
               putMVar mvar (Just (current,1),[])
               return True
         (Just (holder,n),pnd) | current == holder -> 
            do
               putMVar mvar (Just (holder,n+1),pnd)
               return True
         _ -> 
            do
               putMVar mvar st
               return False



