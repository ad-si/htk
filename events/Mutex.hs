{- #########################################################################

MODULE        : Mutex
AUTHOR        : Einar W. Karlsen
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

                Reentrant locks solve a major and inherent problem of
                MVar's.

   This module is ghc-specific, as it uses thread ids. For hugs we
   could implement a simple "empty" implementation, which does no
   locking at all, since with cooperative multi-tasking locking is not
   needed (at least not for our purposes).

   ######################################################################### -}


module Mutex (
        Mutex,
        newMutex,
	acquire,
	release
        
        ) where

import Concurrent

import Exception

import qualified Computation

import Synchronized


-- --------------------------------------------------------------------------
-- Data Type
-- --------------------------------------------------------------------------

data Mutex = Mutex (MVar (Maybe (ThreadId,Int), [(ThreadId, MVar ())]))

-- --------------------------------------------------------------------------
-- New Mutex
-- --------------------------------------------------------------------------

newMutex :: IO Mutex
newMutex = do
   mvar <- newMVar (Nothing, [])
   return (Mutex mvar)     

instance Synchronized Mutex where
   synchronize mtx cmd = 
      do
         acquire mtx
         ans <- Computation.try cmd
         release mtx
         Computation.propagate ans

-- --------------------------------------------------------------------------
-- Implementation of Locking Commands
-- --------------------------------------------------------------------------

acquire :: Mutex -> IO ()
acquire (Mutex mvar) = 
   do st <- takeMVar mvar
      current <- myThreadId
      case st of
         (Nothing,[]) -> putMVar mvar (Just (current,1),[])
         (Just (holder,n),pnd) ->
            if current == holder then putMVar mvar (Just (holder,n+1),pnd)
              else do bsem <- newEmptyMVar
                      putMVar mvar (Just (holder,n),(current,bsem):pnd)
                      takeMVar bsem



release :: Mutex -> IO ()
release (Mutex mvar) = 
   do st <- takeMVar mvar
      current <- myThreadId
      case st of
         (Just (holder,n),pnd) | current == holder -> 
            release' mvar holder n pnd
         _ -> do putMVar mvar st
                 Exception.throw illegalMutexRelease
   where 
      release' mvar _ 1 [] = putMVar mvar (Nothing,[])
      release' mvar _ 1 waiting = 
         do let (holder', sem) = last waiting
                pnd'           = tail waiting 
            putMVar mvar (Just (holder',1),pnd')
            putMVar sem ()
      release' mvar holder n pnd = putMVar mvar (Just (holder,n-1),pnd)

{- --- Wonder if anyone still needs this...
tryAcquireMutex :: Mutex -> IO Bool
tryAcquireMutex (Mutex mvar) = 
   do
      st <- takeMVar mvar
      current <- myThreadId
      case st of
         (Nothing,[]) -> 
            do
               "13" @: putMVar mvar (Just (current,1),[])
               return True
         (Just (holder,n),pnd) | current == holder -> 
            do
               "14" @: putMVar mvar (Just (holder,n+1),pnd)
               return True
         _ -> 
            do
               "15" @: putMVar mvar st
               return False

-- -}


-- --------------------------------------------------------------------------
--  Exceptions
-- --------------------------------------------------------------------------

illegalMutexRelease :: Exception
illegalMutexRelease = ErrorCall "Mutex: Illegal release operation"




