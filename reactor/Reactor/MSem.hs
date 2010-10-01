-- |
-- Description: Reentrant Lock
--
-- This is a much simpler reimplementation of Einar's old Mutex semaphores.
-- This is a lock which can be required by a thread which is already holding
-- it.
--
-- See also "TSem".
module Reactor.MSem(
   MSem,
   newMSem, -- :: IO MSem
   synchronizeWithChoice,
      -- :: MSem -> (Bool -> IO a) -> IO a
      -- Lock on the MSem.  The action is given True iff this thread
      -- already holds the lock on the MSem.
   ) where

import Data.IORef
import Control.Concurrent

import Util.Computation
import Events.Synchronized
import Reactor.Lock
import Reactor.BSem

data MSem = MSem {
   lock :: BSem,
   holdingThreadRef :: IORef (Maybe ThreadId)
      -- only written when BSem is held by this thread.
   }

newMSem :: IO MSem
newMSem =
   do
      lock <- newBSem
      holdingThreadRef <- newIORef Nothing
      return (MSem {lock = lock,holdingThreadRef = holdingThreadRef})

synchronizeWithChoice :: MSem -> (Bool -> IO a) -> IO a
synchronizeWithChoice mSem toAct =
   do
      holdingThreadOpt <- readIORef (holdingThreadRef mSem)
      thisThread <- myThreadId
      let
         heldByUs = case holdingThreadOpt of
            Nothing -> False
            Just holdingThread -> holdingThread == thisThread
      if heldByUs
         then
            (toAct True)
         else
            do
               acquire (lock mSem)
               writeIORef (holdingThreadRef mSem) (Just thisThread)
               actOut <- try (toAct False)
               writeIORef (holdingThreadRef mSem) Nothing
               release (lock mSem)
               propagate actOut

instance Synchronized MSem where
   synchronize mSem act = synchronizeWithChoice mSem (const act)
