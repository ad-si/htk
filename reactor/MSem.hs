{- This is a much simpler reimplementation of Einar's old Mutex semaphores.  
   This is a lock which can be required by a thread which is already holding
   it.

   See also util/TSem.hs.
   -}
module MSem(
   MSem,
   newMSem, -- :: IO MSem
   ) where

import Data.IORef
import Control.Concurrent

import Computation

import Synchronized

import BSem

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

instance Synchronized MSem where
   synchronize mSem act =
      do
         holdingThreadOpt <- readIORef (holdingThreadRef mSem)
         thisThread <- myThreadId
         let
            heldByUs = case holdingThreadOpt of
               Nothing -> False
               Just holdingThread -> holdingThread == thisThread
         if heldByUs
            then
               act
            else
               do
                  acquire (lock mSem)
                  writeIORef (holdingThreadRef mSem) (Just thisThread)
                  actOut <- try act
                  writeIORef (holdingThreadRef mSem) Nothing
                  release (lock mSem)
                  propagate actOut
                  