{- #########################################################################

   A simple semaphore
                
   ######################################################################### -}


module BSem (
   module Lock,
   
   BSem,
   newBSem,
   newLockedBSem
   ) where

import Maybe
 
import Concurrent

import Thread
import Lock

import Debug(debug)

-- --------------------------------------------------------------------------
-- Type
-- --------------------------------------------------------------------------

---
-- A simple lock.
newtype BSem = BSem (MVar ()) deriving Eq

-- --------------------------------------------------------------------------
-- Instances
-- --------------------------------------------------------------------------

instance Lock BSem where
   acquire (BSem sem) = takeMVar sem
   release (BSem sem) = putMVar sem ()
   tryAcquire (BSem sem) = 
      do
         success <- tryTakeMVar sem
         return (isJust success)

instance Synchronized BSem where
   synchronize (BSem sem) c = 
      do
         takeMVar sem
         ans <- try c
         putMVar sem ()
         propagate ans

-- --------------------------------------------------------------------------
-- Commands
-- --------------------------------------------------------------------------

---
-- Create a new unlocked BSem
newBSem :: IO BSem
newBSem = newMVar () >>= return . BSem

---
-- Create a new locked BSem
newLockedBSem   :: IO BSem
newLockedBSem = newEmptyMVar >>= return . BSem

