{- #########################################################################

   A simple semaphore
                
   ######################################################################### -}


module BSem (
   module Lock,
   
   BSem,
   newBSem,
   newLockedBSem,

   tryAcquireBSems,
   tryAcquireBSemsWithError,
   ) where

import Maybe
 
import Concurrent

import Thread
import Lock

import Computation
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


-- --------------------------------------------------------------------------
-- Utilities
-- --------------------------------------------------------------------------

---
-- tryAcquireBSems attempts to acquire a list of BSems.  If successful it
-- returns the action to release them all again.  If unsuccessful it
-- returns Nothing, and leaves all the BSems released.
tryAcquireBSems :: [BSem] -> IO (Maybe (IO ()))
tryAcquireBSems [] = return (Just done)
tryAcquireBSems (bsem:bsems) =
   do
      acquire1 <- tryAcquire bsem
      if acquire1
         then
            do
               acquires <- tryAcquireBSems bsems
               case acquires of
                  Just releaseAct ->
                     return (Just (
                        do
                           releaseAct
                           release bsem
                        ))
                  Nothing ->
                     do
                        release bsem
                        return Nothing
         else
            return Nothing   
---
-- tryAcquireBSemsWithError is a generalisation of tryAcquireBSems, which
-- produces an error message
--
-- The first argument extracts an object's BSem; the second a String to
-- be used as a message if we can't get the object's lock.
tryAcquireBSemsWithError :: (object -> BSem) -> (object -> String) -> [object]
   -> IO (WithError (IO ()))
tryAcquireBSemsWithError _ _ [] = return (hasValue done)
tryAcquireBSemsWithError toBSem toMess (object:objects) =
   do
      let
         bsem = toBSem object
      acquire1 <- tryAcquire bsem
      if acquire1
         then
            do
               acquires <- tryAcquireBSemsWithError toBSem toMess objects
               case fromWithError acquires of
                  Right releaseAct ->
                     return (hasValue (
                        do
                           releaseAct
                           release bsem
                        ))
                  Left _ ->
                     do
                        release bsem
                        return acquires
         else
            return (hasError (toMess object))
   
