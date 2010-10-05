-- |
-- Description: Simple Lock
--
-- A simple semaphore
module Reactor.BSem (
   BSem,
   newBSem,
   newLockedBSem,

   tryAcquireBSems,
   tryAcquireBSemsWithError,
   tryAcquireBSemsWithError1,
   ) where

import Data.Maybe

import Control.Concurrent.MVar

import Util.Computation
import Events.Synchronized
import Reactor.Lock

-- --------------------------------------------------------------------------
-- Type
-- --------------------------------------------------------------------------

-- | A simple lock.
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

-- | Create a new unlocked BSem
newBSem :: IO BSem
newBSem = newMVar () >>= return . BSem

-- | Create a new locked BSem
newLockedBSem   :: IO BSem
newLockedBSem = newEmptyMVar >>= return . BSem


-- --------------------------------------------------------------------------
-- Utilities
-- --------------------------------------------------------------------------

-- | tryAcquireBSems attempts to acquire a list of BSems.  If successful it
-- returns the action to release them all again.  If unsuccessful it
-- returns Nothing, and leaves all the BSems released.
tryAcquireBSems :: [BSem] -> IO (Maybe (IO ()))
tryAcquireBSems bSems =
   do
      let
         toMess _ = return ""
      actWithError <- tryAcquireBSemsWithError id toMess bSems
      return (case fromWithError actWithError of
         Left _ -> Nothing
         Right act -> Just act
         )

-- | tryAcquireBSemsWithError is a generalisation of tryAcquireBSems, which
-- produces an error message
--
-- The first argument extracts an object\'s BSem; the second gets a String to
-- be used as a message if we can\'t get the object\'s lock.
tryAcquireBSemsWithError :: (object -> BSem) -> (object -> IO String)
   -> [object] -> IO (WithError (IO ()))
tryAcquireBSemsWithError toBSem toMess objects =
   let
      getBSem object = return (toBSem object)
      getMessIfError object =
         do
            mess <- toMess object
            return (Just mess)
   in
      tryAcquireBSemsWithError1 getBSem getMessIfError objects

-- | tryAcquireBSemsWithError1 toBSem getMessIfError objects
-- attempts to acquire the BSems in (map toBSem objects).  In
-- the event of a (toBSem object) already being acquired, it looks at
-- the result of getMessIfError object.  If this is (Just mess)
-- it returns an error condition with message (mess), first
-- releasing all BSems it has already acquired; if it is (Nothing)
-- it goes on to attempt to acquire the BSems for the remaining objects.
-- If it gets to the end of the list it returns an action which can be
-- used to release all the BSems it has acquired.
tryAcquireBSemsWithError1 ::
   (object -> IO BSem) -> (object -> IO (Maybe String)) -> [object]
   -> IO (WithError (IO ()))
tryAcquireBSemsWithError1 _ _ [] = return . return $ done
tryAcquireBSemsWithError1 getBSem getMessIfError (object:objects) =
   do
      bSem <- getBSem object
      acquire1 <- tryAcquire bSem
      if acquire1
         then
            do
               acquires
                  <- tryAcquireBSemsWithError1 getBSem getMessIfError objects
               case fromWithError acquires of
                  Right releaseAct ->
                     return (return (
                        do
                           releaseAct
                           release bSem
                        ))
                  Left _ ->
                     do
                        release bSem
                        return acquires
         else
            do
               errorMessIfError <- getMessIfError object
               case errorMessIfError of
                  Just errorMess -> return (fail errorMess)
                  Nothing ->
                     tryAcquireBSemsWithError1 getBSem getMessIfError objects


