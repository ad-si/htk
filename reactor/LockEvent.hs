-- | This module provides a function for /locking/ and /unlocking/ an event,
-- so that it is not handled (but delayed).  NB - there is nothing to
-- stop an event being locked several times at once. 
module LockEvent(
   EventLock,
   lockEvent,
   newEventLock,
   ) where

import Exception

import Events
import Channels
import Synchronized

import Lock

-- -------------------------------------------------------------------------
-- The type
-- -------------------------------------------------------------------------

-- | Sending True locks the event, False unlocks it.
newtype EventLock = EventLock (Channel Bool)

instance Lock EventLock where
   acquire (EventLock channel) = sendIO channel True
{-   acquire (EventLock channel) = sync (noWait (send channel True)) -}
   release (EventLock channel) = sendIO channel False

newEventLock :: IO EventLock
newEventLock =
   do
      channel <- newChannel
      return (EventLock channel)

instance Synchronized EventLock where
   synchronize lock action =
      do
         acquire lock
         finally action (release lock)

-- -------------------------------------------------------------------------
-- Locking an event
-- -------------------------------------------------------------------------

theLockEvent :: EventLock -> Int -> Event ()
theLockEvent (lock@(EventLock channel)) counter =
   do
      lockMessage <- receive channel
      if lockMessage
         then
            theLockEvent lock (counter + 1)
         else
            case compare counter 1 of
               GT -> theLockEvent lock (counter - 1)
               EQ -> return ()
               LT -> error "LockEvent: release event applied without a lock"
      
lockEvent :: EventLock -> Event a -> Event a
lockEvent lock event =
      event
   +> (do
         theLockEvent lock 0
         event
      )
