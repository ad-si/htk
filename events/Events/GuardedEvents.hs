{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | In GuardedEvents we extend the notion of PrimEvents to allow Guarded
-- Events, which can be guarded with the new (|>) operator.  GuardedChannels
-- will implement guarded events on channels, which will hopefully be the
-- only guarded event we will ever need.
module Events.GuardedEvents(
   GuardedEvent(..),
      -- the datatype of guarded events. Instance of HasGuard,
      -- IsBaseEvent (and hence IsEvent), HasContinuation, HasChoice
      --
   HasGuard(..), -- the class implementing |>
   Guard(..), -- the class of guards.

   HasListen(..), -- the class of (guarded) channels implementing listen.
   ) where

import Events.Events

-- | A GuardedEvent guard a represents a source of values of type a, which
-- may be selected from according to guards of type guard.
data Guard guard => GuardedEvent guard a =
   GuardedEvent !(guard -> Event a) !guard

-- ----------------------------------------------------------------------
-- The Guard class
-- ----------------------------------------------------------------------

-- | A Guard represents some condition on a value which we impose on
-- a channel, selecting those values we are interested in.
class Guard guard where
   -- NB.  Instances of this class should try to force evaluation as
   -- much as possible before returning the guard value, because
   -- otherwise it has to be done while the channel is locked to
   -- everyone else.

   -- | this should be the guard that always matches
   nullGuard :: guard

   -- | this should be the guard that corresponds to the conjunction
   -- of the two given guards.
   andGuard :: guard -> guard -> guard

-- ----------------------------------------------------------------------
-- The HasGuard class
-- ----------------------------------------------------------------------

infixr 2 |>
-- So higher precedence than >>>/>>>= or +>


class Guard guard => HasGuard eventType guard where
   ---
   -- Qualify an event source by a guard.
   (|>) :: eventType a -> guard -> eventType a

-- ----------------------------------------------------------------------
-- The HasListen class
-- ----------------------------------------------------------------------

-- | Class of those channels which have guarded events.
class HasListen chan where
   ---
   -- Generate a guarded event from a channel (which may then be
   -- synchronised on, or qualified using |>
   listen :: Guard guard => chan guard a -> GuardedEvent guard a

-- ----------------------------------------------------------------------
-- Instances
-- ----------------------------------------------------------------------

instance Guard guard => HasGuard (GuardedEvent guard) guard where
   (|>) (GuardedEvent getEvent guard1) guard2 =
      GuardedEvent getEvent (guard2 `andGuard` guard1)

instance Guard guard => HasEvent (GuardedEvent guard) where
   toEvent (GuardedEvent getEvent guard) = getEvent guard

