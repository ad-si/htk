{- In GuardedEvents we extend the notion of PrimEvents to allow Guarded
   Events, which can be guarded with the new (|>) operator.  GuardedChannels
   will implement guarded events on channels, which will hopefully be the
   only guarded event we will ever need. -}
module GuardedEvents(
   GuardedEvent(..), 
      -- the datatype of guarded events. Instance of HasGuard,
      -- IsBaseEvent (and hence IsEvent), HasContinuation, HasChoice
      -- 
   HasGuard(..), -- the class implementing |>
   Guard(..), -- the class of guards.

   HasListen(..), -- the class of (guarded) channels implementing listen.
   ) where

import Events

data Guard guard => GuardedEvent guard a = 
   GuardedEvent !(guard -> Event a) !guard
        
-- ----------------------------------------------------------------------
-- The Guard class
-- ----------------------------------------------------------------------

class Guard guard where
   -- NB.  Instances of this class should try to force evaluation as
   -- much as possible before returning the guard value, because
   -- otherwise it has to be done while the channel is locked to
   -- everyone else.
   nullGuard :: guard 
      -- this should be the guard that always matches
   andGuard :: guard -> guard -> guard 
      -- this should be the guard that corresponds to the conjunction
      -- of the two given guards.

-- ----------------------------------------------------------------------
-- The HasGuard class
-- ----------------------------------------------------------------------

infixr 2 |> 
-- So higher precedence than >>>/>>>= or +>

class Guard guard => HasGuard eventType guard where
   (|>) :: eventType a -> guard -> eventType a

-- ----------------------------------------------------------------------
-- The HasListen class
-- ----------------------------------------------------------------------

class HasListen chan where
   listen :: Guard guard => chan guard a -> GuardedEvent guard a

-- ----------------------------------------------------------------------
-- Instances
-- ----------------------------------------------------------------------

instance Guard guard => HasGuard (GuardedEvent guard) guard where
   (|>) (GuardedEvent getEvent guard1) guard2 =
      GuardedEvent getEvent (guard2 `andGuard` guard1)

instance Guard guard => HasEvent (GuardedEvent guard) where
   toEvent (GuardedEvent getEvent guard) = getEvent guard

