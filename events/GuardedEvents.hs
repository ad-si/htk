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
   ) where

import GuardBasics
import EventClasses

data Guard guard => GuardedEvent guard a = 
   GuardedEvent !(guard -> PrimEvent a) !guard
        
-- ----------------------------------------------------------------------
-- The HasGuard class
-- ----------------------------------------------------------------------

infixl 2 |> 
-- So higher precedence than >>>/>>>= or +>, and unlike them
-- done from the left, so a |> guard1 |> guard2 works sensibly.

class Guard guard => HasGuard eventType guard where
   (|>) :: eventType a -> guard -> eventType a

-- ----------------------------------------------------------------------
-- Instances
-- ----------------------------------------------------------------------

instance Guard guard => HasGuard (GuardedEvent guard) guard where
   (|>) (GuardedEvent getPrimEvent guard1) guard2 =
      GuardedEvent getPrimEvent (guard2 `andGuard` guard1)

toPrimEvent :: GuardedEvent guard a -> PrimEvent a
toPrimEvent (GuardedEvent getPrimEvent guard) = getPrimEvent guard

instance IsBaseEvent (GuardedEvent guard) where
   toBaseEvent guardedEvent = toBaseEvent (toPrimEvent guardedEvent)

instance HasChoice (GuardedEvent guard) where
   choose guardedEvent events = choose (toBaseEvent guardedEvent) events

instance HasContinuation (GuardedEvent guard) where
   (>>>=) guardedEvent continuation = 
      (toBaseEvent guardedEvent) >>>= continuation

instance HasSync (GuardedEvent guard) where
   sync guardedEvent = sync (toBaseEvent guardedEvent)
   