{- GuardedChannels implements high level events in terms of guarded channels.
   -}
module GuardedChannels(
   newGuardedChannel, -- imported from PrimGuardedChannel
   send, -- :: (--context--) =>
      -- GuardedChannel value guardQueue valueQueue -> value -> 
      --       BaseEvent ()
   receive, -- :: (--context--) => 
      -- GuardedChannel value guardQueue valueQueue -> GuardedEvent guard value
   sneak, -- :: (--context--) => 
      -- GuardedChannel value guardQueue valueQueue 
      -- -> GuardedEvent guard (Maybe value)
   replace, -- :: (--context--) => 
      -- GuardedChannel value guardQueue valueQueue -> value 
      -- -> GuardedEvent guard (Maybe value)
   ) where

import GuardBasics
import EventClasses
import GuardedEvents
import PrimGuardedChannels


-- ---------------------------------------------------------------
-- Sending values
-- ---------------------------------------------------------------

send :: (GuardQueue guardQueue value,AddValueQueue valueQueue value)
   => GuardedChannel value guardQueue valueQueue -> value -> BaseEvent ()
send channel value = toBaseEvent (sendPrim channel value)

-- ---------------------------------------------------------------
-- Getting values
-- ---------------------------------------------------------------

receive :: (ValueQueue valueQueue guard value,AddGuardQueue guardQueue guard,
   GuardQueue guardQueue value) 
   => GuardedChannel value guardQueue valueQueue -> GuardedEvent guard value
receive guardedChannel = 
   GuardedEvent (\ guard -> receivePrim guardedChannel guard) nullGuard

sneak :: (ValueQueue valueQueue guard value,AddGuardQueue guardQueue guard,
   GuardQueue guardQueue value) 
   => GuardedChannel value guardQueue valueQueue 
   -> GuardedEvent guard (Maybe value)
sneak guardedChannel =
   GuardedEvent (\ guard -> sneakPrim guardedChannel guard) nullGuard

replace :: (ValueQueue valueQueue guard value,AddGuardQueue guardQueue guard,
   GuardQueue guardQueue value) 
   => GuardedChannel value guardQueue valueQueue -> value
   -> GuardedEvent guard (Maybe value)
replace guardedChannel newValue = GuardedEvent 
   (\ guard -> replacePrim guardedChannel guard newValue) nullGuard 
