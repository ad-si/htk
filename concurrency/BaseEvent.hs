{- A BaseEvent is a PrimEvent with an extra arbitrarily complicated 
   action attached to it which is performed (in the syncing thread)
   when the event happens; hence also the sync operation must wait
   until the event happens.

   Alternative perspective (from the other side) - a BaseEvent is
   eventlike with an operation similar to >>>= but nothing like +>.
   You sync or poll on a list of base events (rather than just one).
   -}
module BaseEvent(
   BaseEvent,
   primToBaseEvent, -- So channels start by defining a primitive event
   -- and go on to get a base event this way
   (>>>=#),
   syncBaseEvents,
   pollBaseEvents,
   tryBaseEvent -- this modifies a base event to catch errors, like tryEV
   ) where

import qualified Concurrent

import Computation(try,tryM,Answer)
import ExtendedPrelude(monadDot)

import PrimEvent

data BaseEvent eventResult =
   forall primEventResult .  
      BaseEvent (PrimEvent primEventResult,primEventResult -> IO eventResult)

primToBaseEvent :: PrimEvent eventResult -> BaseEvent eventResult
primToBaseEvent primEvent = BaseEvent (primEvent,return)

(>>>=#) :: 
   BaseEvent eventFirstResult -> 
   (eventFirstResult -> IO eventResult) ->
   BaseEvent eventResult
(>>>=#) (BaseEvent (primEvent,continuation)) converter =
   BaseEvent (primEvent,converter `monadDot` continuation)
         
tryBaseEvent :: BaseEvent eventResult -> BaseEvent (Answer eventResult)
tryBaseEvent(BaseEvent(primEvent,continuation)) =
   BaseEvent(primEvent,\ eventResult -> tryM(continuation eventResult))

{- syncBaseEvents syncs on a list of base events and doesn't
   return until the event happens -}
syncBaseEvents :: [BaseEvent eventResult] -> IO eventResult
syncBaseEvents baseEventList = syncOrPollBaseEvents baseEventList Nothing

{- pollBaseEvents polls a list of base events and if one doesn't
   happen immediately, returns Nothing. -}
pollBaseEvents :: [BaseEvent eventResult] -> IO (Maybe eventResult)
pollBaseEvents baseEventList = 
   syncOrPollBaseEvents
      (map (>>>=# (return . Just)) baseEventList)
      (Just Nothing)

syncOrPollBaseEvents :: 
   [BaseEvent eventResult] ->
   (Maybe eventResult) -> -- if this is set, it provides a result and
   -- handles the event if none of the others can be handled immediately.
   IO eventResult
syncOrPollBaseEvents baseEventList fallback =
   do
      continuationHolder <- Concurrent.newEmptyMVar ::
         IO (Concurrent.MVar (IO eventResult))
         -- this is the trick - the action passed in the primEvent will
         -- write the action to be done to this holder; it will not
         -- actually do it.
      let
         primEventSelections =
            map
               (\ (BaseEvent baseEvent) ->
                  let
                     (primEvent,continuation) = baseEvent
                  in
                     PrimEventSelection(primEvent,
                        \ eventResult ->
                           Concurrent.putMVar continuationHolder 
                              (continuation eventResult)
                        )
                  )
               baseEventList
         pollAction =
            fmap
               (\ defaultResult ->
                  Concurrent.putMVar continuationHolder (return defaultResult)
                  )
               fallback

      -- now register all the events!
      syncPrimEvents primEventSelections pollAction

      -- wait for the event to happen and then grab the action
      continuationAction <- Concurrent.takeMVar continuationHolder

      -- do it
      continuationAction
      -- In many cases the continuationAction will never return
      -- because, for example, it always has another sync.  So this must
      -- must be tail-recursive.




