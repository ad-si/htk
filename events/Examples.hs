{- Examples is meant to contain examples of using events which
   are too small to go into their own module. -}
module Examples(
   EventSet, -- These encode a set of events
   emptyEventSet, -- :: EventSet a
   addToEventSet, -- :: EventSet a -> Event a -> EventSet a
   fromEventSet, -- :: EventSet a -> Event (a,EventSet a)
   isEmptyEventSet, -- :: EventSet a -> Bool

   watch, -- :: Event a -> IO (Event a,IO ())
   -- watch is used for events which can be dropped occasionally.
   ) where

import Object
import FiniteMap
import Spawn
import Events
import Channels

-- ------------------------------------------------------------------
-- Event Sets
-- ------------------------------------------------------------------

data EventSet a = EventSet Int (FiniteMap Int (Event a))

emptyEventSet :: EventSet a
emptyEventSet = EventSet 0 emptyFM

addToEventSet :: EventSet a -> Event a -> EventSet a
addToEventSet (EventSet next fmap) event =
   EventSet (next+1) (addToFM fmap next event)

fromEventSet :: EventSet a -> Event (a,EventSet a)
-- fromEventSet turns the event set into an event which
-- waits for one of the events to happen, and then returns
-- the value, plus the event set containing the remaining events.
fromEventSet (EventSet next fmap) =
   choose
      (map
         (\ (key,event) ->
            event >>>= (\ a -> return (a,EventSet next (delFromFM fmap key)))
            )
         (fmToList fmap)
         )

isEmptyEventSet :: EventSet a -> Bool
isEmptyEventSet (EventSet _ fmap) = isEmptyFM fmap

-- ------------------------------------------------------------------
-- Watchers
-- ------------------------------------------------------------------

watch :: Event a -> IO (Event a,IO ())
-- watch is used for events like mouse motion events where
-- if we can't find time we don't want them queued.
watch (event :: Event a) =
   do
      channel <- newChannel
      dieChannel <- newChannel
      let
         die = receive dieChannel

         waitForNext :: Event ()
         waitForNext =
               do
                  next <- event
                  passOn next
            +> die
         passOn :: a -> Event ()
         passOn val =
               waitForNext
            +> (do
                  send channel val
                  waitForNext
               )
            +> die

      spawnEvent waitForNext

      return (receive channel,sync(send dieChannel ()))
 