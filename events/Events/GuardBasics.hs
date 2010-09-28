{- In this file we define data structures for the classes other files
   need to instance to get GuardedChannels to implement channels for
   guarded events.  These are supposed to be in the form of queues,
   perhaps indexed specially.

   Terminology: the purpose of all this is to implement a way of
   communicating a bit of data in an acknowledged way from
   some thread (which we call the source) to some thread (which we call
   the listener).  A guard corresponds to some condition nominated
   by the listener restricting the set of data values that it will
   accept.  After the data has been transferred (a) the source may need
   to know that it's been transferred; (b) the listener obviously needs to
   have the data.
   -}
module Events.GuardBasics(
   Guard(..),
   HasEmptyQueue(..),
   AddGuardQueue(..),
   GuardQueue(..),
   AddValueQueue(..),
   ValueQueue(..),
   ) where

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

-- ---------------------------------------------------------------
-- General observations on queues.
-- Queues are essentially single threaded; that is; the
-- implementation may assume that the queues returned by
-- addGuard, lookupValue, addValue, lookupGuard replace the
-- queues supplied in their arguments.
--    addGuard and addValue
-- additionally return an IO action that, if executed, should
-- invalidate the guard or value supplied, so that it won't
-- be returned any more by lookupGuard or lookupValue.  In addition
-- it should be harmless if the invalidation action is performed
-- after the value/guard has already left the queue (that is,
-- been returned by lookupGuard or lookupValue).
--    Invalidation will be done after sync'ing on an event has
-- been completed, when we are no longer interested in the
-- component events.  Invalidation has two advantages:
-- (A) it prevents the guard implementation wasting time
--     digging out things in the queue which we know are no
--     longer going to be wanted.
-- (B) the invalidation can potentially prevent space leaks, because
--     the garbage collector won't be forced to keep points to the
--     old data.
--
--    In fact I am planning to make the channel implementation
--    store the queues directly into MVars.  The reason we don't
--    do that here is that I also plan for the channel implementation
--    to use these MVars for some locking on the channel to implement
--    an atomic read-and-send on a channel.
--
-- Concurrency Warning:  Instances should never lock or wait on a variable
-- over which they don't have full control.

-- ---------------------------------------------------------------


class HasEmptyQueue queue where
   emptyQueue :: queue someData -- this is the initial value.

-- A GuardQueue corresponds to a list of listeners waiting to
-- hear something from an event source.  We add guards to it to
-- represent new listeners with addGuard; we look for a guard
-- matching a given value using lookupValue.
class (HasEmptyQueue guardQueue,Guard guard) =>
      AddGuardQueue guardQueue guard where
   addGuard :: guardQueue listenerData -> guard -> listenerData
      -> IO(guardQueue listenerData,IO())

class (HasEmptyQueue guardQueue) => GuardQueue guardQueue value where
   lookupValue :: guardQueue listenerData -> value
      -> Maybe (listenerData,guardQueue listenerData)
      -- returns Nothing if no matching guard can be found.
   -- If more than one guard matches, then all things being equal
   -- lookupValue should return the guard first added to the guardQueue.
   -- This ensures that events are served in FIFO order.  All things might
   -- not be equal if, for example, the application allows guards to
   -- have priorities.

-- A ValueQueue corrresponds to a list of value datas from an event source
-- that haven't yet found a matching guard.  It will be observed
-- that the situation is almost exactly dual if we swap
-- ValueQueue <=> GuardQueue,guard <=> value, sourceData <=> listenerData.
-- The only change is that lookupGuard returns the value as well, while
-- lookupValue doesn't bother to return the guard.
class (HasEmptyQueue valueQueue)
   => AddValueQueue valueQueue value where
   addValue :: valueQueue sourceData -> value -> sourceData
      -> IO(valueQueue sourceData,IO())

class (AddValueQueue valueQueue value,Guard guard)
   => ValueQueue valueQueue guard value where
   lookupGuard :: valueQueue sourceData -> guard ->
      Maybe (sourceData,value,valueQueue sourceData)


