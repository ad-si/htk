{- VariableSet is an interface and a simple implementation of it that
   allow us to track changes to an unordered mutable set.  The elements
   of the set are keyed by instancing HasKey with some Ord instance;
   this allows us to set up a special HasKey instance for this module without
   committing us to that Ord instance everywhere. -}
module VariableSet(
   HasKey(..),
   Keyed(..),
   VariableSetUpdate(..),
   VariableSet(..),

   newVariableSet,
   updateSet,
   ) where

import Set
import Concurrent

import Sink
import Broadcaster

-- --------------------------------------------------------------------
-- The HasKey and Keyed types
-- --------------------------------------------------------------------

class Ord key => HasKey x key | x -> key where
   toKey :: x -> key


newtype Keyed x = Keyed x

unKey :: Keyed x -> x
unKey (Keyed x) = x

lift :: (HasKey x1 key1,HasKey x2 key2) 
   => (key1 -> key2 -> a) -> (Keyed x1 -> Keyed x2 -> a)
lift f x1 x2 = f (toKey . unKey $ x1) (toKey . unKey $ x2)


---
-- HasKey specifies the ordering to use (without committing us to
-- a particular Ord instance elsewhere).
instance HasKey x key => Eq (Keyed x) where
   (==) = lift (==)
   (/=) = lift (/=)

instance HasKey x key => Ord (Keyed x) where
   compare = lift compare
   (<=) = lift (<=)
   (>=) = lift (>=)
   (<) = lift (<)
   (>) = lift (>)

-- --------------------------------------------------------------------
-- The datatype
-- --------------------------------------------------------------------

newtype VariableSetData x = VariableSetData (Set (Keyed x))

---
-- Encodes the updates to a variable set.
data VariableSetUpdate x =
      AddElement x
   |  DelElement x

update :: HasKey x key 
   => VariableSetData x -> VariableSetUpdate x -> VariableSetData x
update (VariableSetData set) update =
   case update of
      AddElement x -> VariableSetData (addToSet set (Keyed x))
      DelElement x -> VariableSetData (delFromSet set (Keyed x))


newtype VariableSet x = 
   VariableSet (Broadcaster (VariableSetData x) (VariableSetUpdate x))

-- --------------------------------------------------------------------
-- The provider's interface
-- --------------------------------------------------------------------

---
-- Create a new empty variable set.
newVariableSet :: HasKey x key => IO (VariableSet x)
newVariableSet = 
   do
      broadcaster <- newBroadcaster update (VariableSetData emptySet)
      return (VariableSet broadcaster)

---
-- Update a variable set in some way.
updateSet :: HasKey x key => VariableSet x -> VariableSetUpdate x -> IO ()
updateSet (VariableSet broadcaster) update = 
   updateBroadcaster broadcaster update


-- --------------------------------------------------------------------
-- The client's interface
-- --------------------------------------------------------------------

instance HasKey x key 
   => CanAddSinks (VariableSet x) [x] (VariableSetUpdate x) where
   addOldSink (VariableSet broadcaster) sink =
      do
         (VariableSetData x) <- addOldSink broadcaster sink
         return (map unKey (setToList x))

   



