{- VariableSet allow us to track changes to an unordered mutable set.  
   The elements of the set are keyed by instancing HasKey with some Ord
   instance; this allows us to set up a special HasKey instance for this 
   module without committing us to that Ord instance everywhere. -}
module VariableSet(
   HasKey(..),
   Keyed(..),
   VariableSetUpdate(..),
   VariableSet(..),

   newEmptyVariableSet,
   newVariableSet,
   updateSet,

   VariableSetSource,

   mapVariableSetSourceIO',
   concatVariableSetSource,
   ) where

import Maybe

import Set
import Concurrent

import Dynamics
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
   => VariableSetData x -> VariableSetUpdate x -> Maybe (VariableSetData x)
update (VariableSetData set) update =
   case update of
      AddElement x -> 
         let
            kx = Keyed x
            isElement = elementOf kx set
         in
            if isElement then Nothing else 
               Just (VariableSetData (addToSet set kx))
      DelElement x ->
         let
            kx = Keyed x
            isElement = elementOf kx set
         in
            if isElement then Just(VariableSetData (delFromSet set kx))
               else Nothing

newtype VariableSet x = 
   VariableSet (Broadcaster (VariableSetData x) (VariableSetUpdate x))

-- --------------------------------------------------------------------
-- The provider's interface
-- --------------------------------------------------------------------

---
-- Create a new empty variable set.
newEmptyVariableSet :: HasKey x key => IO (VariableSet x)
newEmptyVariableSet = 
   do
      broadcaster <- newGeneralBroadcaster update (VariableSetData emptySet)
      return (VariableSet broadcaster)

---
-- Create a new variable set with given contents
newVariableSet :: HasKey x key => [x] -> IO (VariableSet x)
newVariableSet contents =
   do
      broadcaster <- newGeneralBroadcaster update 
         (VariableSetData (mkSet (map Keyed contents)))
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
         (VariableSetData set) <- addOldSink broadcaster sink
         return (map unKey (setToList set))

   readContents (VariableSet broadcaster) =
      do
         (VariableSetData set) <- readContents broadcaster
         return (map unKey (setToList set))

-- --------------------------------------------------------------------
-- Make VariableSet Typeable
-- --------------------------------------------------------------------

variableSet_tyRep = mkTyRep "VariableSet" "VariableSet"
instance HasTyRep1 VariableSet where
   tyRep1 _ = variableSet_tyRep

-- --------------------------------------------------------------------
-- Type with the clients interface to a variable set (but which may be
-- otherwise implemented)
-- --------------------------------------------------------------------

type VariableSetSource x = SinkSource [x] (VariableSetUpdate x)

-- --------------------------------------------------------------------
-- Combinators for VariableSetSource
-- --------------------------------------------------------------------

mapVariableSetSourceIO' :: (x -> IO (Maybe y)) -> VariableSetSource x 
   -> VariableSetSource y
mapVariableSetSourceIO' mapFn variableSetSource =
   mapSinkSourceIO
      (\ currentEls ->
         do
            newEls <- mapM mapFn currentEls
            return (catMaybes newEls)
         )
      (\ change ->
         case change of
            AddElement x ->
               do
                  yOpt <- mapFn x
                  case yOpt of
                     Nothing -> return Nothing
                     Just y -> return (Just (AddElement y))
            DelElement x ->
               do
                  yOpt <- mapFn x
                  case yOpt of
                     Nothing -> return Nothing
                     Just y -> return (Just (DelElement y))
         )
      variableSetSource

concatVariableSetSource :: VariableSetSource x -> VariableSetSource x 
   -> VariableSetSource x
concatVariableSetSource (source1 :: VariableSetSource x) source2 =
   let
      pair :: SinkSource ([x],[x]) 
         (Either (VariableSetUpdate x) (VariableSetUpdate x))
      pair = pairSinkSource source1 source2

      res :: SinkSource [x] (VariableSetUpdate x)
      res = mapSinkSource (\ (x1,x2) -> x1 ++ x2)
         (\ xlr -> case xlr of 
            Left x -> x 
            Right x -> x
            )
         pair
   in
      res
   



