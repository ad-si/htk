{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}

-- | VariableSet allow us to track changes to an unordered mutable set.
-- The elements of the set are keyed by instancing HasKey with some Ord
-- instance; this allows us to set up a special HasKey instance for this
-- module without committing us to that Ord instance everywhere.
module Util.VariableSet(
   HasKey(..),
   Keyed(..),
   VariableSetUpdate(..),
   VariableSet(..),

   newEmptyVariableSet,
   newVariableSet,
   updateSet,
   setVariableSet,

   VariableSetSource,
   emptyVariableSetSource,

   mapVariableSetSourceIO',
   concatVariableSetSource,

   mapVariableSetSource,
   singletonSetSource,
   listToSetSource,
   ) where

import Data.Maybe
import qualified Data.List as List

import qualified Data.Set as Set

import Util.Dynamics
import Util.Sources
import Util.Broadcaster

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


-- | HasKey specifies the ordering to use (without committing us to
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

newtype VariableSetData x = VariableSetData (Set.Set (Keyed x))

-- | Encodes the updates to a variable set.
-- BeginGroup does not actually alter the set itself, but
-- indicate that a group of updates is about to begin, terminated by EndGroup.
-- This prevents the client from trying to recalculate the state after every single
-- update.
--
-- BeginGroup\/EndGroup may be nested (though I don\'t have any application for that
-- yet).
data VariableSetUpdate x =
      AddElement x
   |  DelElement x
   |  BeginGroup
   |  EndGroup

update :: HasKey x key
   => VariableSetUpdate x -> VariableSetData x
   -> (VariableSetData x,[VariableSetUpdate x])
update setUpdate (variableSet @ (VariableSetData set)) =
   let
      noop = (variableSet,[])
      grouper = (variableSet,[setUpdate])
      oneop newSet = (VariableSetData newSet,[setUpdate])
   in
      case setUpdate of
         AddElement x ->
            let
               kx = Keyed x
               isElement = Set.member kx set
            in
               if isElement then noop else oneop (Set.insert kx set)
         DelElement x ->
            let
               kx = Keyed x
               isElement = Set.member kx set
            in
               if isElement then oneop (Set.delete kx set)
                  else noop
         BeginGroup -> grouper
         EndGroup -> grouper

newtype VariableSet x
   = VariableSet (Broadcaster (VariableSetData x) (VariableSetUpdate x))
   deriving (Typeable)

-- --------------------------------------------------------------------
-- The provider's interface
-- --------------------------------------------------------------------

-- | Create a new empty variable set.
newEmptyVariableSet :: HasKey x key => IO (VariableSet x)
newEmptyVariableSet =
   do
      broadcaster <- newBroadcaster (VariableSetData Set.empty)
      return (VariableSet broadcaster)

-- | Create a new variable set with given contents
newVariableSet :: HasKey x key => [x] -> IO (VariableSet x)
newVariableSet contents =
   do
      broadcaster
         <- newBroadcaster (VariableSetData (Set.fromList (fmap Keyed contents)))
      return (VariableSet broadcaster)

-- | Update a variable set in some way.
updateSet :: HasKey x key => VariableSet x -> VariableSetUpdate x -> IO ()
updateSet (VariableSet broadcaster) setUpdate
   = applyUpdate broadcaster (update setUpdate)

-- | Set the elements of the variable set.
setVariableSet :: HasKey x key => VariableSet x -> [x] -> IO ()
setVariableSet (VariableSet broadcaster) newList =
   do
     let
        newSet = Set.fromList (fmap Keyed newList)

        updateFn (VariableSetData oldSet) =
           let
              toAddList
                 = List.filter
                    (\ el -> not (Set.member (Keyed el) oldSet)) newList
              toDeleteList = fmap unKey (Set.toList (Set.difference oldSet newSet))
              updates =
                 [BeginGroup] ++ (fmap AddElement toAddList)
                    ++ (fmap DelElement toDeleteList) ++ [EndGroup]
           in
              (VariableSetData newSet,updates)

     applyUpdate broadcaster updateFn

-- --------------------------------------------------------------------
-- The client's interface
-- --------------------------------------------------------------------

instance HasKey x key => HasSource (VariableSet x) [x] (VariableSetUpdate x)
      where
   toSource (VariableSet broadcaster) =
      map1
         (\ (VariableSetData set) -> fmap unKey (Set.toList set))
         (toSource broadcaster)

-- --------------------------------------------------------------------
-- Type with the clients interface to a variable set (but which may be
-- otherwise implemented)
-- --------------------------------------------------------------------

type VariableSetSource x = Source [x] (VariableSetUpdate x)

emptyVariableSetSource :: VariableSetSource x
emptyVariableSetSource = staticSource []

-- --------------------------------------------------------------------
-- Combinators for VariableSetSource
-- --------------------------------------------------------------------

mapVariableSetSourceIO' :: (x -> IO (Maybe y)) -> VariableSetSource x
   -> VariableSetSource y
mapVariableSetSourceIO' mapFn=
   (map1IO
      (\ currentEls ->
         do
            newEls <- mapM mapFn currentEls
            return (catMaybes newEls)
         )
      )
   .
   (filter2IO
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
            BeginGroup -> return (Just BeginGroup)
            EndGroup -> return (Just EndGroup)
         )
      )

concatVariableSetSource :: VariableSetSource x -> VariableSetSource x
   -> VariableSetSource x
concatVariableSetSource (source1 :: VariableSetSource x) source2 =
   let
      pair :: Source ([x],[x])
         (Either (VariableSetUpdate x) (VariableSetUpdate x))
      pair = choose source1 source2

      res :: Source [x] (VariableSetUpdate x)
      res =
         (map1 (\ (x1,x2) -> x1 ++ x2))
         .
         (map2
            (\ xlr -> case xlr of
               Left x -> x
               Right x -> x
               )
            )
         $
         pair
   in
      res

-- --------------------------------------------------------------------
-- VariableSetUpdate is an instance of Functor.
-- mapVariableSetSource is functor-like for VariableSetSource.
-- --------------------------------------------------------------------

instance Functor VariableSetUpdate where
   fmap fn (AddElement x) = AddElement (fn x)
   fmap fn (DelElement x) = DelElement (fn x)
   fmap fn BeginGroup = BeginGroup
   fmap fn EndGroup = EndGroup

mapVariableSetSource :: (x -> y) -> VariableSetSource x -> VariableSetSource y
mapVariableSetSource fn source =
   (map1 (fmap fn)) .
   (map2 (fmap fn)) $
   source

-- --------------------------------------------------------------------
-- singletonSetSource creates a VariableSet with a single element
-- --------------------------------------------------------------------

singletonSetSource :: SimpleSource x -> VariableSetSource x
singletonSetSource (source0 :: SimpleSource x) =
   let
      (source1 :: Source x x) = toSource source0
      (source2 :: Source x (x,x)) = mkHistorySource id source1
      (source3 :: Source [x] [VariableSetUpdate x]) =
         (map1
            (\ x -> [x])
            )
         .
         (map2
            (\ (x1,x2) -> [BeginGroup,AddElement x2,DelElement x1,EndGroup])
            )
         $
         source2
      (source4 :: VariableSetSource x) = flattenSource source3
   in
      source4

-- | Creates a VariableSetSource whose elements are the same as those of the
-- corresponding list.
listToSetSource :: Ord x => SimpleSource [x] -> VariableSetSource x
listToSetSource (simpleSource :: SimpleSource [x]) =
   let
      source1 :: Source [x] [x]
      source1 = toSource simpleSource

      source2 :: Source (Set.Set x,[x]) [VariableSetUpdate x]
      source2 = foldSource
         (\ list -> Set.fromList list)
         (\ oldSet newList ->
            let
               newSet = Set.fromList newList

               toAdd = Set.difference newSet oldSet
               adds = fmap AddElement (Set.toList toAdd)

               toDelete = Set.difference oldSet newSet
               deletes = fmap DelElement (Set.toList toDelete)
            in
               (newSet,adds ++ deletes)
            )
         source1

      source3 :: Source [x] [VariableSetUpdate x]
      source3 = map1 snd source2

      source4 :: Source [x] (VariableSetUpdate x)
      source4 = flattenSource source3
   in
      source4



