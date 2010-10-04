-- | Given a list of poset relations, remove those which can be deduced
-- from others.
module Hasse(
   hasse -- Ord a => [(a,a)] -> [(a,a)]
   ) where

import Data.List hiding (intersect,union)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Graphs.TopSort

data Ord a => HasseData a = HasseData {
   leftToDo :: [a],
   positions :: Map.Map a Int,
   soFar :: [(a,a)],
   directPredecessors :: Map.Map a [a],
   knownPredecessors :: Map.Map a (Set.Set a)
   }

hasse :: Ord a => [(a,a)] -> [(a,a)]
hasse relations =
   let
      init = initialise relations
      doWork hasseData =
         case oneStep hasseData of
            Left result -> result
            Right nextStep -> doWork nextStep
   in
      doWork init

initialise :: Ord a => [(a,a)] -> HasseData a
initialise relations =
   let
      sorted = topSort relations
      positions = Map.fromList (zip sorted [0..])
      soFar = []

      emptyMap = Map.fromList (zip sorted (repeat []))
      directPredecessors =
         foldr
            (\ (from,to) map ->
               let
                  Just predecessors = Map.lookup map to
               in
                  Map.insert map to (from:predecessors)
               )
            emptyMap
            relations
   in
      HasseData {
         leftToDo = sorted,
         positions = positions,
         soFar = soFar,
         directPredecessors = directPredecessors,
         knownPredecessors = Map.empty
         }

oneStep :: Ord a => HasseData a -> Either [(a,a)] (HasseData a)
oneStep (HasseData {
   leftToDo = leftToDo,
   positions = positions,
   soFar = soFar,
   directPredecessors = directPredecessors,
   knownPredecessors = knownPredecessors
   }) =
   case leftToDo of
      [] -> Left soFar
      (next:remainingToDo) ->
         let
            Just nextPredecessors = Map.lookup directPredecessors next
            sortedPredecessors =
               sortBy
                  (\ x y ->
                     let
                        Just xNo = Map.lookup positions x
                        Just yNo = Map.lookup positions y
                     in
                        compare xNo yNo
                     )
                  nextPredecessors
            lessThanNext = Set.singleton next
            (newSoFar,lessThanNext2) =
               foldr
                  (\ predecessor (sF@(soFar,lessThanNext)) ->
                     if Set.member predecessor lessThanNext
                        then
                           sF
                        else
                           let
                              Just predPredecessors =
                                 Map.lookup knownPredecessors predecessor
                           in
                              ((predecessor,next):soFar,
                                 Set.union predPredecessors lessThanNext
                                 )
                     )
                  (soFar,lessThanNext)
                  sortedPredecessors
            knownPredecessors2 = Map.insert knownPredecessors next lessThanNext2
         in
            Right (HasseData{
               leftToDo = remainingToDo,
               positions = positions,
               soFar = newSoFar,
               directPredecessors = directPredecessors,
               knownPredecessors = knownPredecessors2
               })


