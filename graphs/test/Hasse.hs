-- | Given a list of poset relations, remove those which can be deduced
-- from others. 
module Hasse(
   hasse -- Ord a => [(a,a)] -> [(a,a)]
   ) where

import List hiding (intersect,union)

import FiniteMap
import Set

import TopSort

data Ord a => HasseData a = HasseData {
   leftToDo :: [a],
   positions :: FiniteMap a Int,
   soFar :: [(a,a)],
   directPredecessors :: FiniteMap a [a],
   knownPredecessors :: FiniteMap a (Set a)
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
      positions = listToFM (zip sorted [0..])
      soFar = []

      emptyMap = listToFM (zip sorted (repeat []))
      directPredecessors =
         foldr
            (\ (from,to) map ->
               let
                  Just predecessors = lookupFM map to
               in
                  addToFM map to (from:predecessors)
               )
            emptyMap
            relations
   in
      HasseData { 
         leftToDo = sorted,
         positions = positions,
         soFar = soFar,
         directPredecessors = directPredecessors,
         knownPredecessors = emptyFM
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
            Just nextPredecessors = lookupFM directPredecessors next
            sortedPredecessors =
               sortBy 
                  (\ x y ->
                     let
                        Just xNo = lookupFM positions x
                        Just yNo = lookupFM positions y
                     in
                        compare xNo yNo
                     ) 
                  nextPredecessors
            lessThanNext = unitSet next
            (newSoFar,lessThanNext2) =
               foldr
                  (\ predecessor (sF@(soFar,lessThanNext)) ->
                     if elementOf predecessor lessThanNext
                        then
                           sF
                        else
                           let
                              Just predPredecessors =
                                 lookupFM knownPredecessors predecessor
                           in
                              ((predecessor,next):soFar,
                                 union predPredecessors lessThanNext
                                 )
                     ) 
                  (soFar,lessThanNext)
                  sortedPredecessors
            knownPredecessors2 = addToFM knownPredecessors next lessThanNext2
         in    
            Right (HasseData{
               leftToDo = remainingToDo,
               positions = positions,
               soFar = newSoFar,
               directPredecessors = directPredecessors,
               knownPredecessors = knownPredecessors2
               })

   