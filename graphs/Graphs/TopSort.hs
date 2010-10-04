module Graphs.TopSort(
   topSort, -- :: Ord a => [(a,a)] -> [a]
   topSort1, -- :: Ord a => [(a,a)] -> [a] -> [a]
   ) where

import qualified Data.Map as Map

-- Based on the Art of Computer Programming
-- Chapter 1, 2.2.3.
data Ord a => TopSortState a = TopSortState {
   soFar :: [a],
   -- Elements so far added to the list.
   maximal :: [a],
   -- All elements with no remaining greater elements.
   -- When this is empty, soFar is the correct solution.
   remaining :: Map.Map a (Int,[a])
   -- Map giving for each element
   -- (a) successors not yet added to soFar.
   -- (b) its direct predecessors.
   }

topSort :: Ord a => [(a,a)] -> [a]
topSort relations = topSort1 relations []

topSort1 :: Ord a => [(a,a)] -> [a] -> [a]
topSort1 relations nodes =
   let
      topSortState0 = initialise relations
      topSortState1 = ensureNodes topSortState0 nodes

      doWork topSortState0 =
         case oneStep topSortState0 of
            Left result -> result
            Right topSortState1 -> doWork topSortState1
   in
      doWork topSortState1

ensureNodes :: Ord a => TopSortState a -> [a] -> TopSortState a
ensureNodes = foldl ensureNode

ensureNode :: Ord a => TopSortState a -> a -> TopSortState a
ensureNode
   (state @ (
      TopSortState {soFar = soFar,maximal = maximal,remaining = remaining}))
   node =

   case Map.lookup node remaining of
      Nothing -> -- node not mentioned.  Add it to soFar
         state {soFar = node : soFar}
      Just _ -> state

initialise :: Ord a => [(a,a)] -> TopSortState a
initialise list =
   let
      soFar = []
      map = foldr
         (\ (from,to) map ->
            let
               (nFromSuccs,fromPredecessors) =
                  Map.findWithDefault (0,[]) from map
               map2 = Map.insert from (nFromSuccs+1,fromPredecessors) map
               (nToSuccs,toPredecessors) =
                  Map.findWithDefault (0,[]) to map2
               map3 = Map.insert to (nToSuccs,from:toPredecessors) map2
            in
               map3
            )
         Map.empty
         list
      mapEls =  Map.toList map
      maximal = [ key | (key,(nSuccs,_)) <- mapEls, nSuccs ==0 ]
   in
      TopSortState { soFar = soFar, remaining = map, maximal = maximal }

oneStep :: Ord a => TopSortState a -> Either [a] (TopSortState a)
oneStep(TopSortState { soFar = soFar, remaining = map, maximal = maximal }) =
   case maximal of
      [] ->
         if Map.null map
            then Left soFar
            else error "TopSort - cycle in data"
      next:newMaximal ->
         let
            Just (0,nextPredecessors) = Map.lookup next map
            newSoFar = next:soFar
            (newMaximal2,newMap) =
               foldr
                  (\ pred (maximal,map) ->
                     let
                        Just (nPredSuccs,predPredecessors) = Map.lookup pred map
                        newNPredSuccs = nPredSuccs-1
                        newMap = Map.insert pred
                           (newNPredSuccs,predPredecessors) map
                        newMaximal = if newNPredSuccs == 0
                           then
                              (pred:maximal)
                           else
                              maximal
                     in
                        (newMaximal,newMap)
                     )
                  (newMaximal,map)
                  nextPredecessors
            newMap2 = Map.delete next newMap
         in
            Right(TopSortState {
               soFar = newSoFar,maximal = newMaximal2,remaining = newMap2
               })


