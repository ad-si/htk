module Graphs.TopSort(
   topSort, -- :: Ord a => [(a,a)] -> [a]
   topSort1, -- :: Ord a => [(a,a)] -> [a] -> [a]
   ) where

import Util.DeprecatedFiniteMap

-- Based on the Art of Computer Programming
-- Chapter 1, 2.2.3.
data Ord a => TopSortState a = TopSortState {
   soFar :: [a],
   -- Elements so far added to the list.
   maximal :: [a],
   -- All elements with no remaining greater elements.
   -- When this is empty, soFar is the correct solution.
   remaining :: FiniteMap a (Int,[a])
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

   case lookupFM remaining node of
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
                  lookupWithDefaultFM map (0,[]) from
               map2 = addToFM map from (nFromSuccs+1,fromPredecessors)
               (nToSuccs,toPredecessors) =
                  lookupWithDefaultFM map2 (0,[]) to
               map3 = addToFM map2 to (nToSuccs,from:toPredecessors)
            in
               map3
            )
         emptyFM
         list
      mapEls =  fmToList map
      maximal = [ key | (key,(nSuccs,_)) <- mapEls, nSuccs ==0 ]
   in
      TopSortState { soFar = soFar, remaining = map, maximal = maximal }

oneStep :: Ord a => TopSortState a -> Either [a] (TopSortState a)
oneStep(TopSortState { soFar = soFar, remaining = map, maximal = maximal }) =
   case maximal of
      [] ->
         if isEmptyFM map
            then Left soFar
            else error "TopSort - cycle in data"
      next:newMaximal ->
         let
            Just (0,nextPredecessors) = lookupFM map next
            newSoFar = next:soFar
            (newMaximal2,newMap) =
               foldr
                  (\ pred (maximal,map) ->
                     let
                        Just (nPredSuccs,predPredecessors) = lookupFM map pred
                        newNPredSuccs = nPredSuccs-1
                        newMap = addToFM map pred
                           (newNPredSuccs,predPredecessors)
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
            newMap2 = delFromFM newMap next
         in
            Right(TopSortState {
               soFar = newSoFar,maximal = newMaximal2,remaining = newMap2
               })


