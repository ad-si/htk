-- | The function in this module finds a cycle in a given directed graph, if one
-- exists. 
module FindCycle (
   findCycle, 
      -- :: Ord a => [a] -> (a -> [a]) -> Maybe [a]
      -- List of all nodes, and a successor function.
   ) where

import Data.Set

data DFSOut a =
      NoCycle (Set a) -- set of nodes *not* visited
   |  Cycle [a]
   |  PartialCycle [a] a

findCycle :: Ord a => [a] -> (a -> [a]) -> Maybe [a]
findCycle (nodes :: [a]) (sFn :: a -> [a]) = 
   let
      notVisited0 = mkSet nodes

      findCycle1 :: Set a -> Maybe [a]
      findCycle1 notVisited0 =
         case setToList notVisited0 of
            a : _ ->
               case findCycle2 emptySet notVisited0 a of
                  NoCycle notVisited1 -> findCycle1 notVisited1
                  Cycle cycle -> Just cycle
                  _ -> error "findCycle - unexpected PartialCycle"
            [] -> Nothing


      findCycle2 :: Set a -> Set a -> a -> DFSOut a
      findCycle2 aboveThis0 notVisited0 this =
         if elementOf this notVisited0
            then
               if elementOf this aboveThis0
                  then
                     PartialCycle [] this
                  else
                     let
                        succs = sFn this
                        aboveThis1 = addToSet aboveThis0 this
                     
                        doSuccs :: [a] -> Set a -> DFSOut a
                        doSuccs [] notVisited 
                           = NoCycle (delFromSet notVisited this) 
                        doSuccs (succ:succs) notVisited0 =
                           case findCycle2 aboveThis1 notVisited0 succ of
                              NoCycle notVisited1 -> doSuccs succs notVisited1
                              PartialCycle arc node ->
                                 if node == this
                                    then
                                       Cycle (this : arc)
                                    else
                                       PartialCycle (this : arc) node
                              cycle -> cycle
                     in
                        doSuccs succs notVisited0
            else
               NoCycle notVisited0
   in
      findCycle1 notVisited0