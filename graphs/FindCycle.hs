-- | The function in this module finds a cycle in a given directed graph, if
-- one exists. 
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

-- | Find a cycle in a graph.  We are given a list of nodes to start
-- from, and a successor function.
findCycle :: Ord a => [a] -> (a -> [a]) -> Maybe [a]
findCycle (nodes :: [a]) (sFn :: a -> [a]) = 
   let
      findCycle1 :: [a] -> Set a -> Maybe [a]
      findCycle1 nodes0 visited0 =
         case nodes0 of
            a : nodes1 ->
               case findCycle2 emptySet visited0 a of
                  NoCycle visited1 -> findCycle1 nodes1 visited1
                  Cycle cycle -> Just cycle
                  _ -> error "findCycle - unexpected PartialCycle"
            [] -> Nothing


      findCycle2 :: Set a -> Set a -> a -> DFSOut a
      findCycle2 aboveThis0 visited0 this =
         if elementOf this visited0
            then
               NoCycle visited0
            else
               if elementOf this aboveThis0
                  then
                     PartialCycle [] this
                  else
                     let
                        succs = sFn this
                        aboveThis1 = addToSet aboveThis0 this
                     
                        doSuccs :: [a] -> Set a -> DFSOut a
                        doSuccs [] visited 
                           = NoCycle (addToSet visited this) 
                        doSuccs (succ:succs) visited0 =
                           case findCycle2 aboveThis1 visited0 succ of
                              NoCycle visited1 -> doSuccs succs visited1
                              PartialCycle arc node ->
                                 if node == this
                                    then
                                       Cycle (this : arc)
                                    else
                                       PartialCycle (this : arc) node
                              cycle -> cycle
                     in
                        doSuccs succs visited0
   in
      findCycle1 nodes emptySet