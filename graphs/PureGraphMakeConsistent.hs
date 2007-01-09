{- pureGraphMakeConsistent removes parent links to non-existent nodes from
   the graph.

   This deals with a situation which can sometimes occur when versions are
   being rewired.
   -}
module PureGraphMakeConsistent(
   pureGraphMakeConsistent,
   ) where

import List

import DeprecatedFiniteMap

import PureGraph

pureGraphMakeConsistent :: Ord nodeInfo 
   => PureGraph nodeInfo arcInfo -> PureGraph nodeInfo arcInfo
pureGraphMakeConsistent (PureGraph {nodeDataFM = nodeDataFM0}) =
   let
      nodeDataFM1 = mapFM
         (\ _ nodeData0 ->
            let
               parents0 = parents nodeData0
               parents1 = filter 
                  (\ arcData -> elemFM (target arcData) nodeDataFM0) 
                  parents0

               nodeData1 = NodeData {parents = parents1}
            in
               nodeData1
            )
         nodeDataFM0
   in
      PureGraph {nodeDataFM = nodeDataFM1}
