{- Graph defines the Graph class, which defines the basic things a
   graph must do.  Graphs are directed with labelled nodes (but
   not labelled arcs).  The node type is fixed to be Node.
   Two arcs may not have identical start- and end-
   vertices. -}
module Graph(
   Graph(..), -- the Graph class

   -- Nodes
   Node,
      -- nodes are essentially Strings.  Allocating these nodes in
      -- a unique way is the caller's responsibility
      -- Node is an instance of (Eq,Ord)
   toNode, 
      -- toNode :: String -> IO Node
   fromNode,
      -- fromNode :: Node -> IO String
      -- toNode and fromNode are guaranteed to behave as the identity function
      -- were String to be identified with String. 

   -- Updates
   Update(..),
      -- datatype encoding update to shared graph
      -- Parameterised on node vertex type and node label.
      -- node must be an instance of Ord.
      -- Update derives (Read,Show).
      -- The node type as fed to shareGraph and updateShareGraph is
      -- always Node.  However Node is not an instance of Read or Show,
      -- so to convert to/from readable updates you use mapMUpdate together
      -- with toNode and fromNode.
   mapMUpdate, -- :: (Ord nodeIn,Ord nodeOut) => 
      --                (nodeIn -> IO nodeOut) -> Update nodeIn nodeLabel ->
      -- IO (Update nodeOut nodeLabel)
      -- map updates monadically by node label.
   ) where

import AtomString
import SmallSet

class Graph graph where
   getNodes :: graph nodeLabel -> IO [Node]
   getPredecessors :: graph nodeLabel -> Node -> IO [Node]
   getSuccessors :: graph nodeLabel -> Node -> IO [Node]
   getLabel :: graph nodeLabel -> Node -> IO nodeLabel
   updateGraph :: graph nodeLabel -> Update Node nodeLabel -> IO ()

------------------------------------------------------------------------
-- Nodes
------------------------------------------------------------------------

newtype Node = Node AtomString deriving (Eq,Ord)
 
toNode :: String -> IO Node
toNode nodeString = 
   do
      atomString <- mkAtom nodeString
      return (Node atomString)

fromNode :: Node -> IO String
fromNode (Node atomString) = readAtom atomString

------------------------------------------------------------------------
-- Update
------------------------------------------------------------------------

data (Ord node) => Update node nodeLabel =
   EditNode {
      node :: node, 
      -- if this node is not in graph add it,
      -- likewise for nodes not previously mentioned in
      -- pred and succ list.  When a node is added, its 
      -- predecessors, successors and label are all empty unless
      -- values are specified/              
      predecessorsOpt :: Maybe (SmallSet node), 
         -- replace predecessors if Just.
      successorsOpt :: Maybe (SmallSet node), 
         -- ditto successors
      nodeLabelOpt :: Maybe nodeLabel -- ditto label
      } deriving (Read,Show)

mapMUpdate :: (Ord nodeIn,Ord nodeOut) => 
      (nodeIn -> IO nodeOut) -> Update nodeIn nodeLabel -> 
      IO (Update nodeOut nodeLabel)
mapMUpdate act (EditNode {
      node = nodeIn,
      predecessorsOpt = predecessorsInOpt,
      successorsOpt = successorsInOpt,
      nodeLabelOpt = nodeLabelOpt
      }) =
   do
      nodeOut <- act nodeIn
      let
         mapSmallSetOpt Nothing = return Nothing
         mapSmallSetOpt (Just smallSetIn) =
            do
               smallSetOut <- mapMSmallSet act smallSetIn
               return (Just smallSetOut)
      predecessorsOutOpt <- mapSmallSetOpt predecessorsInOpt
      successorsOutOpt <- mapSmallSetOpt successorsInOpt
      return (EditNode {
         node = nodeOut,
         predecessorsOpt = predecessorsOutOpt,
         successorsOpt = successorsOutOpt,
         nodeLabelOpt = nodeLabelOpt
         })  
                      

