{- Graph defines the Graph class, which defines the basic things a
   graph must do.  Peculiarities:
   (1) Graphs are directed with labelled nodes and
       arcs.
   (2) The nodes and arcs are identified by values of type Node and Arc.
       These values are essentially strings.  The strings are provided by
       the user; there is no mechanism for generating new unique strings.
       (This is because this is easy in the applications I have in mind.)
   (3) A necessary feature of these graphs is that it is supposed to
       be easy generate copies, both on the same system and on others.
 -}
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

   toArc, 
      -- toArc :: String -> IO Arc
   fromArc,
      -- fromArc :: Arc -> IO String
      -- similar to toNode/fromNode.

   -- Updates
   Update(..),
      -- datatype encoding update to shared graph
      -- Parameterised on node vertex type and node label.
      -- node must be an instance of Ord.
      -- Update derives (Read,Show).
      -- The node type as fed to shareGraph and updateShareGraph is
      -- always Node.  However Node is not an instance of Read or Show,
      -- so to convert to/from readable updates you use mapUpdate together
      -- with toNode and fromNode.
   mapUpdate, -- :: (nodeIn -> nodeOut) -> (arcIn -> arcOut) 
              --    -> Update nodeIn nodeLabel arcIn arcLabel 
              --    -> Update nodeOut nodeLabel arcOut arcLabel
   CannedGraph(..),
   CannedGraphNode(..),
      -- contains complete immutable contents of a Graph at some time
      -- also takes a parameter nodeLabel.  nodeLabel must be 
      -- an instance of Read/Show and so is CannedGraph

   GraphConnection(..),
      -- A GraphConnection contains the information generated by one
      -- instance of Graph, which can be used to construct another.

   ) where

import AtomString
import SmallSet
import QuickReadShow
import Selective(EV)

class Graph graph where
   -- access functions
   getNodes :: graph nodeLabel arcLabel -> IO [Node]

   getArcsOut :: graph nodeLabel arcLabel -> Node -> IO [Arc]
   getArcsIn :: graph nodeLabel arcLabel -> Node -> IO [Arc]
   getNodeLabel :: graph nodeLabel arcLabel -> Node -> IO nodeLabel

   getSource :: graph nodeLabel arcLabel -> Arc -> IO Node
   getTarget :: graph nodeLabel arcLabel -> Arc -> IO Node
   getArcLabel :: graph nodeLabel arcLabel -> Arc -> IO nodeLabel

   shareGraph :: graph nodeLabel arcLabel -> 
      IO (GraphConnection nodeLabel arcLabel)
   newGraph :: GraphConnection nodeLabel arcLabel -> 
      IO (graph nodeLabel arcLabel)

   update :: graph nodeLabel arcLabel 
      -> Update Node nodeLabel Arc arcLabel -> IO ()

------------------------------------------------------------------------
-- GraphConnection
------------------------------------------------------------------------

data GraphConnection nodeLabel arcLabel = GraphConnection {   
   graphState :: CannedGraph nodeLabel arcLabel, 
      -- current state of graph
   graphUpdates :: EV(Update Node nodeLabel Arc arcLabel), 
      -- changes since the canned graph was made, apart from
      -- those communicated via the graphUpdate action.
      -- Becomes inaction after deRegister is called.
   deRegister :: IO (),
      -- disables graphUpdates
   graphUpdate :: Update Node nodeLabel Arc arcLabel -> IO()
      -- Similar to update (in class definition) except that
      -- it doesn't get echoed on graphUpdates.
   }

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
-- Arcs
------------------------------------------------------------------------

newtype Arc = Arc AtomString deriving (Eq,Ord)
 
toArc :: String -> IO Arc
toArc arcString = 
   do
      atomString <- mkAtom arcString
      return (Arc atomString)

fromArc :: Arc -> IO String
fromArc (Arc atomString) = readAtom atomString

------------------------------------------------------------------------
-- Update
------------------------------------------------------------------------

data Update node nodeLabel arc arcLabel =
      NewNode node nodeLabel
   |  DeleteNode node
   |  SetNodeLabel node nodeLabel
   |  NewArc arc arcLabel node node
   |  DeleteArc arc
   |  SetArcLabel arc arcLabel

mapUpdate :: (nodeIn -> nodeOut) -> (arcIn -> arcOut) 
   -> Update nodeIn nodeLabel arcIn arcLabel 
   -> Update nodeOut nodeLabel arcOut arcLabel
mapUpdate mapNode mapArc update =
   case update of
      NewNode nodeIn nodeLabel -> NewNode (mapNode nodeIn) nodeLabel
      DeleteNode nodeIn -> DeleteNode (mapNode nodeIn)
      SetNodeLabel nodeIn nodeLabel -> SetNodeLabel (mapNode nodeIn) nodeLabel
      NewArc arcIn arcLabel nodeSource nodeTarget ->
         NewArc (mapArc arcIn) arcLabel (mapNode nodeSource) 
            (mapNode nodeTarget)
      DeleteArc arcIn -> DeleteArc (mapArc arcIn)
      SetArcLabel arcIn arcLabel -> SetArcLabel (mapArc arcIn) arcLabel
 
------------------------------------------------------------------------
-- CannedGraph
------------------------------------------------------------------------

data CannedGraph nodeLabel arcLabel =
   CannedGraph [CannedGraphNode nodeLabel] deriving (Read,Show)

data (Read nodeLabel,Show nodeLabel) => CannedGraphNode nodeLabel =
   CannedGraphNode {
      nodeString :: String,
      successorStrings :: [String],
      nodeLabel :: nodeLabel
      }

instance (Read nodeLabel,Show nodeLabel) => 
      QuickRead (CannedGraphNode nodeLabel) where
   quickRead = WrapRead (\ (nodeString,successorStrings,nodeLabel) ->
      CannedGraphNode {
         nodeString = nodeString,
         successorStrings = successorStrings,
         nodeLabel = nodeLabel
         }
      )

instance (Read nodeLabel,Show nodeLabel) => 
      QuickShow (CannedGraphNode nodeLabel) where
   quickShow = WrapShow (\ cgn ->
      (nodeString cgn,successorStrings cgn,nodeLabel cgn)
      )

emptyCannedGraph :: (Read nodeLabel,Show nodeLabel) => CannedGraph nodeLabel
emptyCannedGraph = CannedGraph []


