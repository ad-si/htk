{- In the original UniForM, it was only possible to use the DaVinci
   encapsulation for displaying directed graphs.  While this is very good,
   in the new UniForM it is intended to factor out this encapsulation
   so that it will not be too difficult to replace DaVinci by other
   graph-drawing package (or variants of DaVinci) for particular graphs.
   Example alternatives that might be considered:
   (1) some sort of text-only interface.
   (2) Windows-style displaying of a tree structure using clickable
       folders.
   In this module we present the classes that any such "graph-drawing package"
   is supposed to implement.

   The types which are supposed in various combinations to be instances
   of the classes are as follows:

      graph.  This corresponds to one graph display.
      graphConfig.  This is configuration information for a graph.
         This might be a window title or size for example.
      graphParms.  This is a collection of graphConfig's used to
         construct a graph.

      node.  A value of this type is an actual node in a graph. 
      nodeType.  Nodes are created with a particular UniForM "type" which
         is a Haskell value of type nodetype.  In fact a graph might
         conceivably have multiply Haskell types corresponding to node
         and nodeType, meaning that nodes, or their UniForM types,
         will be distinguished additionally by the Haskell type system. 
      nodeTypeConfig.  Configuration information for a nodeType.
         This might include how a node with this type is to be displayed
         graphically.  This also includes information on what to do when the
         node is clicked.
      nodeTypeParms.  A collection of nodeTypeConfig's used to construct
         a nodeType

      Similar constructions for arcs . . .
      arc.
      arcType.
      arcTypeConfig.
      arcTypeParms.


   There are quite a lot of classes.  This is partly because of the need
   to have a separate class for each subset of the type variables
   which is actually used in the type of a function.

   This file is fairly repetitive, mainly because of the need to
   repeat the configuration machinery over and over again.
   -}
module GraphDisp(
   Graph(..),
   GraphConfig,
   GraphParms(..),
   GraphConfigParms(..),

   NewNode(..),
   DeleteNode(..),
   Node,
   NodeType,
   NewNodeType(..),
   NodeTypeConfig,
   NodeTypeParms(..),
   NodeTypeConfigParms(..),

   NewArc(..),
   DeleteArc(..),
   Arc,
   ArcType,
   NewArcType(..),
   ArcTypeConfig,
   ArcTypeParms(..),
   ArcTypeConfigParms(..),

   MenuButton(..)
   ) where

import Dynamic
import SIM(IA)

------------------------------------------------------------------------
-- Graphs
------------------------------------------------------------------------

class Graph graph where
   redraw :: graph -> IO ()
   -- done after updates have been added

class (Graph graph,GraphParms graphParms) => NewGraph graph graphParms where
   newGraph :: graphParms -> IO graph

class GraphConfig graphConfig where
-- empty to prevent just anything being usable for the graphConfig 
-- function.

class GraphParms graphParms where
   emptyGraphParms :: graphParms

class (GraphConfig graphConfig,GraphParms graphParms) =>
      GraphConfigParms graphConfig graphParms where
   graphConfigUsed :: graphConfig -> graphParms -> Bool
   -- indicates if this instance actually does anything
   -- with this configuration option, unlike the following
   -- instance.

   graphConfig :: graphConfig -> graphParms -> graphParms

instance (GraphConfig graphConfig,GraphParms graphParms) =>
      GraphConfigParms graphConfig graphParms where
   graphConfigUsed _ _ = False
   graphConfig _ parms = parms

------------------------------------------------------------------------
-- Nodes
------------------------------------------------------------------------

class (Graph graph,Node node,NodeType nodeType) =>
      NewNode graph node nodeType where
   newNode :: nodeType -> graph -> Dynamic -> IO node
   readNode :: graph -> node -> IO (nodeType,Dynamic)

class (Graph graph,Node node) =>
      DeleteNode graph node where
   deleteNode :: graph -> node -> IO ()

class Node node where

class NodeType nodeType where

class (NodeType nodeType,NodeTypeParms nodeTypeParms) => 
      NewNodeType nodeType nodeTypeParms where
   newNodeType :: nodeTypeParms -> IO nodeType   

class NodeTypeConfig nodeTypeConfig where
-- empty to prevent just anything being usable for the nodeTypeConfig 
-- function.

class NodeTypeParms nodeTypeParms where
   emptyNodeTypeParms :: nodeTypeParms

class (NodeTypeConfig nodeTypeConfig,NodeTypeParms nodeTypeParms) =>
      NodeTypeConfigParms nodeTypeConfig nodeTypeParms where
   nodeTypeConfigUsed :: nodeTypeConfig -> nodeTypeParms -> Bool
   -- indicates if this instance actually does anything
   -- with this configuration option, unlike the following
   -- instance.

   nodeTypeConfig :: nodeTypeConfig -> nodeTypeParms -> nodeTypeParms

instance (NodeTypeConfig nodeTypeConfig,NodeTypeParms nodeTypeParms) =>
      NodeTypeConfigParms nodeTypeConfig nodeTypeParms where
   nodeTypeConfigUsed _ _ = False
   nodeTypeConfig _ parms = parms

------------------------------------------------------------------------
-- Arcs
------------------------------------------------------------------------

class (Graph graph,Node nodeFrom,Node nodeTo,Arc arc,ArcType arcType) =>
      NewArc graph nodeFrom nodeTo arc arcType where
   newArc :: arcType -> graph -> nodeFrom -> nodeTo -> Dynamic -> IO arc
   readArc :: graph -> arc -> IO (arcType,nodeFrom,nodeTo,Dynamic)

class (Graph graph,Arc arc) => DeleteArc graph arc where
   deleteArc :: graph -> arc -> IO ()

class Arc arc where

class ArcType arcType where

class (ArcType arcType,ArcTypeParms arcTypeParms) => 
      NewArcType arcType arcTypeParms where
   newArcType :: arcTypeParms -> IO arcType   

class ArcTypeConfig arcTypeConfig where
-- empty to prevent just anything being usable for the arcTypeConfig 
-- function.

class ArcTypeParms arcTypeParms where
   emptyArcTypeParms :: arcTypeParms

class (ArcTypeConfig arcTypeConfig,ArcTypeParms arcTypeParms) =>
      ArcTypeConfigParms arcTypeConfig arcTypeParms where
   arcTypeConfigUsed :: arcTypeConfig -> arcTypeParms -> Bool
   -- indicates if this instance actually does anything
   -- with this configuration option, unlike the following
   -- instance.

   arcTypeConfig :: arcTypeConfig -> arcTypeParms -> arcTypeParms

instance (ArcTypeConfig arcTypeConfig,ArcTypeParms arcTypeParms) =>
      ArcTypeConfigParms arcTypeConfig arcTypeParms where
   arcTypeConfigUsed _ _ = False
   arcTypeConfig _ parms = parms

------------------------------------------------------------------------
-- Menus and buttons
-- As in DaVinci, a menu is simply considered as a tree of buttons,
-- allowing an elegant recursive definition.
------------------------------------------------------------------------

data MenuButton =
      Button String (IA Dynamic)
      -- first argument is text to put on button
      -- second is event for when button is pressed.
      -- The dynamic value is that supplied to the node/arc when it
      -- was created.
   |  Menu (Maybe String) [MenuButton]
      -- first argument is title for menu, if any
      -- second argument is list of buttons.



