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

   Nodes and arcs carry values.  Thus all the following carry
   a type parameter.  But, for ease of implementation with, for example,
   DaVinci, the type parameter is required to be an instance of Typeable.

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

   The functionality provided in this file is inspired by that
   provided by DaVinci.  However we extend it by allowing
   nodes to have labels provided in the form of Dynamic values.
   -}
module GraphDisp(
   GraphAll(displaySort),
   Graph(..),
   NewGraph(..),
   GraphConfig,
   GraphParms(..),
   GraphConfigParms(..),
   GConfig(..),
   graphConfigs,

   NewNode(..),
   DeleteNode(..),
   Node,
   NodeType,
   NewNodeType(..),
   NodeTypeConfig,
   NodeTypeParms(..),
   NodeTypeConfigParms(..),
  

   NewArc(..),
   GetFrom(..),
   GetTo(..),
   GetArcType(..),
   DeleteArc(..),
   Arc,
   ArcType,
   NewArcType(..),
   ArcTypeConfig,
   ArcTypeParms(..),
   ArcTypeConfigParms(..),

   MenuButton(..),
   GraphTitle(..),
   ValueTitle(..)
   ) where

import Dynamics
import SIM(IA,Destructible)

------------------------------------------------------------------------
-- GraphAll
-- This class encapsulates all that you can do with a single graph
-- implementation with one node and one arc type.
-- The idea is that a typical user of graphs will have signature something
-- like
-- display :: (GraphAll GraphAll graph graphParms node nodeType nodeTypeParms 
--       arc arcType arcTypeParms) =>
--    (graph,graphParms,node,nodeType,nodeTypeParms,arc,arcType,arcTypeParms)
--    -> (other arguments for example the actual structure to display)
-- Then a call might look like (for daVinci)
-- display 
--   (displaySort :: (DaVinciGraph,DaVinciGraphParms,DaVinciNode,
--    DaVinciNodeType,DaVinciNodeTypeParms,DaVinciArc,DaVinciArcType,
--    DaVinciArcTypeParms)) -> (actual data)
-- 

------------------------------------------------------------------------

class (Graph graph,NewGraph graph graphParms,GraphParms graphParms,
   NewNode graph node nodeType,DeleteNode graph node,
   Node node,NodeType nodeType,
   NewNodeType graph nodeType nodeTypeParms,NodeTypeParms nodeTypeParms,
   NewArc graph node node arc arcType,
   GetFrom graph node arc,GetTo graph node arc,
   GetArcType graph arc arcType,DeleteArc graph arc,
   Arc arc,ArcType arcType,
   NewArcType graph arcType arcTypeParms
   ) => 
   GraphAll graph graphParms node nodeType nodeTypeParms 
      arc arcType arcTypeParms where

   displaySort :: 
          (Typeable a,Typeable b,Typeable c,Typeable d,Typeable e,
          Typeable p,Typeable q,Typeable r) 
      => (graph,graphParms,
      node a,nodeType b,nodeTypeParms c,
      arc p d e,arcType q,arcTypeParms r)
   -- displaySort is a parameter which can be passed to something
   -- which produces graphs and displays them, to indicate what
   -- types we are using.  The only definition of it is about to
   -- be given . . .   

instance (Graph graph,NewGraph graph graphParms,GraphParms graphParms,
   NewNode graph node nodeType,DeleteNode graph node,
   Node node,NodeType nodeType,
   NewNodeType graph nodeType nodeTypeParms,NodeTypeParms nodeTypeParms,
   NewArc graph node node arc arcType,
   GetFrom graph node arc,GetTo graph node arc,
   GetArcType graph arc arcType,DeleteArc graph arc,
   Arc arc,ArcType arcType,
   NewArcType graph arcType arcTypeParms
   ) => 
   GraphAll graph graphParms node nodeType nodeTypeParms 
      arc arcType arcTypeParms where

   displaySort = (bot,bot,bot,bot,bot,bot,bot,bot)
      where
         bot = bot
   

------------------------------------------------------------------------
-- Graphs
------------------------------------------------------------------------

class (Destructible graph) => Graph graph where
   redraw :: graph -> IO ()
   -- done after updates have been added

class (Graph graph,GraphParms graphParms) => NewGraph graph graphParms where
   newGraph :: graphParms -> IO graph

class GraphConfig graphConfig where
-- empty to prevent just anything being usable for the graphConfig 
-- function.

class GraphParms graphParms where
   emptyGraphParms :: graphParms

class (GraphConfig graphConfig,GraphParms graphParms) 
   => GraphConfigParms graphConfig graphParms where
   graphConfigUsed :: graphConfig -> graphParms -> Bool
   -- indicates if this instance actually does anything
   -- with this configuration option, unlike the following
   -- instance.

   graphConfig :: graphConfig -> graphParms -> graphParms

data (GraphParms graphParms) 
   => GConfig graphParms = 
      forall graphConfig . GraphConfigParms graphConfig graphParms 
   => GConfig graphConfig

graphConfigs :: (GraphParms graphParms)
   => [GConfig graphParms] -> graphParms
graphConfigs [] = emptyGraphParms
graphConfigs ((GConfig gConfig):rest) =
   graphConfig gConfig (graphConfigs rest)

------------------------------------------------------------------------
-- Nodes
------------------------------------------------------------------------

class (Graph graph,Node node,NodeType nodeType) =>
      NewNode graph node nodeType where
   newNode :: Typeable value => 
      nodeType value -> graph -> value -> IO (node value)
   getNodeType :: Typeable value =>
      graph -> node value -> IO (nodeType value)

class (Graph graph,Node node) =>
      DeleteNode graph node where
   deleteNode :: Typeable value =>
      graph -> node value -> IO ()
   getNodeValue :: Typeable value =>
      graph -> node value -> IO value

class KindOne node => Node node

class KindOne nodeType => NodeType nodeType

class (Graph graph,NodeType nodeType,NodeTypeParms nodeTypeParms) => 
      NewNodeType graph nodeType nodeTypeParms where
   newNodeType :: Typeable value =>
      graph -> nodeTypeParms value -> IO (nodeType value)   

class KindOne nodeTypeConfig => NodeTypeConfig nodeTypeConfig
-- empty to prevent just anything being usable for the nodeTypeConfig 
-- function.

class NodeTypeParms nodeTypeParms where
   emptyNodeTypeParms :: Typeable value =>
      nodeTypeParms value

class (NodeTypeConfig nodeTypeConfig,NodeTypeParms nodeTypeParms) =>
      NodeTypeConfigParms nodeTypeConfig nodeTypeParms where
   nodeTypeConfigUsed :: Typeable value =>
      nodeTypeConfig value -> nodeTypeParms value -> Bool
   -- indicates if this instance actually does anything
   -- with this configuration option, unlike the following
   -- instance.

   nodeTypeConfig :: Typeable value =>
      nodeTypeConfig value -> nodeTypeParms value -> nodeTypeParms value

instance (NodeTypeConfig nodeTypeConfig,NodeTypeParms nodeTypeParms) =>
      NodeTypeConfigParms nodeTypeConfig nodeTypeParms where
   nodeTypeConfigUsed _ _ = False
   nodeTypeConfig _ parms = parms

------------------------------------------------------------------------
-- Arcs
------------------------------------------------------------------------

class (Graph graph,Node nodeFrom,Node nodeTo,Arc arc,ArcType arcType) 
   => NewArc graph nodeFrom nodeTo arc arcType where
   newArc :: 
      (Typeable value,Typeable nodeFromValue,Typeable nodeToValue) 
      => arcType value -> graph -> value 
      -> nodeFrom nodeFromValue -> nodeTo nodeToValue
      -> IO (arc value nodeFromValue nodeToValue)

class (Graph graph,Node nodeFrom,Arc arc) 
   => GetFrom graph nodeFrom arc where
   getFrom :: 
      (Typeable value,Typeable nodeFromValue,Typeable nodeToValue) 
      => graph -> arc value nodeFromValue nodeToValue 
      -> IO (nodeFrom nodeFromValue)

class (Graph graph,Node nodeTo,Arc arc) 
   => GetTo graph nodeTo arc where
   getTo :: 
      (Typeable value,Typeable nodeFromValue,Typeable nodeToValue) 
      => graph
      -> arc value nodeFromValue nodeToValue 
      -> IO (nodeTo nodeToValue)

class (Graph graph,Arc arc,ArcType arcType) 
   => GetArcType graph arc arcType where
   getArcType ::
      (Typeable value,Typeable nodeFromValue,Typeable nodeToValue) 
      => graph
      -> arc value nodeFromValue nodeToValue
      -> IO (arcType value)

class (Graph graph,Arc arc) => DeleteArc graph arc where
   deleteArc :: 
      (Typeable value,Typeable nodeFromValue,Typeable nodeToValue) 
      => graph -> arc value nodeFromValue nodeToValue -> IO ()
   getArcValue :: 
      (Typeable value,Typeable nodeFromValue,Typeable nodeToValue) 
      => graph -> arc value nodeFromValue nodeToValue -> IO value

class KindThree arc => Arc arc

class KindOne arcType => ArcType arcType

class (Graph graph,ArcType arcType,ArcTypeParms arcTypeParms) => 
      NewArcType graph arcType arcTypeParms where
   newArcType :: Typeable value =>
      graph -> arcTypeParms value -> IO (arcType value)  

class ArcTypeConfig arcTypeConfig where
-- empty to prevent just anything being usable for the arcTypeConfig 
-- function.  However we include a totally useless function so Haskell
-- can work out what the kind of nodeTypeConfig is.
   nullArcTypeConfig :: Typeable value =>
      arcTypeConfig value -> ()
   nullArcTypeConfig _ = ()

class ArcTypeParms arcTypeParms where
   emptyArcTypeParms :: Typeable value => arcTypeParms value

class (ArcTypeConfig arcTypeConfig,ArcTypeParms arcTypeParms) =>
      ArcTypeConfigParms arcTypeConfig arcTypeParms where
   arcTypeConfigUsed :: Typeable value =>
      arcTypeConfig value -> arcTypeParms value -> Bool
   -- indicates if this instance actually does anything
   -- with this configuration option, unlike the following
   -- instance.

   arcTypeConfig :: Typeable value => 
      arcTypeConfig value -> arcTypeParms value -> arcTypeParms value
      
------------------------------------------------------------------------
-- Menus and buttons
-- As in DaVinci, a menu is simply considered as a tree of buttons,
-- allowing an elegant recursive definition.
------------------------------------------------------------------------

instance NodeTypeConfig MenuButton

instance ArcTypeConfig MenuButton

data Typeable value => MenuButton value =
      Button String (value -> IO ())
      -- first argument is text to put on button.
      -- second argument generates an action to be performed when the
      -- button is pressed.
      -- The dynamic value is that supplied to the node/arc when it
      -- was created.
   |  Menu (Maybe String) [MenuButton value]
      -- List of buttons with a possible title..

------------------------------------------------------------------------
-- Titles
------------------------------------------------------------------------

data GraphTitle = GraphTitle String
instance GraphConfig GraphTitle

data ValueTitle value = ValueTitle (value -> IO String)

instance NodeTypeConfig ValueTitle

instance ArcTypeConfig ValueTitle
 
------------------------------------------------------------------------
-- The KindOne class and KindThree classes are a silly hack so that we 
-- can define empty classes of things which take a fixed number of
-- type parameters.  
------------------------------------------------------------------------

class KindOne takesParm where
   kindOne :: takesParm value -> ()

instance KindOne takesParm where
   kindOne _ = ()

class KindThree takes3Parms where
   kindThree :: takes3Parms value1 value2 value3 -> ()

instance KindThree takesParms where
   kindThree _ = ()




