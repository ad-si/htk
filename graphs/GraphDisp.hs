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
         (Will be an instance of Typeable via HasTyCon1.)
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
   nodes to have labels.

   Additional Notes
   ----------------
   (1) At the end of a program using a GraphDisp instance,
       InfoBus.shutdown should be called.  For example,
       in the case of the DaVinci instance this is
       required to get rid of the DaVinci and HTk processes.
   (2) It is more cumbersome writing the Graph Editor than I would
       like because the menu code doesn't give you
       direct access to the node or arc type.  Unfortunately doing this
       would make the classes in this file even more complicated than
       they are now.
   -}
module GraphDisp(
   GraphAll(displaySort),
   Graph(..),
   NewGraph(..),
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

   -- LocalMenu describes menus or buttons for objects that carry a value,
   -- IE nodes or arcs.
   LocalMenu(..),

   -- GlobalMenu describes menus or buttons for objects that don't carry a
   -- value, IE graphs.
   GlobalMenu(..),

   -- MenuPrim is supposed to be the generalised Menu/Button type.
   MenuPrim(..), -- a type with TWO parameters.  We provide maps
                       -- and monadic methods for both.
   mapMenuPrim,
   mapMenuPrim',
   mapMMenuPrim,
   mapMMenuPrim',
   

   GraphTitle(..),
   ValueTitle(..),
   Shape(..),
   ) where

import Dynamics
import ExtendedPrelude(monadDot)

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
   Node node,HasTyCon1 node,NodeType nodeType,
   NewNodeType graph nodeType nodeTypeParms,NodeTypeParms nodeTypeParms,
   NewArc graph node node arc arcType,
   GetFrom graph node arc,GetTo graph node arc,
   GetArcType graph arc arcType,DeleteArc graph arc,
   Arc arc,HasTyCon3 arc,ArcType arcType,
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
   setNodeValue :: Typeable value =>
      graph -> node value -> value -> IO ()

class HasTyCon1 node => Node node

class Kind1 nodeType => NodeType nodeType

class (Graph graph,NodeType nodeType,NodeTypeParms nodeTypeParms) => 
      NewNodeType graph nodeType nodeTypeParms where
   newNodeType :: Typeable value =>
      graph -> nodeTypeParms value -> IO (nodeType value)   

class Kind1 nodeTypeConfig => NodeTypeConfig nodeTypeConfig
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
   setArcValue :: 
      (Typeable value,Typeable nodeFromValue,Typeable nodeToValue) 
      => graph -> arc value nodeFromValue nodeToValue -> value -> IO ()

class HasTyCon3 arc => Arc arc

class Kind1 arcType => ArcType arcType

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
-- We define MenuPrim as it may be useful for
-- implementations, so they don't have to define their own datatypes
-- for menus.
------------------------------------------------------------------------

instance GraphConfig GlobalMenu

newtype GlobalMenu = GlobalMenu(MenuPrim (Maybe String) (IO ()))

instance NodeTypeConfig LocalMenu

instance ArcTypeConfig LocalMenu

newtype LocalMenu value = 
   LocalMenu(MenuPrim (Maybe String) (value -> IO()))

data MenuPrim subMenuValue value =
      Button String value
      -- first argument is text to put on button.
      -- second argument generates an action to be performed when the
      -- button is pressed.
      -- The dynamic value is that supplied to the node/arc when it
      -- was created.
   |  Menu subMenuValue [MenuPrim subMenuValue value]
      -- List of buttons with a possible title.
   |  Blank
      -- A Blank can be used to separate groups of menu buttons in the
      -- same menu.     

mapMenuPrim :: (a -> b) -> MenuPrim c a -> MenuPrim c b
mapMenuPrim a2b (Button label a) = Button label (a2b a)
mapMenuPrim a2b (Menu subMenuValue menuButtons) =
   Menu subMenuValue (map (mapMenuPrim a2b) menuButtons)
mapMenuPrim a2b Blank = Blank

mapMenuPrim' :: (c -> d) -> MenuPrim c a -> MenuPrim d a
mapMenuPrim' c2d (Button title action) = Button title action
mapMenuPrim' c2d (Menu subMenuValue menuButtons) =
   Menu (c2d subMenuValue) (map (mapMenuPrim' c2d) menuButtons)
mapMenuPrim' c2d Blank = Blank

mapMMenuPrim :: (Monad m) => (a -> m b) -> MenuPrim c a 
   -> m (MenuPrim c b)
mapMMenuPrim a2bAct (Button label a) =
   do
      b <- a2bAct a
      return (Button label b)
mapMMenuPrim a2bAct (Menu subMenuValue menuButtons) =
   do
      bMenuButtons <- mapM (mapMMenuPrim a2bAct) menuButtons
      return (Menu subMenuValue bMenuButtons)
mapMMenuPrim a2bAct Blank = return Blank

mapMMenuPrim' :: (Monad m) => (c -> m d) -> MenuPrim c a 
   -> m (MenuPrim d a)
mapMMenuPrim' c2dAct (Button title action) = 
   return (Button title action)
mapMMenuPrim' c2dAct (Menu subMenuValue menuButtons) =
   do
      dMenuButtons <- mapM (mapMMenuPrim' c2dAct) menuButtons
      dSubMenuValue <- c2dAct subMenuValue
      return (Menu dSubMenuValue dMenuButtons) 
mapMMenuPrim' c2dAct Blank = return Blank
------------------------------------------------------------------------
-- Titles
------------------------------------------------------------------------

data GraphTitle = GraphTitle String
instance GraphConfig GraphTitle

data ValueTitle value = ValueTitle (value -> IO String)
-- ValueTitles are computed from the node or arc value using the supplied
-- computation when the node or arc is created or when 
-- setNodeValue/setArcValue are called.

instance NodeTypeConfig ValueTitle

instance ArcTypeConfig ValueTitle

------------------------------------------------------------------------
-- Shapes etcetera
------------------------------------------------------------------------

-- This datatype is based on DaVinciClasses.hs, including several
-- name clashes.  However we omit Textual, add the file argument
-- to iconic and the shape Triangle.  This datatype may get bigger!
data Shape nodeLabel = Box | Circle | Ellipse | Rhombus | Triangle | 
   Icon FilePath deriving (Read,Show)

instance NodeTypeConfig Shape

------------------------------------------------------------------------
-- The Kind* classes are a silly hack so that we 
-- can define empty classes of things which take a fixed number of
-- type parameters.  
------------------------------------------------------------------------

class Kind1 takesParm where
   kindOne :: takesParm value -> ()

instance Kind1 takesParm where
   kindOne _ = ()

class Kind2 takes2Parms where
   kindTwo :: takes2Parms value1 value2-> ()

instance Kind2 takesParms where
   kindTwo _ = ()

class Kind3 takes3Parms where
   kindThree :: takes3Parms value1 value2 value3 -> ()

instance Kind3 takesParms where
   kindThree _ = ()




