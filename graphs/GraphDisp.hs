{- In UniForM we need ways of displaying typed directed graphs.
   In the original UniForM, it was only possible to use the DaVinci
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

   This module is in two parts.  

   The first part contains the
   "user-friendly" versions of the functions.  For these, it is assumed
   (as will usually be the case) that there is only one 
   node/nodeType/arc/arcType around for a particular graph.  The whole lot
   is indexed by the GraphAll, which contains ALL the functionality
   required for accessing the graphs (apart from configuration options).
   For example, the only daVinci-specific thing you should need to use
   to write a program which calls daVinci will be the daVinciSort variable.
   
   The second part contains the "user-hateful" versions.  All the 
   user-hateful functions have names ending in "Prim". 
   Graph display implementations only have to implement the user-hateful
   versions.  The user-hateful versions should only be of interest to other
   people if the graph display provides more than one implementation of
   the NodeClass, NodeTypeClass (or whatever) implementation.  One
   disadvantage to the user of using the user-hateful versions of the
   functions is that because of all the overloading, you have to put
   in lots of explicit types, or else get the most hideous type errors.

   Configuring things like graph titles, shape of node boxes, menus,
   and so on should also be implemented, where possible, by graph display
   interfaces.  The various options are documented in GraphConfigure.hs.
   They should be applied using the Computation.HasConfig interface.

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

   This file should be read in conjunction with GraphConfigure.hs,
   which contains various configuration options to be used for
   graph objects.

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
   -- User-friendly interface
   Graph(..), -- (a type)
      -- Graph takes a lot of parameters I don't want to mentiond
      -- I shall write them in this module signature as "...".
   newGraph,    -- :: Graph ... -> graphParms
      -- The argument to newGraph is just there to provide the types
      -- and isn't looked at.  Each implementation will provide a trivial
      -- one to start off with; for daVinci it is daVinciSort. 
                --    -> IO (Graph ...)
   redraw,      -- :: Graph ... -> IO ()
   GraphParms(emptyGraphParms),

   newNode,      -- :: Graph ... -> nodeType value -> value -> IO (node value)
   deleteNode,   -- :: Graph ... -> node value -> IO ()
   setNodeValue, -- :: Graph ... -> node value -> value -> IO ()
   newNodeType,  -- :: Graph ... -> nodeTypeParms value -> IO (nodeType value)
   NodeTypeParms(emptyNodeTypeParms),

   newArc,      -- :: Graph ... -> arcType value -> value 
                --    -> node nodeFromValue -> node nodeToValue
                --    -> IO (arc value)
   deleteArc,   -- :: Graph ... -> arc value 
                --    -> IO ()
   setArcValue, -- :: Graph ... -> arc value
                --    -> value -> IO ()
   newArcType,  -- :: Graph ... -> arcTypeParms value -> IO (arcType value)
   ArcTypeParms(emptyArcTypeParms),

   -- User-hateful interface, arranged by classes.
   GraphAll(displaySort),
   GraphClass(..),
   NewGraph(..),
   GraphConfig,

   NewNode(..),
   DeleteNode(..),
   NodeClass,
   NodeTypeClass,
   NewNodeType(..),
   NodeTypeConfig,

   NewArc(..),
   DeleteArc(..),
   ArcClass,
   ArcTypeClass,
   NewArcType(..),
   ArcTypeConfig,

   ) where

import Dynamics
import ExtendedPrelude(monadDot)
import Computation(HasConfig(..))

import Destructible

------------------------------------------------------------------------
-- This is the start of the user-friendly interface.
------------------------------------------------------------------------

--- Graphs

newtype 
   GraphAll graph graphParms node nodeType nodeTypeParms arc arcType 
      arcTypeParms 
   => Graph graph graphParms node nodeType nodeTypeParms arc arcType 
      arcTypeParms
   = Graph graph


instance (GraphAll graph graphParms node nodeType nodeTypeParms arc arcType
   arcTypeParms) 
   => Destroyable (Graph graph graphParms node nodeType nodeTypeParms arc
      arcType arcTypeParms) where
   destroy (Graph graph) = destroy graph


instance (GraphAll graph graphParms node nodeType nodeTypeParms arc arcType
   arcTypeParms) 
   => Destructible (Graph graph graphParms node nodeType nodeTypeParms arc
      arcType arcTypeParms) where
   destroyed (Graph graph) = destroyed graph

-- The argument to newGraph can be obtained from displaySort
-- (see later in this section), and there should be one of these
-- for each graph display implementation.  EG for daVinci it's
-- called daVinciSort.  So you just have to use 
-- newGraph daVinciSort and then don't have to worry any more about
-- all these type variables.

newGraph :: (GraphAll graph graphParms node nodeType nodeTypeParms arc 
   arcType arcTypeParms) => 
   (Graph graph graphParms node nodeType nodeTypeParms arc arcType
      arcTypeParms) ->
   graphParms
   -> IO (Graph graph graphParms node nodeType nodeTypeParms arc arcType 
         arcTypeParms)
newGraph 
   (_::(Graph graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms)) 
   (graphParms :: graphParms) =
   do
      (graph :: graph) <- newGraphPrim graphParms
      return (Graph graph :: 
         Graph graph graphParms node nodeType nodeTypeParms arc arcType 
         arcTypeParms)

redraw :: (GraphAll graph graphParms node nodeType nodeTypeParms arc 
   arcType arcTypeParms) =>
   (Graph graph graphParms node nodeType nodeTypeParms arc arcType 
      arcTypeParms)
   -> IO ()
redraw (Graph graph) = redrawPrim graph

class GraphParms graphParms where
   emptyGraphParms :: graphParms

-- Nodes

newNode :: (GraphAll graph graphParms node nodeType nodeTypeParms 
              arc arcType arcTypeParms,Typeable value) => 
   (Graph graph graphParms node nodeType nodeTypeParms arc arcType arcTypeParms)
   -> nodeType value -> value -> IO (node value)
newNode 
   ((Graph graph :: Graph graph graphParms node nodeType nodeTypeParms arc 
      arcType arcTypeParms))
   (nodeType :: nodeType value) 
   (value :: value) = (newNodePrim graph nodeType value) :: IO (node value)

deleteNode :: (GraphAll graph graphParms node nodeType nodeTypeParms 
                 arc arcType arcTypeParms,Typeable value) => 
   (Graph graph graphParms node nodeType nodeTypeParms 
      arc arcType arcTypeParms)
   -> node value -> IO ()
deleteNode 
   ((Graph graph :: Graph graph graphParms node nodeType nodeTypeParms arc 
      arcType arcTypeParms))
   (node :: node value) = deleteNodePrim graph node

setNodeValue :: (GraphAll graph graphParms node nodeType nodeTypeParms 
                   arc arcType arcTypeParms,Typeable value) => 
   (Graph graph graphParms node nodeType nodeTypeParms 
      arc arcType arcTypeParms)
   -> node value -> value -> IO ()
setNodeValue
   (Graph graph :: Graph graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms)
   (node :: node value)
   (value :: value) = setNodeValuePrim graph node value

newNodeType :: (GraphAll graph graphParms node nodeType nodeTypeParms 
                  arc arcType arcTypeParms,Typeable value) => 
   (Graph graph graphParms node nodeType nodeTypeParms arc arcType 
      arcTypeParms)
   -> nodeTypeParms value -> IO (nodeType value)
newNodeType
   ((Graph graph :: Graph graph graphParms node nodeType nodeTypeParms
      arc  arcType arcTypeParms))
   (nodeTypeParms :: nodeTypeParms value) = 
      (newNodeTypePrim graph nodeTypeParms) :: IO (nodeType value)

class NodeTypeParms nodeTypeParms where
   emptyNodeTypeParms :: Typeable value =>
      nodeTypeParms value

-- Arcs


newArc :: (GraphAll graph graphParms node nodeType nodeTypeParms 
             arc arcType arcTypeParms,
             Typeable value,Typeable nodeFromValue,Typeable nodeToValue) => 
   (Graph graph graphParms node nodeType nodeTypeParms 
      arc arcType arcTypeParms)
   -> arcType value -> value -> node nodeFromValue -> node nodeToValue
   -> IO (arc value)  
newArc
   (Graph graph :: Graph graph graphParms node nodeType nodeTypeParms 
      arc arcType arcTypeParms)
   (arcType :: arcType value)
   (value :: value)
   (nodeFrom :: node nodeFromValue)
   (nodeTo :: node nodeToValue) =
   (newArcPrim graph arcType value nodeFrom nodeTo) :: IO (arc value)

deleteArc :: (GraphAll graph graphParms node nodeType nodeTypeParms 
                arc arcType arcTypeParms,
                Typeable value)=> 
   (Graph graph graphParms node nodeType nodeTypeParms 
      arc arcType arcTypeParms) ->  arc value -> IO ()
deleteArc 
   (Graph graph :: Graph graph graphParms node nodeType nodeTypeParms arc 
      arcType arcTypeParms)
   (arc :: arc value) = deleteArcPrim graph arc

setArcValue :: (GraphAll graph graphParms node nodeType nodeTypeParms 
                  arc arcType arcTypeParms,Typeable value) => 
   (Graph graph graphParms node nodeType nodeTypeParms 
      arc arcType arcTypeParms) -> arc value -> value -> IO ()
setArcValue
   (Graph graph :: Graph graph graphParms node nodeType nodeTypeParms arc 
      arcType arcTypeParms)
   (arc :: arc value) (value :: value) = setArcValuePrim graph arc value

newArcType :: (GraphAll graph graphParms node nodeType nodeTypeParms 
                 arc arcType arcTypeParms,Typeable value) =>
   (Graph graph graphParms node nodeType nodeTypeParms 
      arc arcType arcTypeParms) ->
   arcTypeParms value -> IO (arcType value)
newArcType
   (Graph graph :: Graph graph graphParms node nodeType nodeTypeParms arc 
      arcType arcTypeParms)
   (arcTypeParms :: arcTypeParms value) =
      (newArcTypePrim graph arcTypeParms) :: IO (arcType value)

class ArcTypeParms arcTypeParms where
   emptyArcTypeParms :: Typeable value => arcTypeParms value

------------------------------------------------------------------------
-- This is the start of the user-hateful interface.
------------------------------------------------------------------------

-- The GraphAll class indicates that a set of types have the complete
-- graph-displaying functionality we need.
-- 
class (GraphClass graph,NewGraph graph graphParms,GraphParms graphParms,
   NewNode graph node nodeType,DeleteNode graph node,
   NodeClass node,HasTyCon1 node,NodeTypeClass nodeType,
   NewNodeType graph nodeType nodeTypeParms,NodeTypeParms nodeTypeParms,
   NewArc graph node node arc arcType,
   DeleteArc graph arc,
   ArcClass arc,HasTyCon1 arc,ArcTypeClass arcType,
   NewArcType graph arcType arcTypeParms
   ) => 
   GraphAll graph graphParms node nodeType nodeTypeParms 
      arc arcType arcTypeParms where

   displaySort :: Graph graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms 
   -- displaySort is a parameter which can be passed to something
   -- which produces graphs and displays them, to indicate what
   -- types we are using.  The only definition of it is about to
   -- be given . . .   

instance (GraphClass graph,NewGraph graph graphParms,GraphParms graphParms,
   NewNode graph node nodeType,DeleteNode graph node,
   NodeClass node,NodeTypeClass nodeType,
   NewNodeType graph nodeType nodeTypeParms,NodeTypeParms nodeTypeParms,
   NewArc graph node node arc arcType,
   DeleteArc graph arc,
   ArcClass arc,ArcTypeClass arcType,
   NewArcType graph arcType arcTypeParms
   ) => 
   GraphAll graph graphParms node nodeType nodeTypeParms 
      arc arcType arcTypeParms where

   displaySort = displaySort

------------------------------------------------------------------------
-- Graphs
------------------------------------------------------------------------

class (Destructible graph) => GraphClass graph where
   redrawPrim :: graph -> IO ()
   -- done after updates have been added

class (GraphClass graph,GraphParms graphParms) 
   => NewGraph graph graphParms where
   newGraphPrim :: graphParms -> IO graph

class GraphConfig graphConfig where
-- empty to prevent just anything being usable for the graphConfig 
-- function.

------------------------------------------------------------------------
-- Nodes
------------------------------------------------------------------------

class (GraphClass graph,NodeClass node,NodeTypeClass nodeType) =>
      NewNode graph node nodeType where
   newNodePrim :: Typeable value => 
      graph -> nodeType value -> value -> IO (node value)

class (GraphClass graph,NodeClass node) =>
      DeleteNode graph node where
   deleteNodePrim :: Typeable value =>
      graph -> node value -> IO ()
   setNodeValuePrim :: Typeable value =>
      graph -> node value -> value -> IO ()

class HasTyCon1 node => NodeClass node

class Kind1 nodeType => NodeTypeClass nodeType

class (GraphClass graph,NodeTypeClass nodeType,NodeTypeParms nodeTypeParms)
   => NewNodeType graph nodeType nodeTypeParms where
   newNodeTypePrim :: Typeable value =>
      graph -> nodeTypeParms value -> IO (nodeType value)   

class Kind1 nodeTypeConfig => NodeTypeConfig nodeTypeConfig
-- empty to prevent just anything being usable for the nodeTypeConfig 
-- function.

------------------------------------------------------------------------
-- Arcs
------------------------------------------------------------------------

class (GraphClass graph,NodeClass nodeFrom,NodeClass nodeTo,ArcClass arc,
      ArcTypeClass arcType) 
   => NewArc graph nodeFrom nodeTo arc arcType where
   newArcPrim :: 
      (Typeable value,Typeable nodeFromValue,Typeable nodeToValue) 
      => graph -> arcType value -> value 
      -> nodeFrom nodeFromValue -> nodeTo nodeToValue
      -> IO (arc value)

class (GraphClass graph,ArcClass arc) => DeleteArc graph arc where
   deleteArcPrim :: (Typeable value) => graph -> arc value -> IO ()
   setArcValuePrim  :: Typeable value => graph -> arc value -> value -> IO ()

class HasTyCon1 arc => ArcClass arc

class Kind1 arcType => ArcTypeClass arcType

class (GraphClass graph,ArcTypeClass arcType,ArcTypeParms arcTypeParms) => 
      NewArcType graph arcType arcTypeParms where
   newArcTypePrim :: Typeable value =>
      graph -> arcTypeParms value -> IO (arcType value)  

class Kind1 arcTypeConfig => ArcTypeConfig arcTypeConfig

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




