{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}

-- |
-- Description: Basic Interface for Graph Display
--
-- In UniForM we need ways of displaying typed directed graphs.
-- In the original UniForM, it was only possible to use the DaVinci
-- encapsulation for displaying directed graphs.  While this is very good,
-- in the new UniForM it is intended to factor out this encapsulation
-- so that it will not be too difficult to replace DaVinci by other
-- graph-drawing package (or variants of DaVinci) for particular graphs.
-- Example alternatives that might be considered:
-- (1) some sort of text-only interface.
-- (2) Windows-style displaying of a tree structure using clickable
--     folders.
-- In this module we present the classes that any such \"graph-drawing package\"
-- is supposed to implement.
--
-- This module is in two parts.
--
-- The first part contains the
-- \"user-friendly\" versions of the functions.  For these, it is assumed
-- (as will usually be the case) that there is only one
-- node\/nodeType\/arc\/arcType around for a particular graph.  The whole lot
-- is indexed by the GraphAll, which contains ALL the functionality
-- required for accessing the graphs (apart from configuration options).
-- For example, the only daVinci-specific thing you should need to use
-- to write a program which calls daVinci will be the daVinciSort variable.
--
-- The second part contains the \"user-hateful\" versions.  All the
-- user-hateful functions have names ending in \"Prim\".
-- Graph display implementations only have to implement the user-hateful
-- versions.  The user-hateful versions should only be of interest to other
-- people if the graph display provides more than one implementation of
-- the NodeClass, NodeTypeClass (or whatever) implementation.  One
-- disadvantage to the user of using the user-hateful versions of the
-- functions is that because of all the overloading, you have to put
-- in lots of explicit types, or else get the most hideous type errors.
--
-- Configuring things like graph titles, shape of node boxes, menus,
-- and so on should also be implemented, where possible, by graph display
-- interfaces.  The various options are documented in GraphConfigure.hs.
-- They should be applied using the Computation.HasConfig interface.
--
-- The types which are supposed in various combinations to be instances
-- of the classes are as follows:
--
--    graph.  This corresponds to one graph display.
--    graphConfig.  This is configuration information for a graph.
--       This might be a window title or size for example.
--    graphParms.  This is a collection of graphConfig's used to
--       construct a graph.
--
-- Nodes and arcs carry values.  Thus all the following carry
-- a type parameter.  But, for ease of implementation with, for example,
-- DaVinci, the type parameter is required to be an instance of 'Typeable'.
--
-- *   node.  A value of this type is an actual node in a graph.
--       (Will be an instance of 'Typeable' via 'Typeable'.)
--
-- *   nodeType.  Nodes are created with a particular UniForM \"type\" which
--       is a Haskell value of type nodetype.  In fact a graph might
--       conceivably have multiply Haskell types corresponding to node
--       and nodeType, meaning that nodes, or their UniForM types,
--       will be distinguished additionally by the Haskell type system.
--
-- *   nodeTypeConfig.  Configuration information for a nodeType.
--       This might include how a node with this type is to be displayed
--       graphically.  This also includes information on what to do when the
--       node is clicked.
--
-- *   nodeTypeParms.  A collection of nodeTypeConfig's used to construct
--       a nodeType
--
--    Similar constructions for arcs . . .
--    arc.
--    arcType.
--    arcTypeConfig.
--    arcTypeParms.
--
--
-- There are quite a lot of classes.  This is partly because of the need
-- to have a separate class for each subset of the type variables
-- which is actually used in the type of a function.
--
-- This file is fairly repetitive, mainly because of the need to
-- repeat the configuration machinery over and over again.
--
-- The functionality provided in this file is inspired by that
-- provided by DaVinci.  However we extend it by allowing
-- nodes to have labels.
--
-- This file should be read in conjunction with "GraphConfigure",
-- which contains various configuration options to be used for
-- graph objects.
--
-- Additional Notes
-- ----------------
--
-- (1) At the end of a program using a GraphDisp instance,
--     'shutdown' should be called.  For example,
--     in the case of the DaVinci instance this is
--     required to get rid of the DaVinci and HTk processes.
--
-- (2) It is more cumbersome writing the Graph Editor than I would
--     like because the menu code doesn't give you
--     direct access to the node or arc type.  Unfortunately doing this
--     would make the classes in this file even more complicated than
--     they are now.
--
module Graphs.GraphDisp(
   -- * User-Friendly Interface.
   -- | You should not need any more than this for drawing graphs.
   Graph(..), -- (a type)
      -- Graph takes a lot of parameters I don't want to mentiond
      -- I shall write them in this module signature as "...".
   newGraph,    -- :: Graph ... -> graphParms
      -- The argument to newGraph is just there to provide the types
      -- and isn't looked at.  Each implementation will provide a trivial
      -- one to start off with; for daVinci it is daVinciSort.
                --    -> IO (Graph ...)
   redraw,      -- :: Graph ... -> IO ()

   getMultipleNodes,
      -- :: Graph ... ->  (Event (WrappedNode node) -> IO a) -> IO a

   GraphParms(emptyGraphParms),

   newNode,      -- :: Graph ... -> nodeType value -> value -> IO (node value)
   setNodeType,  -- :: Graph ... -> node value -> nodeType value -> IO ()
   deleteNode,   -- :: Graph ... -> node value -> IO ()
   setNodeFocus,  -- :: Graph ... -> node value -> IO ()
   getNodeValue, -- :: Graph ... -> node value -> IO value
   setNodeValue, -- :: Graph ... -> node value -> value -> IO ()
      -- WARNING.  setNodeValue should not be used with nodes with
      -- types which specify ValueTitleSource, as the results are
      -- undefined.
   newNodeType,  -- :: Graph ... -> nodeTypeParms value -> IO (nodeType value)
   NodeTypeParms(..),

   newArc,      -- :: Graph ... -> arcType value -> value
                --    -> node nodeFromValue -> node nodeToValue
                --    -> IO (arc value)

   WrappedNode(..),
   newArcListDrawer,
                -- :: Graph .. -> node nodeFromValue
                -- -> ListDrawer (arcType value,value,WrappedNode node)
                --    (arc value)

   deleteArc,   -- :: Graph ... -> arc value
                --    -> IO ()
   setArcValue, -- :: Graph ... -> arc value
                --    -> value -> IO ()
   setArcType,  -- :: Graph ... -> arc value -> value -> IO ()
      -- WARNING.  For daVinci at least, this function is not currently
      -- implemented
   getArcValue, -- :: Graph ... -> arc value
                --    -> IO value
   newArcType,  -- :: Graph ... -> arcTypeParms value -> IO (arcType value)
   ArcTypeParms(..),

   Eq1(..),Ord1(..), -- Classes with nodes and arcs should instance,
      -- allowing comparisongs on them.

   -- * User-Hateful Interface
   -- | This is only needed
   -- by people wanting to implement new implementations of the interface.
   GraphAll(displaySort),
   GraphClass(..),
   NewGraph(..),
   GraphConfig,

   NewNode(..),
   DeleteNode(..),
   SetNodeFocus(..),
   NodeClass,
   NodeTypeClass,
   NewNodeType(..),
   NodeTypeConfig,

   NewArc(..),
   SetArcType(..),
   DeleteArc(..),
   ArcClass,
   ArcTypeClass(..),
   NewArcType(..),
   ArcTypeConfig,

   ) where

import Data.Typeable

import Util.VariableList hiding (redraw)
import Util.Delayer(HasDelayer(..))

import Events.Events(Event)

import Events.Destructible

------------------------------------------------------------------------
-- This is the start of the user-friendly interface.
------------------------------------------------------------------------

--- Graphs

-- | The graph implementation will provide a value of this type to
-- get you started.  For example, for daVinci this is called 'daVinciSort'.
-- However you then need to use it as an argument to 'newGraph' to construct
-- the actual graph.
newtype
   GraphAll graph graphParms node nodeType nodeTypeParms arc arcType
      arcTypeParms
   => Graph graph graphParms node nodeType nodeTypeParms arc arcType
      arcTypeParms
   = Graph graph deriving (Eq,Ord)


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

instance (GraphAll graph graphParms node nodeType nodeTypeParms arc arcType
   arcTypeParms)
   => HasDelayer (Graph graph graphParms node nodeType nodeTypeParms arc
      arcType arcTypeParms) where
   toDelayer (Graph graph) = toDelayer graph

-- | Construct a new graph.  The input value will be something like
-- "DaVinciGraph"'s value 'daVinciSort'; the resulting graph will be
-- returned.
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

-- | Redraw the graph.  This is needed when you want to show updates.
redraw :: (GraphAll graph graphParms node nodeType nodeTypeParms arc
   arcType arcTypeParms) =>
   (Graph graph graphParms node nodeType nodeTypeParms arc arcType
      arcTypeParms)
   -> IO ()
redraw (Graph graph) = redrawPrim graph

-- | Take over all interaction on the graph, and perform the given
-- action, supplying it with an event which is activated when the user
-- double-clicks a node.  This is helpful when you need an interaction
-- selecting several nodes.
getMultipleNodes :: (GraphAll graph graphParms node nodeType nodeTypeParms arc
   arcType arcTypeParms) =>
   (Graph graph graphParms node nodeType nodeTypeParms arc arcType
      arcTypeParms)
   -> (Event (WrappedNode node) -> IO a) -> IO a
getMultipleNodes (Graph graph) = getMultipleNodesPrim graph

class GraphParms graphParms where
   emptyGraphParms :: graphParms

-- Nodes

-- | construct a new node.
newNode :: (GraphAll graph graphParms node nodeType nodeTypeParms
              arc arcType arcTypeParms,Typeable value) =>
   (Graph graph graphParms node nodeType nodeTypeParms arc arcType arcTypeParms)
   -> nodeType value -> value -> IO (node value)
newNode
   ((Graph graph :: Graph graph graphParms node nodeType nodeTypeParms arc
      arcType arcTypeParms))
   (nodeType :: nodeType value)
   (value :: value) = (newNodePrim graph nodeType value) :: IO (node value)

-- | set a node's type
setNodeType :: (GraphAll graph graphParms node nodeType nodeTypeParms
              arc arcType arcTypeParms,Typeable value) =>
   (Graph graph graphParms node nodeType nodeTypeParms arc arcType arcTypeParms)
   -> node value -> nodeType value -> IO ()
setNodeType
   ((Graph graph :: Graph graph graphParms node nodeType nodeTypeParms arc
      arcType arcTypeParms))
   (node :: node value)
   (nodeType :: nodeType value)
      = (setNodeTypePrim graph node nodeType) :: IO ()


-- | delete a node
deleteNode :: (GraphAll graph graphParms node nodeType nodeTypeParms
                 arc arcType arcTypeParms,Typeable value) =>
   (Graph graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms)
   -> node value -> IO ()
deleteNode
   ((Graph graph :: Graph graph graphParms node nodeType nodeTypeParms arc
      arcType arcTypeParms))
   (node :: node value) = deleteNodePrim graph node

setNodeFocus :: (GraphAll graph graphParms node nodeType nodeTypeParms
                 arc arcType arcTypeParms,Typeable value) =>
   (Graph graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms)
   -> node value -> IO ()
setNodeFocus
   ((Graph graph :: Graph graph graphParms node nodeType nodeTypeParms arc
      arcType arcTypeParms))
   (node :: node value) = setNodeFocusPrim graph node



-- | get the value associated with a node
getNodeValue :: (GraphAll graph graphParms node nodeType nodeTypeParms
                   arc arcType arcTypeParms,Typeable value) =>
   (Graph graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms)
   -> node value -> IO value
getNodeValue
   (Graph graph :: Graph graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms)
   (node :: node value) = getNodeValuePrim graph node

-- | set the value associated with a node.
setNodeValue :: (GraphAll graph graphParms node nodeType nodeTypeParms
                   arc arcType arcTypeParms,Typeable value) =>
   (Graph graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms)
   -> node value -> value -> IO ()
setNodeValue
   (Graph graph :: Graph graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms)
   (node :: node value) value = setNodeValuePrim graph node value

-- | construct a node type.
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

   coMapNodeTypeParms :: (Typeable value1,Typeable value2) =>
      (value2 -> value1) -> nodeTypeParms value1 -> nodeTypeParms value2

-- Arcs

-- | construct a new arc.
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

-- | Given a node, construct a 'ListDrawer' which can be used as a way
-- of drawing ordered sets of out-arcs from that node.
-- (NB.  At the moment daVinci does not do this properly, but that is
-- daVinci's fault, not mine.)
newArcListDrawer :: (GraphAll graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms,
      Typeable value,Typeable nodeFromValue)
  => (Graph graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms)
  -> node nodeFromValue
  -> ListDrawer (arcType value,value,WrappedNode node) (arc value)
newArcListDrawer
   (Graph graph :: Graph graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms)
   (nodeFrom :: node nodeFromValue) =
   (newArcListDrawerPrim graph nodeFrom)
--      :: (ListDrawer (arcType value,value,WrappedNode node) (arc value))

-- | delete an arc
deleteArc :: (GraphAll graph graphParms node nodeType nodeTypeParms
                arc arcType arcTypeParms,
                Typeable value)=>
   (Graph graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms) ->  arc value -> IO ()
deleteArc
   (Graph graph :: Graph graph graphParms node nodeType nodeTypeParms arc
      arcType arcTypeParms)
   (arc :: arc value) = deleteArcPrim graph arc

-- | set the value associated with an arc
setArcValue :: (GraphAll graph graphParms node nodeType nodeTypeParms
                  arc arcType arcTypeParms,Typeable value) =>
   (Graph graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms) -> arc value -> value -> IO ()
setArcValue
   (Graph graph :: Graph graph graphParms node nodeType nodeTypeParms arc
      arcType arcTypeParms)
   (arc :: arc value) (value :: value) = setArcValuePrim graph arc value

setArcType :: (GraphAll graph graphParms node nodeType nodeTypeParms
                  arc arcType arcTypeParms,Typeable value) =>
   (Graph graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms) -> arc value -> arcType value -> IO ()
setArcType
   (Graph graph :: Graph graph graphParms node nodeType nodeTypeParms arc
      arcType arcTypeParms)
   (arc :: arc value) (arcType :: arcType value)
      = setArcTypePrim graph arc arcType

-- | get the value associated with an arc
getArcValue :: (GraphAll graph graphParms node nodeType nodeTypeParms
                  arc arcType arcTypeParms,Typeable value) =>
   (Graph graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms) -> arc value -> IO value
getArcValue
   (Graph graph :: Graph graph graphParms node nodeType nodeTypeParms arc
      arcType arcTypeParms)
   (arc :: arc value) = getArcValuePrim graph arc

-- | create a new arc type
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

   -- This is a special arc type parms which will not be drawn at all.
   -- The effect of setting options to an invisibleArcTypeParms object
   -- is for now undefined.
   invisibleArcTypeParms :: Typeable value => arcTypeParms value

   coMapArcTypeParms :: (Typeable value1,Typeable value2) =>
      (value2 -> value1) -> arcTypeParms value1 -> arcTypeParms value2

------------------------------------------------------------------------
-- This is the start of the user-hateful interface.
------------------------------------------------------------------------

-- The GraphAll class indicates that a set of types have the complete
-- graph-displaying functionality we need.
--
class (GraphClass graph,NewGraph graph graphParms,GraphParms graphParms,
   NewNode graph node nodeType,DeleteNode graph node,SetNodeFocus graph node,
   NodeClass node,Typeable node,NodeTypeClass nodeType,
   NewNodeType graph nodeType nodeTypeParms,NodeTypeParms nodeTypeParms,
   NewArc graph node node arc arcType,
   SetArcType graph arc arcType,
   DeleteArc graph arc,
   ArcClass arc,Typeable arc,ArcTypeClass arcType,
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
   NewNode graph node nodeType,DeleteNode graph node,SetNodeFocus graph node,
   NodeClass node,NodeTypeClass nodeType,
   NewNodeType graph nodeType nodeTypeParms,NodeTypeParms nodeTypeParms,
   NewArc graph node node arc arcType,
   SetArcType graph arc arcType,
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

class (Destructible graph,Ord graph,Typeable graph,HasDelayer graph)
      => GraphClass graph where
   redrawPrim :: graph -> IO ()
   -- done after updates have been added

   -- The Delayer temporarily disables redraw actions, or at least tries to.
   -- (For daVinci the situation is that redraw actions currently have to
   -- be forced anyway when attributes are changed.)

class (GraphClass graph,GraphParms graphParms)
   => NewGraph graph graphParms where
   newGraphPrim :: graphParms -> IO graph

class GraphConfig graphConfig
-- empty to prevent just anything being usable for the graphConfig
-- function.

------------------------------------------------------------------------
-- Nodes
------------------------------------------------------------------------

class (GraphClass graph,NodeClass node,NodeTypeClass nodeType) =>
      NewNode graph node nodeType where
   newNodePrim :: Typeable value =>
      graph -> nodeType value -> value -> IO (node value)
   setNodeTypePrim :: Typeable value =>
      graph -> node value -> nodeType value -> IO ()

class (GraphClass graph,NodeClass node) =>
      DeleteNode graph node where
   deleteNodePrim :: Typeable value =>
      graph -> node value -> IO ()
   getNodeValuePrim :: Typeable value =>
      graph -> node value -> IO value
   setNodeValuePrim :: Typeable value =>
      graph -> node value -> value -> IO ()

      -- WARNING.  setNodeValuePrim should not be used with nodes with
      -- types which specify ValueTitleSource, as the results are
      -- undefined.

   getMultipleNodesPrim :: graph -> (Event (WrappedNode node) -> IO a) -> IO a
   -- Running this function disables all other user interaction on
   -- the graph and creates
   -- a new event which occurs each time the node is double-clicked,
   -- assuming this graph was created with drag-and-drop enabled.
   -- Using this event we execute the given action, until it terminates,
   -- when we restore normal user interaction.
   -- NB.  This function must not be nested, or it will block.


class (GraphClass graph,NodeClass node) =>
      SetNodeFocus graph node where
   setNodeFocusPrim :: Typeable value =>
      graph -> node value -> IO ()


class (Typeable node,Ord1 node) => NodeClass node

class Typeable (nodeType :: * -> *) => NodeTypeClass nodeType

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
   newArcListDrawerPrim ::
      (Typeable value,Typeable nodeFromValue)
      => graph -> nodeFrom nodeFromValue
      -> ListDrawer (arcType value,value,WrappedNode nodeTo) (arc value)

class (ArcClass arc,ArcTypeClass arcType) => SetArcType graph arc arcType where
   setArcTypePrim ::
      Typeable value => graph -> arc value -> arcType value -> IO ()

data WrappedNode node = forall value . Typeable value
   => WrappedNode (node value)

class (GraphClass graph,ArcClass arc) => DeleteArc graph arc where
   deleteArcPrim :: (Typeable value) => graph -> arc value -> IO ()
   setArcValuePrim  :: Typeable value => graph -> arc value -> value -> IO ()
   getArcValuePrim :: Typeable value => graph -> arc value -> IO value

class (Typeable arc,Ord1 arc) => ArcClass arc

class (Typeable arcType,Ord1 arcType) => ArcTypeClass arcType where
   -- This is a special arc type which stops the arc being drawn at all.
   invisibleArcType :: Typeable value => arcType value

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

-- ----------------------------------------------------------------------
-- Classes for comparing and equality for types of kind 1
-- ----------------------------------------------------------------------

class Eq1 takesParm where
   eq1 :: takesParm value1 -> takesParm value1 -> Bool

class Eq1 takesParm => Ord1 takesParm where
   compare1 :: takesParm value1 -> takesParm value1 -> Ordering
