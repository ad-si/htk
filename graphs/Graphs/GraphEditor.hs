{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | #########################################################################
--
-- This Graph Editor is inspired by the one by Einar Karlsen but uses
-- the new graph interface.
--
-- #########################################################################


module Graphs.GraphEditor (
   newGraphEditor, -- start a GraphEditor, given a Graph

   GraphEditor, -- a running GraphEditor

   -- Graph types associated with graph editors
   Displayable,
   DisplayableUpdate,
   DisplayableGraphConnection,
   DisplayableCannedGraph,
   ) where

import Control.Concurrent(forkIO,killThread)

import Util.Registry
import Util.Computation(done)
import Util.Object
import Util.Dynamics

import Reactor.InfoBus
import Events.Events
import Events.Channels
import Events.Destructible

import Graphs.DisplayGraph
import Graphs.Graph
import qualified Graphs.GraphDisp as GraphDisp
import Graphs.GraphConfigure
import Graphs.GetAttributes

type Displayable graph =
   graph String (NodeTypeAttributes Node) () ArcTypeAttributes

-- DisplayableUpdate, DisplayableGraphConnection and DisplayableCannedGraph
-- are used elsewhere to refer to the types associated with an editable graph.
type DisplayableUpdate =
   Update String (NodeTypeAttributes Node) () ArcTypeAttributes

type DisplayableGraphConnection =
   GraphConnection String (NodeTypeAttributes Node) () ArcTypeAttributes

type DisplayableCannedGraph =
   CannedGraph String (NodeTypeAttributes Node) () ArcTypeAttributes


newGraphEditor ::
   (GraphAllConfig dispGraph graphParms
      node nodeType nodeTypeParms arc arcType arcTypeParms,
    HasConfigValue Shape nodeTypeParms,

    Graph graph)
   => (GraphDisp.Graph dispGraph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms)
   -> Displayable graph
   -> IO GraphEditor
newGraphEditor
      (displaySort :: GraphDisp.Graph dispGraph graphParms
         node nodeType nodeTypeParms arc arcType arcTypeParms)
      (graph :: Displayable graph) =
   do
      registry <- newNodeArcTypeRegistry graph

      let
         (graphParms :: graphParms) =
            GraphTitle "Graph Editor" $$
            GlobalMenu (
               Menu (Just "New Types") [
                  Button "New Node Type" (makeNewNodeType graph registry),
                  Button "New Arc Type" (makeNewArcType graph registry)
                  ]
               )  $$
            GraphGesture (makeNewNode graph registry >> done) $$
            SurveyView True $$
            AllowDragging True $$
            GraphDisp.emptyGraphParms

         makeNodeTypeParms :: DisplayGraph -> NodeType
            -> NodeTypeAttributes Node -> IO (nodeTypeParms Node)
         makeNodeTypeParms _ nodeType nodeTypeAttributes =
            return (
               ValueTitle (\ node ->
                  do
                     nodeOwnTitle <- getNodeLabel graph node
                     return (
                        (nodeTypeTitle nodeTypeAttributes) ++ "." ++
                           nodeOwnTitle
                        )
                  ) $$$
               LocalMenu (
                  Button "Delete" (\ toDelete -> deleteNode graph toDelete)
                  ) $$$
               NodeGesture (\ source -> makeNewNodeArc graph registry source)
                                                                         $$$
               NodeDragAndDrop (\ sourceDyn target ->
                  do
                     let
                        Just source = fromDynamic sourceDyn
                     makeNewArc graph registry source target
                  ) $$$
               shape nodeTypeAttributes $$$
               GraphDisp.emptyNodeTypeParms
               )

         makeArcTypeParms _ arcType arcTypeAttributes =
            return
               (LocalMenu (
                  Button "Delete" (\ toDelete -> deleteArc graph toDelete)
                  ) $$$
                  GraphDisp.emptyArcTypeParms
                  )

      displayGraphInstance <-
         displayGraph displaySort graph graphParms
            makeNodeTypeParms makeArcTypeParms

      oID <- newObject
      let
         graphEditor = GraphEditor {
            oID = oID,
            destroyAction =
               do
                  destroyRegistry registry
                  destroy displayGraphInstance
               ,
            destroyedEvent = destroyed displayGraphInstance
            }

      registerTool graphEditor

      return graphEditor

-- -----------------------------------------------------------------------
-- GraphEditor
-- This type is only there to allow us to destroy it.
-- -----------------------------------------------------------------------

data GraphEditor = GraphEditor {
   oID :: ObjectID,
   destroyAction :: IO (), -- run this to end everything
   destroyedEvent :: Event ()
   }

instance Object GraphEditor where
   objectID graphEditor = oID graphEditor

instance Destroyable GraphEditor where
   destroy graphEditor = destroyAction graphEditor

instance Destructible GraphEditor where
   destroyed graphEditor = destroyedEvent graphEditor


-- -----------------------------------------------------------------------
-- Nodes
-- -----------------------------------------------------------------------

-- This action is used when the user requests a new type
makeNewNodeType :: Graph graph
   => Displayable graph
   -> NodeArcTypeRegistry
   -> IO ()
makeNewNodeType graph registry =
   do
      attributesOpt <- getNodeTypeAttributes
      case attributesOpt of
         Nothing -> done
         Just (attributes :: NodeTypeAttributes Node) ->
            do
               nodeType <- newNodeType graph attributes
               setValue (nodeTypes registry)
                  (nodeTypeTitle attributes) nodeType

-- This action is used to construct a new node.
-- (This is sometimes used as part of a node-and-edge construction)
makeNewNode :: Graph graph
   => Displayable graph
   -> NodeArcTypeRegistry
   -> IO (Maybe Node)
makeNewNode graph registry =
   do
      attributesOpt <- getNodeAttributes (nodeTypes registry)
      case attributesOpt of
         Nothing -> return Nothing
         Just attributes ->
            do
               node <- newNode graph (nodeType attributes)
                  (nodeTitle attributes)
               return (Just node)

deleteNode :: Graph graph
   => Displayable graph
   -> Node -> IO ()
deleteNode graph node = update graph (DeleteNode node)


-- -----------------------------------------------------------------------
-- Arcs
-- -----------------------------------------------------------------------

-- This action is used when the user requests a new type
makeNewArcType :: Graph graph
   => Displayable graph
   -> NodeArcTypeRegistry
   -> IO ()
makeNewArcType graph registry =
   do
      attributesOpt <- getArcTypeAttributes
      case attributesOpt of
         Nothing -> done
         Just (attributes :: ArcTypeAttributes) ->
            do
               arcType <- newArcType graph attributes
               setValue (arcTypes registry)
                  (arcTypeTitle attributes) arcType

-- This action makes a new arc between two nodes.
makeNewArc :: Graph graph
   => Displayable graph
   -> NodeArcTypeRegistry
   -> Node -> Node -> IO ()
makeNewArc graph registry source target =
   do
      attributesOpt <- getArcAttributes (arcTypes registry)
      case attributesOpt of
         Nothing -> done
         Just (attributes :: ArcAttributes ArcType) ->
            do
               newArc graph (arcType attributes) () source target
               done
-- This action makes a new node hanging from another one.
makeNewNodeArc :: Graph graph
   => Displayable graph
   -> NodeArcTypeRegistry
   -> Node -> IO ()
makeNewNodeArc graph registry source =
   do
      targetOpt <- makeNewNode graph registry
      case targetOpt of
         Nothing -> done
         Just target -> makeNewArc graph registry source target

deleteArc :: Graph graph
   => Displayable graph
   -> Arc -> IO ()
deleteArc graph arc = update graph (DeleteArc arc)

-- -----------------------------------------------------------------------
-- Maintaining the Registries of nodes and arc types.
-- (These are used for getting node and arc types when we query
-- the user about new nodes and arcs.)
-- -----------------------------------------------------------------------

type NodeTypeRegistry = Registry String NodeType

type ArcTypeRegistry = Registry String ArcType

data NodeArcTypeRegistry = NodeArcTypeRegistry {
   nodeTypes :: NodeTypeRegistry,
   arcTypes :: ArcTypeRegistry,
   destroyRegistry :: IO ()
   }

newNodeArcTypeRegistry :: Graph graph
   => Displayable graph
   -> IO NodeArcTypeRegistry
newNodeArcTypeRegistry graph =
   do
      (nodeTypes :: NodeTypeRegistry) <- newRegistry
      (arcTypes :: ArcTypeRegistry) <- newRegistry

      updateQueue <- newChannel
      GraphConnectionData {
         graphState = CannedGraph { updates = oldUpdates },
         deRegister = deRegister
         } <- shareGraph graph (sendIO updateQueue)

      let
         handleUpdate (NewNodeType nodeType attributes) =
            setValue nodeTypes (nodeTypeTitle attributes) nodeType
         handleUpdate (NewArcType arcType attributes) =
            setValue arcTypes (arcTypeTitle attributes) arcType
         handleUpdate (MultiUpdate updates) = mapM_ handleUpdate updates

         handleUpdate _ = done

         monitorThread =
            do
               update <- receiveIO updateQueue
               handleUpdate update
               monitorThread

      sequence_ (map handleUpdate oldUpdates)

      monitorThreadID <- forkIO monitorThread

      let
         destroyRegistry =
            do
               killThread monitorThreadID
               deRegister
               emptyRegistry nodeTypes
               emptyRegistry arcTypes
      return (NodeArcTypeRegistry {
         nodeTypes = nodeTypes,
         arcTypes = arcTypes,
         destroyRegistry = destroyRegistry
         })

