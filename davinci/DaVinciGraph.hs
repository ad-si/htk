{- This is the implementation of modules GraphDisp and GraphConfigure for 
   daVinci.   See those files for explanation of the names. 
   We encode, for example, the type parameter node as DaVinciNode,
   and so on for other type parameters, prefixing with "DaVinci" and
   capitalising the next letter.  But the only variable you should normally
   need from this module is daVinciSort.
   -}
module DaVinciGraph(
   daVinciSort, -- Magic type parameter indicating we want to use daVinci.

   DaVinciGraph,
   DaVinciGraphParms,

   DaVinciNode,
   DaVinciNodeType,
   DaVinciNodeTypeParms,

   DaVinciArc,
   DaVinciArcType,
   DaVinciArcTypeParms
   ) where

import Maybe

import IOExts
import Set
import FiniteMap
import Concurrent
import Source
import Sink

import Dynamics
import Registry
import Computation
import Debug(debug)
import Thread

import Channels
import Events
import Destructible
import Synchronized

import BSem

import MenuType

import GraphDisp
import GraphConfigure

import DaVinciTypes
import DaVinciBasic

------------------------------------------------------------------------
-- How you refer to everything
------------------------------------------------------------------------

daVinciSort :: Graph DaVinciGraph 
   DaVinciGraphParms DaVinciNode DaVinciNodeType DaVinciNodeTypeParms
   DaVinciArc DaVinciArcType DaVinciArcTypeParms = displaySort

instance GraphAllConfig DaVinciGraph DaVinciGraphParms 
   DaVinciNode DaVinciNodeType DaVinciNodeTypeParms
   DaVinciArc DaVinciArcType DaVinciArcTypeParms 

-- -----------------------------------------------------------------------
-- Graphs.
-- -----------------------------------------------------------------------

data DaVinciGraph = DaVinciGraph {
   context :: Context,

   -- For each node and edge we give (a) its type, (b) its value. 
   nodes :: Registry NodeId NodeData,
   edges :: Registry EdgeId ArcData,

   pendingChangesMVar :: MVar [MixedUpdate],
      -- This refers to changes to the structure of the graph
      -- which haven't yet been sent to daVinci.  Only some
      -- changes can be delayed in this way, namely
      -- node and edge additions and deletions.
      -- Changes to types are not delayed.  Changes to attribute values,
      -- EG setNodeValuePrim, cause this list to be flushed, as does
      -- redrawPrim.
   pendingChangesLock :: BSem,
      -- This lock is acquired during the flushPendingChanges action.

   globalMenuActions :: Registry MenuId (IO ()),
   otherActions :: Registry DaVinciAnswer (IO ()),
   -- The node and edge types contain other event handlers.

   lastSelectionRef :: IORef LastSelection,

   doImprove :: Bool,
   -- improveAll on redrawing graph.

   destructionChannel :: Channel (),

   destroyActions :: IO (),
   -- Various actions to be done when the graph is closed. 

   redrawChannel :: Channel Bool
   -- Sending True along this channel indicates that a 
   -- redraw is desired.
   -- Sending False along it ends the appropriate thread.
   }

data LastSelection = LastNone | LastNode NodeId | LastEdge EdgeId

data DaVinciGraphParms = DaVinciGraphParms {
   graphConfigs :: [DaVinciGraph -> IO ()], -- General setups
   surveyView :: Bool,
   configDoImprove :: Bool,
   graphTitleSource :: Maybe (Source GraphTitle)
   }

instance Destroyable DaVinciGraph where
   destroy (daVinciGraph @ DaVinciGraph {
         context = context,nodes = nodes,edges = edges,
         globalMenuActions = globalMenuActions,otherActions = otherActions,
         destroyActions = destroyActions,redrawChannel = redrawChannel}) =
      do
         sync (noWait (send redrawChannel False))
         destroyActions
         destroy context
         emptyRegistry nodes
         emptyRegistry edges
         emptyRegistry globalMenuActions
         emptyRegistry otherActions
         signalDestruct daVinciGraph

instance Destructible DaVinciGraph where
   destroyed (DaVinciGraph {destructionChannel = destructionChannel}) = 
      receive destructionChannel

signalDestruct :: DaVinciGraph -> IO ()
signalDestruct daVinciGraph = 
   sync(noWait(send (destructionChannel daVinciGraph) ()))

---
-- We run a separate thread for redrawing.  The idea is that when more than
-- one redraw request arrives while daVinci is already redrawing, we only
-- send one.  This means it is not too bad when we make a lot of changes,
-- redrawing each one.
redrawThread :: DaVinciGraph -> IO ()
redrawThread (daVinciGraph @ DaVinciGraph{
      context = context,doImprove = doImprove,redrawChannel = redrawChannel}) =
   do
      b1 <- sync (receive redrawChannel)
      bs <- getAllQueued (receive redrawChannel)
      if and (b1:bs)
         then
            do
               flushPendingChanges daVinciGraph
               if doImprove
                  then
                     doInContext (DaVinciTypes.Menu(Layout(ImproveAll))) 
                        context 
                  else
                     done
               redrawThread daVinciGraph
         else
            done

instance GraphClass DaVinciGraph where
   redrawPrim (daVinciGraph @ DaVinciGraph{
         redrawChannel = redrawChannel}) = 
      sync(noWait(send redrawChannel True))

instance NewGraph DaVinciGraph DaVinciGraphParms where
   newGraphPrim (DaVinciGraphParms {graphConfigs = graphConfigs,
         configDoImprove = configDoImprove,surveyView = surveyView,
         graphTitleSource = graphTitleSource}) =
      do
         nodes <- newRegistry
         edges <- newRegistry
         globalMenuActions <- newRegistry
         otherActions <- newRegistry
         lastSelectionRef <- newIORef LastNone
         
         let
            -- We now come to write the handler function for
            -- the context.  This is quite complex so we handle the
            -- various cases one by one, in separate functions.
            handler :: DaVinciAnswer -> IO ()
            -- In general, the rule is that if we don't find
            -- a handler function, we do nothing.  We do, however,
            -- assume that where a menuId is quoted, there is
            -- an associated handler.  If not, this is (probably)
            -- a bug in daVinci.
            handler daVinciAnswer = case daVinciAnswer of
               NodeSelectionsLabels nodes -> actNodeSelections nodes
               NodeDoubleClick -> nodeDoubleClick
               EdgeSelectionLabel edge -> actEdgeSelections edge
               EdgeDoubleClick -> edgeDoubleClick
               MenuSelection menuId -> actGlobalMenu menuId
               PopupSelectionNode nodeId menuId ->
                  actNodeMenu nodeId menuId
               PopupSelectionEdge edgeId menuId ->
                  actEdgeMenu edgeId menuId
               CreateNodeAndEdge nodeId -> actCreateNodeAndEdge nodeId
               CreateEdge nodeFrom nodeTo -> actCreateEdge nodeFrom nodeTo
               _ -> 
                  do
                     action <- getValueDefault done otherActions daVinciAnswer
                     action
            -- Update lastSelectionRef.  This contains the
            -- last selected node or edge, in case we are about to
            -- double-click or call a menu.
            actNodeSelections :: [NodeId] -> IO ()
            actNodeSelections [nodeId] = 
               writeIORef lastSelectionRef (LastNode nodeId)
            actNodeSelections _ = done -- not a double-click.

            actEdgeSelections :: EdgeId -> IO ()
            actEdgeSelections edgeId = 
               writeIORef lastSelectionRef (LastEdge edgeId)

            -- With node and edge double clicks we also expect the 
            -- node or edge to be recently selected and in lastSelectionRef
            nodeDoubleClick :: IO ()
            nodeDoubleClick =
               do
                  lastSelection <- readIORef lastSelectionRef
                  case lastSelection of
                     LastNode nodeId ->
                        do
                           NodeData nodeType nodeValue <- getValue nodes nodeId
                           (nodeDoubleClickAction nodeType) nodeValue
                     _ -> error "DaVinciGraph: confusing node double click"
  
            edgeDoubleClick :: IO ()
            edgeDoubleClick =
               do
                  lastSelection <- readIORef lastSelectionRef
                  case lastSelection of
                     LastEdge edgeId ->
                        do
                           ArcData arcType arcValue <- getValue edges edgeId
                           (arcDoubleClickAction arcType) arcValue
                     _ -> error "DaVinciGraph: confusing edge double click"

            actGlobalMenu :: MenuId -> IO ()
            actGlobalMenu menuId =
               do
                  action <- getValue globalMenuActions menuId
                  action

            actNodeMenu :: NodeId -> MenuId -> IO ()
            actNodeMenu nodeId menuId =
               do
                  NodeData nodeType nodeValue <- getValue nodes nodeId
                  menuAction <- getValue (nodeMenuActions nodeType) menuId
                  menuAction nodeValue

            actEdgeMenu :: EdgeId -> MenuId -> IO ()
            actEdgeMenu edgeId menuId =
               do
                  ArcData arcType arcValue <- getValue edges edgeId
                  menuAction <- getValue (arcMenuActions arcType) menuId
                  menuAction arcValue

            -- We now do the drag-and-drops.  There is no special
            -- handler for the create node action, since this is
            -- done by the otherActions handler. 
            actCreateNodeAndEdge nodeId =
               do
                  NodeData nodeType nodeValue <- getValue nodes nodeId
                  (createNodeAndEdgeAction nodeType) nodeValue

            actCreateEdge :: NodeId -> NodeId -> IO ()
            actCreateEdge nodeId1 nodeId2 =
               do
                  NodeData nodeType2 nodeValue2 <- getValue nodes nodeId2
                  NodeData _ nodeValue1 <- getValue nodes nodeId1
                  (createEdgeAction nodeType2) (toDyn nodeValue1) nodeValue2

         context <- newContext handler
         pendingChangesMVar <- newMVar []
         destructionChannel <- newChannel
         redrawChannel <- newChannel
         pendingChangesLock <- newBSem

         let
            setTitle :: GraphTitle -> IO ()
            setTitle (GraphTitle graphTitle) 
               = doInContext (Window (Title graphTitle)) context

         -- Sink for changing the title
         (addSink,destroySink) <-  
            case graphTitleSource of
               Nothing -> return (done,done)
               Just graphTitleSource ->
                  do
                     sink <- newSink setTitle
                     let
                        addSink =
                           do
                              currentTitle <- addOldSink graphTitleSource sink
                              setTitle currentTitle
                     return (addSink,invalidate sink)

         let
            daVinciGraph =
               DaVinciGraph {
                  context = context,
                  nodes = nodes,
                  edges = edges,
                  globalMenuActions = globalMenuActions,
                  otherActions = otherActions,
                  pendingChangesMVar = pendingChangesMVar,
                  pendingChangesLock = pendingChangesLock,
                  doImprove = configDoImprove,
                  lastSelectionRef = lastSelectionRef,
                  destructionChannel = destructionChannel,
                  destroyActions = destroySink,
                  redrawChannel = redrawChannel
                  }

         setValue otherActions Closed (signalDestruct daVinciGraph)
         setValue otherActions Quit (signalDestruct daVinciGraph)

         sequence_ (map ($ daVinciGraph) (reverse graphConfigs))

         addSink

         -- Do some initial commands.
         doInContext (DVSet(GapWidth 4)) context
         doInContext (DVSet(GapHeight 40)) context
         if surveyView
            then
               doInContext (DaVinciTypes.Menu(View(OpenSurveyView))) context
            else
               done

         -- We don't bother to explicitly destroy this thread, because
         -- when the 
         forkIOquiet "daVinci redraw thread" (redrawThread daVinciGraph)

         return daVinciGraph

instance GraphParms DaVinciGraphParms where
   emptyGraphParms = DaVinciGraphParms {
      graphConfigs = [],configDoImprove = False,surveyView = False,
      graphTitleSource = Nothing     
      }

addGraphConfigCmd :: DaVinciCmd -> DaVinciGraphParms -> DaVinciGraphParms
addGraphConfigCmd daVinciCmd daVinciGraphParms =
   daVinciGraphParms {
      graphConfigs = (\ daVinciGraph ->
         doInContext daVinciCmd (context daVinciGraph)) 
         : (graphConfigs daVinciGraphParms)
         }

instance HasConfig GraphTitle DaVinciGraphParms where
   configUsed _ _  = True
   ($$) (GraphTitle graphTitle) =
      addGraphConfigCmd (Window(Title graphTitle))

instance HasConfig (Source GraphTitle) DaVinciGraphParms where
   configUsed _ _  = True
   ($$) graphTitleSource graphParms 
      = graphParms {graphTitleSource = Just graphTitleSource}

instance HasConfig OptimiseLayout DaVinciGraphParms where
   configUsed _ _  = True
   ($$) (OptimiseLayout configDoImprove) daVinciGraphParms =
      daVinciGraphParms {configDoImprove = configDoImprove}

instance HasConfig SurveyView DaVinciGraphParms where
   configUsed _ _  = True
   ($$) (SurveyView surveyView) daVinciGraphParms =
      daVinciGraphParms {surveyView = surveyView}

instance HasConfig AllowDragging DaVinciGraphParms where
   configUsed _ _  = True

   ($$) (AllowDragging allowDragging) =
      addGraphConfigCmd (DragAndDrop 
         (if allowDragging then DraggingOn else DraggingOff))

instance HasConfig GlobalMenu DaVinciGraphParms where
   configUsed _ _ = True
   ($$) globalMenu graphParms =
      graphParms {
         graphConfigs =
            (\ daVinciGraph ->
               do
                  menuEntries <- encodeGlobalMenu globalMenu daVinciGraph
                  doInContext (AppMenu(CreateMenus menuEntries))
                     (context daVinciGraph)
                  doInContext (AppMenu(ActivateMenus(getMenuIds menuEntries)))
                     (context daVinciGraph)
               ) : (graphConfigs graphParms)
         }

instance HasConfig GraphGesture DaVinciGraphParms where
   configUsed _ _ = True
   ($$) (GraphGesture action) graphParms =
      graphParms {
         graphConfigs =
            (\ daVinciGraph ->
               setValue (otherActions daVinciGraph) CreateNode action
               ) : (graphConfigs graphParms)
         }
                  

instance GraphConfig graphConfig 
   => HasConfig graphConfig DaVinciGraphParms where

   configUsed graphConfig graphParms = False
   ($$) graphConfig graphParms = graphParms

-- -----------------------------------------------------------------------
-- Nodes
-- -----------------------------------------------------------------------

data DaVinciNode value = DaVinciNode NodeId

-- Tiresomely we need to make the "real" node type untyped.
-- This is so that the interactor which handles drag-and-drop
-- can get the type out without knowing what it is.
data DaVinciNodeType value = DaVinciNodeType {
   nodeType :: Type,
   nodeText :: value -> IO String,
      -- how to compute the displayed name of the node
   nodeMenuActions :: Registry MenuId (value -> IO ()),
   nodeDoubleClickAction :: value -> IO (),
   createNodeAndEdgeAction :: value -> IO (),
   createEdgeAction :: Dyn -> value -> IO () 
   }

data NodeData = forall value . Typeable value => 
   NodeData (DaVinciNodeType value) value

data DaVinciNodeTypeParms value = 
   DaVinciNodeTypeParms {
      nodeAttributes :: Attributes value,
      configNodeText :: value -> IO String,
      configNodeDoubleClickAction :: value -> IO (),
      configCreateNodeAndEdgeAction :: value -> IO (),
      configCreateEdgeAction :: Dyn -> value -> IO ()
      }

instance Eq1 DaVinciNode where
   eq1 (DaVinciNode n1) (DaVinciNode n2) = (n1 == n2)

instance Ord1 DaVinciNode where
   compare1 (DaVinciNode n1) (DaVinciNode n2) = compare n1 n2 

instance NewNode DaVinciGraph DaVinciNode DaVinciNodeType where
   newNodePrim
         (daVinciGraph @ DaVinciGraph {context=context,nodes=nodes}) 
         (nodeType @ DaVinciNodeType {
            nodeType = daVinciNodeType,nodeText = nodeText})
         value =
      do
         thisNodeText <- nodeText value
         nodeId <- newNodeId context
         setValue nodes nodeId (NodeData nodeType value)
         addNodeUpdate daVinciGraph (
            NewNode nodeId daVinciNodeType [A "OBJECT" thisNodeText])
         return (DaVinciNode nodeId)

instance DeleteNode DaVinciGraph DaVinciNode where
   deleteNodePrim (daVinciGraph @ 
            DaVinciGraph {context = context,nodes = nodes})
         (DaVinciNode nodeId) = 
      do
         addNodeUpdate daVinciGraph (DeleteNode nodeId) 

   getNodeValuePrim (daVinciGraph @ DaVinciGraph {
         context = context,nodes = nodes}) (DaVinciNode nodeId) =
      do
         (Just (NodeData _ nodeValue)) <- getValueOpt nodes nodeId
         return (coDyn nodeValue)

   setNodeValuePrim (daVinciGraph @ DaVinciGraph {
         context = context,nodes = nodes}) (DaVinciNode nodeId) newValue =
      do
         flushPendingChanges daVinciGraph
         newTitle <- transformValue nodes nodeId
            (\ (Just (NodeData nodeType _)) -> 
               do
                  let newValue' = coDyn newValue
                  -- newValue' is actually the same as newValue but we
                  -- need this to make the types work.
                  newTitle <- (nodeText nodeType) newValue'
                  return (Just (NodeData nodeType newValue'),newTitle)
               )
         doInContext (DaVinciTypes.Graph(
               ChangeAttr[Node nodeId [A "OBJECT" newTitle]]))
            context

instance NodeClass DaVinciNode where

daVinciNodeTyRep = mkTyRep "DaVinciGraph" "DaVinciNode"

instance HasTyRep1 DaVinciNode where
   tyRep1 _ = daVinciNodeTyRep

instance NodeTypeClass DaVinciNodeType where

daVinciNodeTypeTyRep = mkTyRep "DaVinciGraphDisp" "DaVinciNodeType"

instance HasTyRep1 DaVinciNodeType where
   tyRep1 _ = daVinciNodeTypeTyRep

instance NewNodeType DaVinciGraph DaVinciNodeType DaVinciNodeTypeParms where
   newNodeTypePrim 
         (daVinciGraph@(DaVinciGraph {context = context}))
         (DaVinciNodeTypeParms {
            nodeAttributes = nodeAttributes,
            configNodeText = configNodeText,
            configNodeDoubleClickAction = configNodeDoubleClickAction,
            configCreateNodeAndEdgeAction 
               = configCreateNodeAndEdgeAction,
            configCreateEdgeAction = configCreateEdgeAction
            }) =
      do
         nodeType <- newType context
         (nodeMenuActions,daVinciAttributes) <-
            encodeAttributes nodeAttributes daVinciGraph
         doInContext (Visual(AddRules [NR nodeType daVinciAttributes])) context

         let
            nodeText = configNodeText
            nodeDoubleClickAction = configNodeDoubleClickAction
            createNodeAndEdgeAction = configCreateNodeAndEdgeAction
            createEdgeAction = configCreateEdgeAction
         return (DaVinciNodeType {
            nodeType = nodeType,
            nodeText = nodeText,
            nodeMenuActions = nodeMenuActions,
            nodeDoubleClickAction = nodeDoubleClickAction,
            createNodeAndEdgeAction = createNodeAndEdgeAction,
            createEdgeAction = createEdgeAction
            })

instance NodeTypeParms DaVinciNodeTypeParms where
   emptyNodeTypeParms = DaVinciNodeTypeParms {
      nodeAttributes = emptyAttributes,
      configNodeText = const (return ""),
      configNodeDoubleClickAction = const done,
      configCreateNodeAndEdgeAction = const done,
      configCreateEdgeAction = const (const done)
      }

   coMapNodeTypeParms coMapFn 
      (DaVinciNodeTypeParms {
         nodeAttributes = nodeAttributes,
         configNodeText = configNodeText,
         configNodeDoubleClickAction = configNodeDoubleClickAction,
         configCreateNodeAndEdgeAction = configCreateNodeAndEdgeAction,
         configCreateEdgeAction = configCreateEdgeAction
         }) =
      DaVinciNodeTypeParms {
         nodeAttributes = coMapAttributes coMapFn nodeAttributes,
         configNodeText = configNodeText . coMapFn,
         configNodeDoubleClickAction = configNodeDoubleClickAction . coMapFn,
         configCreateNodeAndEdgeAction = 
            configCreateNodeAndEdgeAction . coMapFn,
         configCreateEdgeAction = (\ dyn ->
            (configCreateEdgeAction dyn) . coMapFn)
         }

instance NodeTypeConfig graphConfig 
   => HasConfigValue graphConfig DaVinciNodeTypeParms where

   configUsed' nodeTypeConfig nodeTypeParms = False
   ($$$) nodeTypeConfig nodeTypeParms = nodeTypeParms

------------------------------------------------------------------------
-- Node type configs for titles, shapes, and so on.
------------------------------------------------------------------------

instance HasConfigValue ValueTitle DaVinciNodeTypeParms where
   configUsed' _ _ = True
   ($$$) (ValueTitle nodeText) parms = 
      parms { configNodeText = nodeText }

instance HasConfigValue Shape DaVinciNodeTypeParms where
   configUsed' _ _ = True
   ($$$) shape parms =
      let
         nodeAttributes0 = nodeAttributes parms
         shaped shape = Att "_GO" shape $$$ nodeAttributes0
         nodeAttributes1 =
            case shape of
               Box -> shaped "box"
               Circle -> shaped "circle"
               Ellipse -> shaped "ellipse"
               Rhombus -> shaped "rhombus"
               Triangle -> shaped "triangle"
               Icon filePath ->
                  Att "ICONFILE" filePath $$$
                  shaped "icon"
      in 
         parms {nodeAttributes = nodeAttributes1}

instance HasConfigValue Color DaVinciNodeTypeParms where
   configUsed' _ _ = True
   ($$$) (Color colorName) parms =
      parms {nodeAttributes = (Att "COLOR" colorName) $$$ 
         (nodeAttributes parms)}

instance HasConfigValue LocalMenu DaVinciNodeTypeParms where
   configUsed' _ _ = True
   ($$$) localMenu parms =
      parms {nodeAttributes = localMenu $$$ (nodeAttributes parms)}

instance HasConfigValue DoubleClickAction DaVinciNodeTypeParms where
   configUsed' _ _ = True
   ($$$) (DoubleClickAction action) parms =
      parms {configNodeDoubleClickAction = action}

------------------------------------------------------------------------
-- Instances of HasModifyValue
------------------------------------------------------------------------

instance HasModifyValue NodeArcsHidden DaVinciGraph DaVinciNode where
   modify (NodeArcsHidden hide) daVinciGraph (DaVinciNode nodeId) =
      do
         flushPendingChanges daVinciGraph
         doInContext (DaVinciTypes.Menu (
            Abstraction ((if hide then HideEdges else ShowEdges) [nodeId])
            )) (context daVinciGraph)

instance HasModifyValue (String,String) DaVinciGraph DaVinciNode where
   modify (key,value) daVinciGraph (DaVinciNode nodeId) =
      do
         flushPendingChanges daVinciGraph
         doInContext
            (DaVinciTypes.Graph (ChangeAttr [Node nodeId [A key value]]))
            (context daVinciGraph)

instance HasModifyValue Border DaVinciGraph DaVinciNode where
   modify border =
      let
         borderStr = case border of
            NoBorder -> "none"
            SingleBorder -> "single"
            DoubleBorder -> "double"
      in
         modify ("BORDER",borderStr)

------------------------------------------------------------------------
-- Node type configs for drag and drop
------------------------------------------------------------------------

instance HasConfigValue NodeGesture DaVinciNodeTypeParms where
   configUsed' _ _ = True
   ($$$) (NodeGesture actFn) nodeTypeParms =
      nodeTypeParms {configCreateNodeAndEdgeAction = actFn}

instance HasConfigValue NodeDragAndDrop DaVinciNodeTypeParms where
   configUsed' _ _ = True
   ($$$) (NodeDragAndDrop actFn) nodeTypeParms =
      nodeTypeParms {configCreateEdgeAction = actFn}

-- -----------------------------------------------------------------------
-- Arcs
-- -----------------------------------------------------------------------

data DaVinciArc value = DaVinciArc EdgeId

-- Like nodes, the "real" type is monomorphic.
data DaVinciArcType value = DaVinciArcType {
   arcType :: Type,
   arcMenuActions :: Registry MenuId (value -> IO ()),
   arcDoubleClickAction :: value -> IO ()
   }

data DaVinciArcTypeParms value = DaVinciArcTypeParms {
   arcAttributes :: Attributes value,
   configArcDoubleClickAction :: value -> IO ()
   }

data ArcData = forall value . Typeable value 
   => ArcData (DaVinciArcType value) value


instance Eq1 DaVinciArc where
   eq1 (DaVinciArc n1) (DaVinciArc n2) = (n1 == n2)

instance Ord1 DaVinciArc where
   compare1 (DaVinciArc n1) (DaVinciArc n2) = compare n1 n2 


instance NewArc DaVinciGraph DaVinciNode DaVinciNode DaVinciArc DaVinciArcType
      where
   newArcPrim 
         (daVinciGraph @ DaVinciGraph {context = context,edges = edges}) 
         daVinciArcType
         value (DaVinciNode nodeFrom) (DaVinciNode nodeTo) =
      do
         edgeId <- newEdgeId context
         setValue edges edgeId (ArcData daVinciArcType value)
         addEdgeUpdate daVinciGraph 
            (NewEdge edgeId (arcType daVinciArcType) [] nodeFrom nodeTo)
         return (DaVinciArc edgeId)

instance DeleteArc DaVinciGraph DaVinciArc where
   deleteArcPrim (daVinciGraph @ DaVinciGraph {edges=edges,context = context})
         (DaVinciArc edgeId) =
      do
         addEdgeUpdate daVinciGraph (DeleteEdge edgeId)
         deleteFromRegistry edges edgeId

   getArcValuePrim (daVinciGraph @ DaVinciGraph {
         context = context,edges = edges}) (DaVinciArc edgeId) =
      do
         (Just (ArcData _ arcValue)) <- getValueOpt edges edgeId
         return (coDyn arcValue)

   setArcValuePrim (daVinciGraph @ DaVinciGraph {
         context = context,edges = edges}) (DaVinciArc edgeId) newValue =
      do
         flushPendingChanges daVinciGraph
         transformValue edges edgeId
            (\ (Just (ArcData edgeType _)) ->
               return (Just (ArcData edgeType (coDyn newValue)),()))

instance ArcClass DaVinciArc where

daVinciArcTyRep = mkTyRep "DaVinciGraphDisp" "DaVinciArc"

instance HasTyRep1 DaVinciArc where
   tyRep1 _ = daVinciArcTyRep

daVinciArcTypeTyRep = mkTyRep "DaVinciGraphDisp" "DaVinciArcType"

instance HasTyRep1 DaVinciArcType where
   tyRep1 _ = daVinciArcTypeTyRep

instance ArcTypeClass DaVinciArcType where

instance NewArcType DaVinciGraph DaVinciArcType DaVinciArcTypeParms where
   newArcTypePrim
         (daVinciGraph@DaVinciGraph{context = context})
         (DaVinciArcTypeParms{arcAttributes = arcAttributes,
            configArcDoubleClickAction = configArcDoubleClickAction
            }) =
      do
         arcType <- newType context
         (arcMenuActions,attributes) 
            <- encodeAttributes arcAttributes daVinciGraph
         doInContext (Visual(AddRules [ER arcType attributes])) context 
         let
            arcDoubleClickAction = configArcDoubleClickAction
         return (DaVinciArcType {
            arcType = arcType,
            arcMenuActions = arcMenuActions,
            arcDoubleClickAction = arcDoubleClickAction
            })

instance ArcTypeParms DaVinciArcTypeParms where
   emptyArcTypeParms = DaVinciArcTypeParms {
      arcAttributes = emptyAttributes,
      configArcDoubleClickAction = const done
      }

   coMapArcTypeParms coMapFn
      (DaVinciArcTypeParms {
         arcAttributes = arcAttributes,
         configArcDoubleClickAction = configArcDoubleClickAction
         }) =
      (DaVinciArcTypeParms {
         arcAttributes = coMapAttributes coMapFn arcAttributes,
         configArcDoubleClickAction = configArcDoubleClickAction . coMapFn
         })

instance HasConfigValue Color DaVinciArcTypeParms where
   configUsed' _ _ = True
   ($$$) (Color colorName) parms =
      parms {arcAttributes = (Att "EDGECOLOR" colorName) $$$ 
         (arcAttributes parms)}

instance HasConfigValue EdgePattern DaVinciArcTypeParms where
   configUsed' _ _ = True
   ($$$) edgePattern parms =
      let
         pattern = case edgePattern of
            Solid -> "solid"
            Dotted -> "dotted"
            Dashed -> "dashed"
            Thick -> "thick"
            Double -> "double"
      in
         parms {arcAttributes = (Att "EDGEPATTERN" pattern) $$$ 
            (arcAttributes parms)}

instance HasConfigValue LocalMenu DaVinciArcTypeParms where
   configUsed' _ _ = True
   ($$$) localMenu parms =
      parms {arcAttributes = localMenu $$$ (arcAttributes parms)}

instance ArcTypeConfig arcTypeConfig 
   => HasConfigValue arcTypeConfig DaVinciArcTypeParms where

   configUsed' arcTypeConfig arcTypeParms = False
   ($$$) arcTypeConfig arcTypeParms = arcTypeParms


instance HasConfigValue DoubleClickAction DaVinciArcTypeParms where
   configUsed' _ _ = True
   ($$$) (DoubleClickAction action) parms =
      parms {configArcDoubleClickAction = action}

------------------------------------------------------------------------
-- Attributes in general
-- The Attributes type encodes the attributes in a DaVinciNodeTypeParms
-- or a DaVinciArcTypeParms
------------------------------------------------------------------------

data Attributes value = Attributes {
   options :: FiniteMap String String,
   menuOpt :: Maybe (LocalMenu value)
   }

emptyAttributes :: Attributes value
emptyAttributes = Attributes {
   options = emptyFM,
   menuOpt = Nothing
   }

coMapAttributes :: (value2 -> value1) -> Attributes value1 
   -> Attributes value2
coMapAttributes coMapFn (Attributes{options = options,menuOpt = menuOpt0}) =
   let
      menuOpt1 =
         fmap -- deals with Maybe
            (\ (LocalMenu menu0) ->
               (LocalMenu (mapMenuPrim (. coMapFn) menu0))
               )
            menuOpt0
   in
      Attributes{options = options,menuOpt = menuOpt1}
                                


data Att value = Att String String
-- An attribute

instance HasConfigValue Att Attributes where
   configUsed' _ _ = True
   ($$$) (Att key value) attributes =
      attributes {
         options = addToFM (options attributes) key value
         }

instance HasConfigValue LocalMenu Attributes where
   configUsed' _ _ = True
   ($$$) localMenu attributes =
      attributes {menuOpt = Just localMenu}

encodeAttributes :: Typeable value => Attributes value -> DaVinciGraph
   -> IO (Registry MenuId (value -> IO ()),[Attribute])
encodeAttributes attributes daVinciGraph =
   do
      let
         keysPart =
            map
               (\ (key,value) -> A key value)
               (fmToList (options attributes))
      case menuOpt attributes of
         Nothing -> return (
            error "MenuId returned by daVinci for object with no menu!",
            keysPart)
         Just localMenu ->
            do
               (registry,menuEntries) <- 
                  encodeLocalMenu localMenu daVinciGraph         
               return (registry,M menuEntries : keysPart)

------------------------------------------------------------------------
-- Menus
------------------------------------------------------------------------

encodeLocalMenu :: Typeable value => LocalMenu value -> DaVinciGraph 
   -> IO (Registry MenuId (value -> IO ()),[MenuEntry])
-- Construct a local menu associated with a particular type,
-- returning (a) a registry mapping MenuId's to actions;
-- (b) the [MenuEntry] to be passed to daVinci.
encodeLocalMenu 
      (LocalMenu (menuPrim0 :: MenuPrim (Maybe String) (value -> IO ()))) 
      (DaVinciGraph {context = context}) =
   do
      registry <- newRegistry
      (menuPrim1 :: MenuPrim (Maybe String) MenuId) <-
         mapMMenuPrim
            (\ valueToAct ->
               do
                  menuId <- newMenuId context
                  setValue registry menuId valueToAct
                  return menuId
               )    
            menuPrim0   
      (menuPrim2 :: MenuPrim (Maybe String,MenuId) MenuId) <-
         mapMMenuPrim'
            (\ stringOpt ->
               do
                  menuId <- newMenuId context
                  return (stringOpt,menuId)
               )
            menuPrim1
      return (registry,encodeDaVinciMenu menuPrim2)

getMenuIds :: [MenuEntry] -> [MenuId]
getMenuIds [] = []
getMenuIds (first:rest) = theseIds ++ getMenuIds rest
   where
      theseIds :: [MenuId]
      theseIds = case first of
         MenuEntry menuId _ -> [menuId]
         MenuEntryMne menuId _ _ _ _ -> [menuId]
         SubmenuEntry menuId _ menuEntries -> menuId : getMenuIds menuEntries
         SubmenuEntryMne menuId _ menuEntries _ -> menuId : getMenuIds menuEntries
         BlankMenuEntry -> []
         _ -> error "DaVinciGraph: (Sub)MenuEntryDisabled not yet handled." 

encodeGlobalMenu :: GlobalMenu -> DaVinciGraph -> IO [MenuEntry]
-- This constructs a global menu.  The menuId actions are written
-- directly into the graphs globalMenuActions registry. 
encodeGlobalMenu (GlobalMenu (menuPrim0 :: MenuPrim (Maybe String) (IO ())))
      (DaVinciGraph {context = context,globalMenuActions = globalMenuActions})
       =
   do
      (menuPrim1 :: MenuPrim (Maybe String) MenuId) <-
         mapMMenuPrim
            (\ action ->
               do
                  menuId <- newMenuId context
                  setValue globalMenuActions menuId action
                  return menuId
               )
            menuPrim0
      (menuPrim2 :: MenuPrim (Maybe String,MenuId) MenuId) <-
         mapMMenuPrim'
            (\ stringOpt ->
               do
                  menuId <- newMenuId context
                  return (stringOpt,menuId)
               )
            menuPrim1
      return (encodeDaVinciMenu menuPrim2)

encodeDaVinciMenu :: MenuPrim (Maybe String,MenuId) MenuId -> [MenuEntry]
-- Used for encoding all menus.  The MenuId in the first argument is
-- used as all submenus need to have a unique menuId, even though
-- daVinci can't send that as an event.
encodeDaVinciMenu menuHead =
   case menuHead of
      GraphConfigure.Menu (Nothing,_) menuPrims -> 
         encodeMenuList menuPrims
      GraphConfigure.Menu (Just label,menuId) menuPrims ->
         [SubmenuEntry menuId (MenuLabel label) (encodeMenuList menuPrims)]
      single -> [encodeMenuItem single]

   where
      encodeMenuList :: [MenuPrim (Maybe String,MenuId) MenuId] -> [MenuEntry]
      encodeMenuList menuPrims = map encodeMenuItem menuPrims

      encodeMenuItem :: MenuPrim (Maybe String,MenuId) MenuId  -> MenuEntry
      encodeMenuItem (Button label menuId) = MenuEntry menuId (MenuLabel label)
      encodeMenuItem (GraphConfigure.Menu (labelOpt,menuId) menuItems) =
         SubmenuEntry menuId (MenuLabel (fromMaybe "" labelOpt)) 
            (encodeMenuList menuItems)
      encodeMenuItem Blank = BlankMenuEntry 
      
-- -----------------------------------------------------------------------
-- Handling pending changes
-- -----------------------------------------------------------------------

addNodeUpdate :: DaVinciGraph -> NodeUpdate -> IO ()
addNodeUpdate (DaVinciGraph {pendingChangesMVar = pendingChangesMVar}) 
      nodeUpdate =
   do
      pendingChanges <- takeMVar pendingChangesMVar
      putMVar pendingChangesMVar (NU nodeUpdate : pendingChanges)


addEdgeUpdate :: DaVinciGraph -> EdgeUpdate -> IO ()
addEdgeUpdate (DaVinciGraph {pendingChangesMVar = pendingChangesMVar})
      edgeUpdate =
   do
      pendingChanges <- takeMVar pendingChangesMVar
      putMVar pendingChangesMVar (EU edgeUpdate : pendingChanges)

sortPendingChanges :: [MixedUpdate] -> DaVinciCmd
-- This is tricky because for daVinci 2.1 mixed updates don't work properly,
-- so we need to feed the updates as a list of node updates followed by
-- a list of edge updates.
sortPendingChanges pendingChanges =
   if isJust daVinciVersion
      then
         -- daVinci has version at least 3.0, and so multi_update works. 
         DaVinciTypes.Graph(UpdateMixed (reverse pendingChanges))
      else
         let
            (newNodes,deleteNodes,newEdges,deleteEdges) =
               foldl
                  (\ (nnSF,dnSF,neSF,deSF) change -> 
                     case change of
                        NU(nn @ (NewNode _ _ _)) -> (nn:nnSF,dnSF,neSF,deSF)
                        NU(dn @ (DeleteNode _)) -> (nnSF,dn:dnSF,neSF,deSF)
                        EU(ne @ (NewEdge _ _ _ _ _)) ->
                           (nnSF,dnSF,ne:neSF,deSF)
                        EU(de @ (DeleteEdge _)) -> (nnSF,dnSF,neSF,de:deSF)
                     )
                  ([],[],[],[])
                  pendingChanges
         -- The changes will be given in order
         -- newNodes ++ deleteNodes ++ newEdges ++ deleteEdges
         -- except that we need to filter newEdges to eliminate edges
         -- containing deleted nodes, and deleteEdges to eliminate
         -- edges filtered.
            (newEdges2,deleteEdges2) =
               if (null deleteNodes) || (null newEdges)
                  then
                     (newEdges,deleteEdges)
                  else -- edges involving deleted nodes must be excised
                     let
                        deletedNodes = 
                           mkSet(map (\ (DeleteNode nodeId) -> nodeId) 
                              deleteNodes)
                        (newEdges2,obsoleteEdges) =
                           foldl
                              (\ (nE2sF,oEsF) (ne @ 
                                    (NewEdge edgeId _ _ nodeFrom nodeTo)) -> 
                                 if (elementOf nodeFrom deletedNodes) ||
                                    (elementOf nodeTo deletedNodes)
                                    then (nE2sF,addToSet oEsF edgeId)
                                    else (ne:nE2sF,oEsF)
                                 )
                              ([],emptySet)
                              newEdges
                        deleteEdges2 = filter (\ (DeleteEdge edgeId) -> 
                           not (elementOf edgeId obsoleteEdges)) deleteEdges 
                     in
                        (newEdges2,deleteEdges2)
         in
            DaVinciTypes.Graph(Update (newNodes ++ deleteNodes)
                     (newEdges2 ++ deleteEdges2))

flushPendingChanges :: DaVinciGraph -> IO ()
flushPendingChanges (DaVinciGraph {context = context,nodes = nodes,
      edges = edges,pendingChangesMVar = pendingChangesMVar,
      pendingChangesLock = pendingChangesLock}) =
   synchronize pendingChangesLock (
      do
         pendingChanges <- takeMVar pendingChangesMVar
         putMVar pendingChangesMVar []
         case pendingChanges of
            [] -> done
            _ -> 
               doInContext (sortPendingChanges pendingChanges) context
         -- Delete all now-irrelevant node and edge entries.
         sequence_ (map
            (\ pendingChange -> case pendingChange of
               NU (DeleteNode nodeId) -> deleteFromRegistry nodes nodeId
               EU (DeleteEdge edgeId) -> deleteFromRegistry edges edgeId
               _ -> done
               )
            pendingChanges
            )
      )
-- -----------------------------------------------------------------------
-- Miscellaneous functions
-- -----------------------------------------------------------------------

-- Transforming one type to another when we know they are
-- actually identical . . .
coDyn :: (Typeable a,Typeable b) => a -> b
coDyn valueA =
   let
      dyn = toDyn valueA
   in
      case fromDyn dyn of
         Just valueB -> valueB

      
