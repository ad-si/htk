{- Here we fit DaVinci into the GraphDisp framework.
   
   We do not give any configuration options to DaVinci 
   or HTk.  However, if this is desired, they can be
   set up by calling the htk and/or davinci functions 
   before, with the appropriate options.
   -}
module DaVinciGraphDisp(
   daVinciSort,
   DaVinciGraph,
   DaVinciGraphParms,
   DaVinciNode,
   DaVinciNodeType,
   DaVinciNodeTypeParms,
   DaVinciArc,
   DaVinciArcType,
   DaVinciArcTypeParms
   ) where

import IO

import FiniteMap

import qualified IOExts(unsafePerformIO)

import Computation
import Debug(debug)

import Concurrent
import Selective
import Dynamics
import Registry

import qualified DaVinci
import qualified HTk
import qualified SIM
import Event

import GraphDisp
import GraphConfigure


-- We follow the order of the GraphDisp file, mostly.

------------------------------------------------------------------------
-- How you refer to everything
------------------------------------------------------------------------

daVinciSort :: Graph DaVinciGraph 
   DaVinciGraphParms DaVinciNode DaVinciNodeType DaVinciNodeTypeParms
   DaVinciArc DaVinciArcType DaVinciArcTypeParms = displaySort

------------------------------------------------------------------------
-- Graphs
------------------------------------------------------------------------

data DaVinciGraph = 
   DaVinciGraph {
      graph :: DaVinci.Graph,
      daVinci :: DaVinci.DaVinci,

      -- maps from DaVinci nodes and edges to various things.
      nodeValues :: UntypedRegistry DaVinci.Node,
      edgeValues :: UntypedRegistry DaVinci.Edge,
      nodeTypes :: Registry DaVinci.Node DaVinciNodeTypePrim,
      dragAndDropper :: SIM.InterActor,
         -- interactor to handle dragging and dropping
      doImprove :: Bool
         -- improveAll on redrawing graph.
      }

instance SIM.Destructible DaVinciGraph where
   SIM.destroy (daVinciGraph@DaVinciGraph {graph = graph,
         nodeValues = nodeValues,dragAndDropper = dragAndDropper}) =
      do
         SIM.destroy graph
         destroyData daVinciGraph

   SIM.destroyed (daVinciGraph@DaVinciGraph {graph=graph,daVinci=daVinci}) =
         SIM.destroyed graph
      +> (SIM.destroyed daVinci >>> destroyData daVinciGraph)
     
 
destroyData :: DaVinciGraph -> IO ()
destroyData (DaVinciGraph {nodeValues = nodeValues,edgeValues = edgeValues,
      nodeTypes = nodeTypes,dragAndDropper = dragAndDropper}) =
   do
      SIM.destroy dragAndDropper
      emptyRegistry nodeValues
      emptyRegistry edgeValues
      emptyRegistry nodeTypes


data DaVinciGraphParms = DaVinciGraphParms {
   graphConfigs :: [Config DaVinci.Graph],
   graphConfigGesture :: IO (),
   surveyView :: Bool,
   configDoImprove :: Bool
   }

instance GraphClass DaVinciGraph where
   redrawPrim (DaVinciGraph{graph=graph,doImprove = doImprove}) = 
      do
         if doImprove
            then
               DaVinci.improveAll graph
            else
               done
         DaVinci.redrawGraph graph

instance NewGraph DaVinciGraph DaVinciGraphParms where
   newGraphPrim (DaVinciGraphParms {
         graphConfigs=graphConfigs,graphConfigGesture=graphGesture,
         configDoImprove=configDoImprove,surveyView=surveyView}) =
      do
         debug "ngP1"
         (daVinci :: DaVinci.DaVinci) <- DaVinci.davinci [] 
         debug "ngP2"
         graph <- DaVinci.newGraph ([
            DaVinci.gapwidth 4,
            DaVinci.gapheight 40
            ] ++ (reverse graphConfigs))
         debug "ngP3"

         DaVinci.displayGraph graph
         debug "ngP4"
         if surveyView
            then
               DaVinci.newSurveyView graph
            else
               done
         debug "ngP5"

         nodeValues <- newRegistry
         edgeValues <- newRegistry
         nodeTypes <- newRegistry

         -- Set up drag and dropper interactor
         dragAndDropper <- SIM.newInterActor (\iact ->
               DaVinci.createNodeGesture graph >>> graphGesture
            +> DaVinci.createChildGesture graph >>>=
                  (\ daVinciNode ->
                     do
                        nodeTypePrim <- getValue nodeTypes daVinciNode
                        dyn <- getValueAsDyn nodeValues daVinciNode
                        (onNodeGesture nodeTypePrim) dyn
                     )      
            +> DaVinci.createEdgeGesture graph >>>=
                  (\ (nodeFrom,nodeTo) ->
                     do
                        fromDyn <- getValueAsDyn nodeValues nodeFrom
                        toDyn <- getValueAsDyn nodeValues nodeTo
                        nodeTypePrim <- getValue nodeTypes nodeTo
                        (onNodeDragAndDrop nodeTypePrim) fromDyn toDyn 
                  )
            )   

         return (DaVinciGraph{
            graph = graph,
            daVinci = daVinci,
            nodeValues = nodeValues,
            edgeValues = edgeValues,
            nodeTypes = nodeTypes,
            dragAndDropper = dragAndDropper,
            doImprove = configDoImprove
            })

instance GraphParms DaVinciGraphParms where
   emptyGraphParms = DaVinciGraphParms {
      graphConfigs = [],graphConfigGesture = done,
      configDoImprove = False,surveyView = False
      }

instance HasConfig GraphTitle DaVinciGraphParms where
   configUsed _ _  = True
   ($$) (GraphTitle graphTitle) daVinciGraphParms =
      daVinciGraphParms {
         graphConfigs = HTk.text graphTitle : 
            (graphConfigs daVinciGraphParms)
            }

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

   ($$) (AllowDragging allowDragging) daVinciGraphParms =
      let
         onOrOff = if allowDragging then DaVinci.On else DaVinci.Off
      in
         daVinciGraphParms {
            graphConfigs = DaVinci.dragging onOrOff : 
            (graphConfigs daVinciGraphParms)
            }

instance GraphConfig graphConfig 
   => HasConfig graphConfig DaVinciGraphParms where

   configUsed graphConfig graphParms = False
   ($$) graphConfig graphParms = graphParms

------------------------------------------------------------------------
-- Nodes
------------------------------------------------------------------------

data DaVinciNode value = DaVinciNode DaVinci.Node

-- Tiresomely we need to make the "real" node type untyped.
-- This is so that the interactor which handles drag-and-drop
-- can get the type out without knowing what it is.
newtype DaVinciNodeType value = DaVinciNodeType DaVinciNodeTypePrim

data DaVinciNodeTypePrim = DaVinciNodeTypePrim {
   nodeType :: DaVinci.NodeType,
   nodeText :: Dyn -> IO String,
      -- how to compute the displayed name of the node
   onNodeGesture :: Dyn -> IO (),
      -- action on node gesture
   onNodeDragAndDrop :: Dyn -> Dyn -> IO ()
      -- action on node drag and drop
   }

data DaVinciNodeTypeParms value = 
   DaVinciNodeTypeParms {
      nodeTypeConfigs :: [DaVinciGraph -> Config DaVinci.NodeType],
         -- DaVinci config options,
      configNodeText :: (value -> IO String),
      configNodeGesture :: value -> IO (),
      configNodeDragAndDrop :: Dyn -> value -> IO ()
      }

instance NewNode DaVinciGraph DaVinciNode DaVinciNodeType where
   newNodePrim
           (DaVinciGraph {
              graph=graph,nodeValues=nodeValues,nodeTypes=nodeTypes}) 
           (DaVinciNodeType (nodeTypePrim @ DaVinciNodeTypePrim {
              nodeType = daVinciNodeType,nodeText = nodeText}))
            value =
      do
         let valueDyn = toDyn value
         thisNodeText <- nodeText valueDyn
         node <- DaVinci.newNode graph Nothing [
            DaVinci.nodetype daVinciNodeType]
         setValueAsDyn nodeValues node valueDyn
         setValue nodeTypes node nodeTypePrim
         configure node [
            HTk.text thisNodeText,
            DaVinci.border DaVinci.cdefault
            ]
         return (DaVinciNode node)
   getNodeTypePrim (DaVinciGraph {nodeTypes=nodeTypes}) (DaVinciNode node) = 
      do
         primType <- getValue nodeTypes node
         return (DaVinciNodeType primType)

instance DeleteNode DaVinciGraph DaVinciNode where
   deleteNodePrim (DaVinciGraph {nodeValues=nodeValues,nodeTypes=nodeTypes})
         (DaVinciNode node) = 
      do
         SIM.destroy node
         deleteFromRegistry nodeValues node
         deleteFromRegistry nodeTypes node

   getNodeValuePrim 
         (DaVinciGraph {nodeValues=nodeValues}) (DaVinciNode node) = 
      getValue nodeValues node

   setNodeValuePrim (DaVinciGraph {nodeValues=nodeValues,nodeTypes=nodeTypes})
         (DaVinciNode node) newValue =
      do
         let valueDyn = toDyn newValue
         setValueAsDyn nodeValues node valueDyn
         nodeType <- getValue nodeTypes node
         newTitle <- (nodeText nodeType) valueDyn
         HTk.text newTitle node
         done

instance NodeClass DaVinciNode where

daVinciNodeTyCon = mkTyCon "DaVinciGraphDisp" "DaVinciNode"

instance HasTyCon1 DaVinciNode where
   tyCon1 _ = daVinciNodeTyCon

instance NodeTypeClass DaVinciNodeType where

-- Although it isn't obligatory, we make DaVinciNodeType things
-- Typeable so that we can save them dynamically.
daVinciNodeTypeTyCon = mkTyCon "DaVinciGraphDisp" "DaVinciNodeType"

instance HasTyCon1 DaVinciNodeType where
   tyCon1 _ = daVinciNodeTypeTyCon

instance NewNodeType DaVinciGraph DaVinciNodeType DaVinciNodeTypeParms where
   newNodeTypePrim 
         (daVinciGraph@(DaVinciGraph{
            graph = graph,
            daVinci = daVinci
            })) 
         (DaVinciNodeTypeParms {
            nodeTypeConfigs = nodeTypeConfigs,
            configNodeText = configNodeText,
            configNodeGesture = configNodeGesture,
            configNodeDragAndDrop = configNodeDragAndDrop
            }) =
      do
         let
            configs = 
               map 
                  (\ mkConfig -> mkConfig daVinciGraph) 
                  nodeTypeConfigs
            nodeText dyn =
               do
                  let Just value = fromDyn dyn
                  configNodeText value
            onNodeGesture dyn =
               do
                  let Just value = fromDyn dyn
                  configNodeGesture value
            onNodeDragAndDrop dynFrom dynTo =
               do
                  let Just value = fromDyn dynTo
                  configNodeDragAndDrop dynFrom value
              
         nodeType <- DaVinci.newNodeType graph Nothing (reverse configs)
         return (DaVinciNodeType (DaVinciNodeTypePrim {
            nodeType = nodeType,
            nodeText = nodeText,
            onNodeGesture = onNodeGesture,
            onNodeDragAndDrop = onNodeDragAndDrop
            }))

instance NodeTypeParms DaVinciNodeTypeParms where
   emptyNodeTypeParms = DaVinciNodeTypeParms {
      nodeTypeConfigs = [],
      configNodeText = (\ value -> return ""),
      configNodeGesture = (\ value -> done),
      configNodeDragAndDrop = (\ dyn value -> done)
      }

instance NodeTypeConfig graphConfig 
   => HasConfigValue graphConfig DaVinciNodeTypeParms where

   configUsed' nodeTypeConfig nodeTypeParms = False
   ($$$) nodeTypeConfig nodeTypeParms = nodeTypeParms

------------------------------------------------------------------------
-- Node type configs for titles and shapes.
------------------------------------------------------------------------

instance HasConfigValue ValueTitle DaVinciNodeTypeParms where
   configUsed' _ _ = True
   ($$$) (ValueTitle nodeText) parms = 
      parms { configNodeText = nodeText }

instance HasConfigValue Shape DaVinciNodeTypeParms where
   configUsed' _ _ = True
   ($$$) shape parms =
      let
         oldConfigs = nodeTypeConfigs parms
         mkConfig :: String -> String -> (DaVinciGraph ->
            Config DaVinci.NodeType)
         mkConfig attributeName attributeValue _ nodeType =
            HTk.cset nodeType attributeName (HTk.RawData attributeValue)
         mkShape shapeName = mkConfig "_GO" shapeName

         newConfigs =
            case shape of
               Box -> (mkShape "box") : oldConfigs
               Circle -> (mkShape "circle") : oldConfigs
               Ellipse -> (mkShape "ellipse") : oldConfigs
               Rhombus -> (mkShape "rhombus") : oldConfigs
               Triangle -> (mkShape "triangle") : oldConfigs
               Icon filePath -> (mkShape "icon") :
                  (mkConfig "ICONFILE" filePath) : oldConfigs
      in
         parms {nodeTypeConfigs = newConfigs} 

------------------------------------------------------------------------
-- Arcs
-- These differ from nodes in that they don't have texts, but do
-- have endnodes.
------------------------------------------------------------------------

newtype DaVinciArc value nodeFromValue nodeToValue = DaVinciArc DaVinci.Edge

newtype DaVinciArcType value = DaVinciArcType DaVinci.EdgeType

data DaVinciArcTypeParms value = 
   DaVinciArcTypeParms {
      arcTypeConfigs :: [DaVinciGraph -> Config DaVinci.EdgeType]
         -- config option for arc type which if present configures a menu
         -- for this node type.  
      }


instance NewArc DaVinciGraph DaVinciNode DaVinciNode DaVinciArc DaVinciArcType
      where
   newArcPrim (DaVinciGraph {edgeValues=edgeValues}) (DaVinciArcType edgeType)
         value (DaVinciNode nodeFrom) (DaVinciNode nodeTo) =
      do
         edge <- DaVinci.newEdge Nothing nodeFrom nodeTo 
            [DaVinci.edgetype edgeType]
         setValue edgeValues edge value
         return (DaVinciArc edge)

instance GetFrom DaVinciGraph DaVinciNode DaVinciArc where
   getFromPrim _ (DaVinciArc edge) =
      do
         nodeFrom <- DaVinci.getSource edge
         return (DaVinciNode nodeFrom)

instance GetTo DaVinciGraph DaVinciNode DaVinciArc where
   getToPrim _ (DaVinciArc edge) =
      do
         nodeTo <- DaVinci.getTarget edge
         return (DaVinciNode nodeTo)

instance GetArcType DaVinciGraph DaVinciArc DaVinciArcType where
   getArcTypePrim _ (DaVinciArc edge) =
      do
         edgeType <- DaVinci.getEdgeType edge
         return (DaVinciArcType edgeType)

instance DeleteArc DaVinciGraph DaVinciArc where
   deleteArcPrim (DaVinciGraph {edgeValues=edgeValues}) (DaVinciArc edge) =
      do
         SIM.destroy edge
         deleteFromRegistry edgeValues edge

   getArcValuePrim (DaVinciGraph {edgeValues=edgeValues}) (DaVinciArc edge) = 
      getValue edgeValues edge

   setArcValuePrim (DaVinciGraph {edgeValues=edgeValues}) (DaVinciArc edge) 
      newValue = setValue edgeValues edge newValue

instance ArcClass DaVinciArc where

daVinciArcTyCon = mkTyCon "DaVinciGraphDisp" "DaVinciArc"

instance HasTyCon3 DaVinciArc where
   tyCon3 _ = daVinciArcTyCon

instance ArcTypeClass DaVinciArcType where

instance NewArcType DaVinciGraph DaVinciArcType DaVinciArcTypeParms where
   newArcTypePrim
         (daVinciGraph@(DaVinciGraph{
            graph = graph,
            daVinci = daVinci
            })) 
         (DaVinciArcTypeParms {
            arcTypeConfigs = arcTypeConfigs
            }) =
      do
         let
            configs = 
               map 
                  (\ mkConfig -> mkConfig daVinciGraph) 
                  arcTypeConfigs
         edgeType <- DaVinci.newEdgeType graph Nothing (reverse configs)

         return (DaVinciArcType edgeType)

instance ArcTypeParms DaVinciArcTypeParms where
   emptyArcTypeParms = DaVinciArcTypeParms {
      arcTypeConfigs = []
      }

instance ArcTypeConfig arcTypeConfig 
   => HasConfigValue arcTypeConfig DaVinciArcTypeParms where

   configUsed' arcTypeConfig arcTypeParms = False
   ($$$) arcTypeConfig arcTypeParms = arcTypeParms

------------------------------------------------------------------------
-- Menus
------------------------------------------------------------------------

instance HasConfig GlobalMenu DaVinciGraphParms where
   configUsed _ _ = True
   ($$) globalMenu daVinciGraphParms =
      daVinciGraphParms {
         graphConfigs = (DaVinci.configGraphMenu globalMenu) :
            (graphConfigs daVinciGraphParms)
         }

instance HasConfigValue LocalMenu DaVinciNodeTypeParms where
   configUsed' _ _ = True
   ($$$) localMenu daVinciNodeTypeParms =
      daVinciNodeTypeParms {
         nodeTypeConfigs =
            (\ daVinciGraph -> 
               DaVinci.configNodeTypeMenu 
                  (convertNodeButton localMenu daVinciGraph)
                  ) : (nodeTypeConfigs daVinciNodeTypeParms)
            }

instance HasConfigValue LocalMenu DaVinciArcTypeParms where
   configUsed' _ _ = True
   ($$$) localMenu daVinciArcTypeParms =
      daVinciArcTypeParms {
         arcTypeConfigs =
            (\ daVinciGraph ->
               DaVinci.configEdgeTypeMenu 
               (convertEdgeButton localMenu daVinciGraph)
               ) : (arcTypeConfigs daVinciArcTypeParms)
            }


convertNodeButton :: Typeable value => LocalMenu value -> DaVinciGraph 
   -> LocalMenu DaVinci.Node
convertNodeButton (LocalMenu menuPrim) 
      (DaVinciGraph {nodeValues=nodeValues}) =
   LocalMenu(
      mapMenuPrim
         (\ action ->
            let
               actionNode node =
                  do
                     value <- getValue nodeValues node
                     action value
            in
               actionNode
            )   
         menuPrim
         )

convertEdgeButton :: Typeable value => LocalMenu value -> DaVinciGraph 
   -> LocalMenu DaVinci.Edge
convertEdgeButton (LocalMenu menuPrim) 
      (DaVinciGraph {edgeValues=edgeValues}) =
   LocalMenu(
      mapMenuPrim
         (\ action ->
            let
               actionEdge edge =
                  do
                     value <- getValue edgeValues edge
                     action value
            in
               actionEdge
            )   
         menuPrim
      )

------------------------------------------------------------------------
-- Drag And Drop
------------------------------------------------------------------------

instance HasConfig GraphGesture DaVinciGraphParms where
   configUsed _ _ = True
   
   ($$) (GraphGesture action) graphParms =
      graphParms {graphConfigGesture = action}


instance HasConfigValue NodeGesture DaVinciNodeTypeParms where
   configUsed' _ _ = True

   ($$$) (NodeGesture onNodeGesture) nodeTypeParms =
      nodeTypeParms {configNodeGesture = onNodeGesture}

instance HasConfigValue NodeDragAndDrop DaVinciNodeTypeParms where
   configUsed' _ _ = True

   ($$$) (NodeDragAndDrop onNodeDragAndDrop) nodeTypeParms =
      nodeTypeParms {configNodeDragAndDrop = onNodeDragAndDrop}


