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

import GraphDisp


-- We follow the order of the GraphDisp file, mostly.

------------------------------------------------------------------------
-- How you refer to everything
------------------------------------------------------------------------

daVinciSort :: (DaVinciGraph,DaVinciGraphParms,
   DaVinciNode a,DaVinciNodeType b,DaVinciNodeTypeParms c,
   DaVinciArc p d e,DaVinciArcType q,DaVinciArcTypeParms r) = displaySort

------------------------------------------------------------------------
-- Graphs
------------------------------------------------------------------------

data DaVinciGraph = 
   DaVinciGraph {
      graph :: DaVinci.Graph,
      daVinci :: DaVinci.DaVinci,
      nodeValues :: UntypedRegistry DaVinci.Node,
      edgeValues :: UntypedRegistry DaVinci.Edge,
      nodeTypes :: UntypedRegistry DaVinci.Node
      }

instance SIM.Destructible DaVinciGraph where
   SIM.destroy (DaVinciGraph {graph=graph}) = SIM.destroy graph
   SIM.destroyed (DaVinciGraph {graph=graph,daVinci=daVinci} ) = (
         SIM.destroyed graph 
      +> DaVinci.lastGraphClosed daVinci
      +> SIM.destroyed daVinci
      )


newtype DaVinciGraphParms = DaVinciGraphParms {
   graphConfigs :: [Config DaVinci.Graph]
   }

instance Graph DaVinciGraph where
   redraw (DaVinciGraph{graph=graph}) = DaVinci.redrawGraph graph

instance NewGraph DaVinciGraph DaVinciGraphParms where
   newGraph (DaVinciGraphParms {graphConfigs=graphConfigs}) =
      do
         (daVinci :: DaVinci.DaVinci) <- DaVinci.davinci []
         graph <- DaVinci.newGraph ([
            DaVinci.gapwidth 4,
            DaVinci.gapheight 40,
            DaVinci.dragging DaVinci.On
            ] ++ (reverse graphConfigs))

         DaVinci.displayGraph graph
         DaVinci.newSurveyView graph

         nodeValues <- newRegistry
         edgeValues <- newRegistry
         nodeTypes <- newRegistry

         return (DaVinciGraph{
            graph = graph,
            daVinci = daVinci,
            nodeValues = nodeValues,
            edgeValues = edgeValues,
            nodeTypes = nodeTypes
            })

instance GraphParms DaVinciGraphParms where
   emptyGraphParms = DaVinciGraphParms {graphConfigs = []}

instance GraphConfigParms GraphTitle DaVinciGraphParms where
   graphConfigUsed _ _  = True
   graphConfig (GraphTitle graphTitle) daVinciGraphParms =
      daVinciGraphParms {
         graphConfigs = HTk.text graphTitle : 
            (graphConfigs daVinciGraphParms)
            }

instance GraphConfig graphConfig 
   => GraphConfigParms graphConfig DaVinciGraphParms where

   graphConfigUsed graphConfig graphParms = False
   graphConfig graphConfig graphParms = graphParms

------------------------------------------------------------------------
-- Nodes
------------------------------------------------------------------------

data DaVinciNode value = DaVinciNode DaVinci.Node

data DaVinciNodeType value = 
   DaVinciNodeType DaVinci.NodeType (value -> IO String)
   -- the second argument gives the displayed name of the node.

data DaVinciNodeTypeParms value = 
   DaVinciNodeTypeParms {
      nodeText :: (value -> IO String),
      nodeTypeConfigs :: [DaVinciGraph -> Config DaVinci.NodeType]
         -- config options.
      }

instance NewNode DaVinciGraph DaVinciNode DaVinciNodeType where
   newNode daVinciNodeType@(DaVinciNodeType nodeType getNodeTitle) 
         (DaVinciGraph{
            graph=graph,nodeValues=nodeValues,nodeTypes=nodeTypes}) 
            value =
      do
         nodeText <- getNodeTitle value
         node <- DaVinci.newNode graph Nothing [DaVinci.nodetype nodeType]
         setValue nodeValues node value
         setValue nodeTypes node daVinciNodeType
         configure node [
            HTk.text nodeText,
            DaVinci.border DaVinci.cdefault
            ]
         return (DaVinciNode node)
   getNodeType (DaVinciGraph {nodeTypes=nodeTypes}) (DaVinciNode node) = 
      getValue nodeTypes node

instance DeleteNode DaVinciGraph DaVinciNode where
   deleteNode (DaVinciGraph {nodeValues=nodeValues,nodeTypes=nodeTypes})
         (DaVinciNode node) = 
      do
         SIM.destroy node
         deleteFromRegistry nodeValues node
         deleteFromRegistry nodeTypes node

   getNodeValue (DaVinciGraph {nodeValues=nodeValues}) (DaVinciNode node) = 
      getValue nodeValues node

   setNodeValue (DaVinciGraph {nodeValues=nodeValues,nodeTypes=nodeTypes})
         (DaVinciNode node) newValue =
      do
         setValue nodeValues node newValue
         (nodeType@(DaVinciNodeType _ getTitle)) <- 
            getValue nodeTypes node
         newTitle <- getTitle newValue
         HTk.text newTitle node
         done

instance Node DaVinciNode where

daVinciNodeTyCon = mkTyCon "DaVinciGraphDisp" "DaVinciNode"

instance HasTyCon1 DaVinciNode where
   tyCon1 _ = daVinciNodeTyCon

instance NodeType DaVinciNodeType where

-- Although it isn't obligatory, we make DaVinciNodeType things
-- Typeable so that we can save them dynamically.
daVinciNodeTypeTyCon = mkTyCon "DaVinciGraphDisp" "DaVinciNodeType"

instance HasTyCon1 DaVinciNodeType where
   tyCon1 _ = daVinciNodeTypeTyCon

instance NewNodeType DaVinciGraph DaVinciNodeType DaVinciNodeTypeParms where
   newNodeType 
         (daVinciGraph@(DaVinciGraph{
            graph = graph,
            daVinci = daVinci
            })) 
         (DaVinciNodeTypeParms {
            nodeText = nodeText,
            nodeTypeConfigs = nodeTypeConfigs
            }) =
      do
         let
            configs = 
               map 
                  (\ mkConfig -> mkConfig daVinciGraph) 
                  nodeTypeConfigs
         nodeType <- DaVinci.newNodeType graph Nothing (reverse configs)
         return (DaVinciNodeType nodeType nodeText)

instance NodeTypeParms DaVinciNodeTypeParms where
   emptyNodeTypeParms = DaVinciNodeTypeParms {
      nodeText = (\ value -> return ""),
      nodeTypeConfigs = []
      }

instance NodeTypeConfig graphConfig 
   => NodeTypeConfigParms graphConfig DaVinciNodeTypeParms where

   nodeTypeConfigUsed nodeTypeConfig nodeTypeParms = False
   nodeTypeConfig nodeTypeConfig nodeTypeParms = nodeTypeParms

------------------------------------------------------------------------
-- Node type configs for titles and shapes.
------------------------------------------------------------------------

instance NodeTypeConfigParms ValueTitle DaVinciNodeTypeParms where
   nodeTypeConfigUsed _ _ = True
   nodeTypeConfig (ValueTitle nodeText) parms = 
      parms { nodeText = nodeText }

instance NodeTypeConfigParms Shape DaVinciNodeTypeParms where
   nodeTypeConfigUsed _ _ = True
   nodeTypeConfig shape parms =
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
   newArc (DaVinciArcType edgeType) (DaVinciGraph {edgeValues=edgeValues})
         value (DaVinciNode nodeFrom) (DaVinciNode nodeTo) =
      do
         edge <- DaVinci.newEdge Nothing nodeFrom nodeTo 
            [DaVinci.edgetype edgeType]
         setValue edgeValues edge value
         return (DaVinciArc edge)

instance GetFrom DaVinciGraph DaVinciNode DaVinciArc where
   getFrom _ (DaVinciArc edge) =
      do
         nodeFrom <- DaVinci.getSource edge
         return (DaVinciNode nodeFrom)

instance GetTo DaVinciGraph DaVinciNode DaVinciArc where
   getTo _ (DaVinciArc edge) =
      do
         nodeTo <- DaVinci.getTarget edge
         return (DaVinciNode nodeTo)

instance GetArcType DaVinciGraph DaVinciArc DaVinciArcType where
   getArcType _ (DaVinciArc edge) =
      do
         edgeType <- DaVinci.getEdgeType edge
         return (DaVinciArcType edgeType)

instance DeleteArc DaVinciGraph DaVinciArc where
   deleteArc (DaVinciGraph {edgeValues=edgeValues}) (DaVinciArc edge) =
      do
         SIM.destroy edge
         deleteFromRegistry edgeValues edge

   getArcValue (DaVinciGraph {edgeValues=edgeValues}) (DaVinciArc edge) = 
      getValue edgeValues edge

   setArcValue (DaVinciGraph {edgeValues=edgeValues}) (DaVinciArc edge) 
      newValue = setValue edgeValues edge newValue

instance Arc DaVinciArc where

daVinciArcTyCon = mkTyCon "DaVinciGraphDisp" "DaVinciArc"

instance HasTyCon3 DaVinciArc where
   tyCon3 _ = daVinciArcTyCon

instance ArcType DaVinciArcType where

instance NewArcType DaVinciGraph DaVinciArcType DaVinciArcTypeParms where
   newArcType 
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
   => ArcTypeConfigParms arcTypeConfig DaVinciArcTypeParms where

   arcTypeConfigUsed arcTypeConfig arcTypeParms = False
   arcTypeConfig arcTypeConfig arcTypeParms = arcTypeParms

------------------------------------------------------------------------
-- Menus
------------------------------------------------------------------------

instance GraphConfigParms GlobalMenu DaVinciGraphParms where
   graphConfigUsed _ _ = True
   graphConfig globalMenu daVinciGraphParms =
      daVinciGraphParms {
         graphConfigs = (DaVinci.configGraphMenu globalMenu) :
            (graphConfigs daVinciGraphParms)
         }

instance NodeTypeConfigParms LocalMenu DaVinciNodeTypeParms where
   nodeTypeConfigUsed _ _ = True

   nodeTypeConfig localMenu daVinciNodeTypeParms =
      daVinciNodeTypeParms {
         nodeTypeConfigs =
            (\ daVinciGraph -> 
               DaVinci.configNodeTypeMenu 
                  (convertNodeButton localMenu daVinciGraph)
                  ) : (nodeTypeConfigs daVinciNodeTypeParms)
            }

instance ArcTypeConfigParms LocalMenu DaVinciArcTypeParms where
   arcTypeConfigUsed _ _ = True

   arcTypeConfig localMenu daVinciArcTypeParms =
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

