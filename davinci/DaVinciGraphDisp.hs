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
   DaVinciGraph DaVinci.Graph DaVinci.DaVinci

instance SIM.Destructible DaVinciGraph where
   SIM.destroy (DaVinciGraph graph _ ) = SIM.destroy graph
   SIM.destroyed (DaVinciGraph graph daVinci ) = (
         SIM.destroyed graph 
      +> DaVinci.lastGraphClosed daVinci
      +> SIM.destroyed daVinci
      )


newtype DaVinciGraphParms = DaVinciGraphParms {
   graphConfigs :: [Config DaVinci.Graph]
   }

instance Graph DaVinciGraph where
   redraw (DaVinciGraph graph _) = DaVinci.redrawGraph graph

instance NewGraph DaVinciGraph DaVinciGraphParms where
   newGraph (DaVinciGraphParms {graphConfigs = graphConfigs}) =
      do
         (daVinci :: DaVinci.DaVinci) <- DaVinci.davinci []
         graph <- DaVinci.newGraph (graphConfigs ++ [
            DaVinci.gapwidth 4,
            DaVinci.gapheight 40,
            DaVinci.dragging DaVinci.On
            ])

         DaVinci.displayGraph graph
         DaVinci.newSurveyView graph

         return (DaVinciGraph graph daVinci)

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
      nodeTypeConfigs :: [Config DaVinci.NodeType]
         -- config option for node type which if present configures a menu
         -- for this node type.
      }

instance NewNode DaVinciGraph DaVinciNode DaVinciNodeType where
   newNode daVinciNodeType@(DaVinciNodeType nodeType getNodeTitle) 
         (DaVinciGraph graph _) value =
      do
         nodeText <- getNodeTitle value
         node <- DaVinci.newNode graph Nothing [DaVinci.nodetype nodeType]
         nodeSet node value
         nodeTypeSet node daVinciNodeType
         configure node [
            HTk.text nodeText,
            DaVinci.shape DaVinci.cdefault,
            DaVinci.border DaVinci.cdefault
            ]
         return (DaVinciNode node)
   getNodeType _ (DaVinciNode node) = nodeTypeLookup node

instance DeleteNode DaVinciGraph DaVinciNode where
   deleteNode _ (DaVinciNode node) = 
      do
         SIM.destroy node
         nodeDelete node
         nodeTypeDelete node

   getNodeValue _ (DaVinciNode node) = nodeLookup node

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
   newNodeType (daVinciGraph@(DaVinciGraph graph daVinci)) 
         (DaVinciNodeTypeParms {
            nodeText = nodeText,
            nodeTypeConfigs = nodeTypeConfigs
            }) =
      do
         nodeType <- DaVinci.newNodeType graph Nothing nodeTypeConfigs
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

instance NodeTypeConfigParms ValueTitle DaVinciNodeTypeParms where
   nodeTypeConfigUsed _ _ = True
   nodeTypeConfig (ValueTitle nodeText) parms = 
      parms { nodeText = nodeText }

------------------------------------------------------------------------
-- Arcs
-- These differ from nodes in that they don't have texts, but do
-- have endnodes.
------------------------------------------------------------------------

newtype DaVinciArc value nodeFromValue nodeToValue = DaVinciArc DaVinci.Edge

newtype DaVinciArcType value = DaVinciArcType DaVinci.EdgeType

data DaVinciArcTypeParms value = 
   DaVinciArcTypeParms {
      arcTypeConfigs :: [Config DaVinci.EdgeType]
         -- config option for arc type which if present configures a menu
         -- for this node type.  
      }


instance NewArc DaVinciGraph DaVinciNode DaVinciNode DaVinciArc DaVinciArcType
      where
   newArc (DaVinciArcType edgeType) _
         value (DaVinciNode nodeFrom) (DaVinciNode nodeTo) =
      do
         edge <- DaVinci.newEdge Nothing nodeFrom nodeTo 
            [DaVinci.edgetype edgeType]
         edgeSet edge value
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
   deleteArc _ (DaVinciArc edge) =
      do
         SIM.destroy edge
         edgeDelete edge

   getArcValue _ (DaVinciArc edge) = edgeLookup edge

instance Arc DaVinciArc where

daVinciArcTyCon = mkTyCon "DaVinciGraphDisp" "DaVinciArc"

instance HasTyCon3 DaVinciArc where
   tyCon3 _ = daVinciArcTyCon

instance ArcType DaVinciArcType where

instance NewArcType DaVinciGraph DaVinciArcType DaVinciArcTypeParms where
   newArcType (daVinciGraph@(DaVinciGraph graph daVinci)) 
         (DaVinciArcTypeParms {
            arcTypeConfigs = arcTypeConfigs
            }) =
      do
         edgeType <- DaVinci.newEdgeType graph Nothing arcTypeConfigs

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
            (DaVinci.configNodeTypeMenu (convertNodeButton localMenu)) :
               (nodeTypeConfigs daVinciNodeTypeParms)
            }

instance ArcTypeConfigParms LocalMenu DaVinciArcTypeParms where
   arcTypeConfigUsed _ _ = True

   arcTypeConfig localMenu daVinciArcTypeParms =
      daVinciArcTypeParms {
         arcTypeConfigs =
            (DaVinci.configEdgeTypeMenu (convertEdgeButton localMenu)) :
               (arcTypeConfigs daVinciArcTypeParms)
            }


convertNodeButton :: Typeable value => LocalMenu value 
   -> LocalMenu DaVinci.Node
convertNodeButton (LocalMenu menuPrim) =
   LocalMenu(
      mapMenuPrim
         (\ action ->
            let
               actionNode node =
                  do
                     value <- nodeLookup node
                     action value
            in
               actionNode
            )   
         menuPrim
         )

convertEdgeButton :: Typeable value => LocalMenu value 
   -> LocalMenu DaVinci.Edge
convertEdgeButton (LocalMenu menuPrim) =
   LocalMenu(
      mapMenuPrim
         (\ action ->
            let
               actionEdge edge =
                  do
                     value <- edgeLookup edge
                     action value
            in
               actionEdge
            )   
         menuPrim
      )

------------------------------------------------------------------------
-- Dynamic data
-- We need to keep dynamic data for the following reasons:
-- (1) to get from DaVinci.Node's to their associated values.
-- (2) ditto for DaVinci.Edge's.
-- (3) to get from DaVinci.Node's to their associated DaVinciNodeType's.
-----------------------------------------------------------------------

nodeSet :: Typeable value => DaVinci.Node -> value -> IO ()
nodeLookup :: Typeable value => DaVinci.Node -> IO value
nodeDelete :: DaVinci.Node -> IO ()

nodeSet node value = nodeSetPrim node (toDyn value)
nodeLookup node =
   do
      dyn <- nodeLookupPrim node
      case fromDyn dyn of
         Just value -> return value
         Nothing -> ioError(userError(
            "Type failure in DaVinciGraphDisp.nodeLookup"
            ))
      
(nodeSetPrim,nodeLookupPrim,nodeDelete) =
   IOExts.unsafePerformIO makeLookupTable

edgeSet :: Typeable value => DaVinci.Edge -> value -> IO ()
edgeLookup :: Typeable value => DaVinci.Edge -> IO value

edgeSet edge value = edgeSetPrim edge (toDyn value)
edgeLookup edge =
   do
      dyn <- edgeLookupPrim edge
      case fromDyn dyn of
         Just value -> return value
         Nothing -> ioError(userError(
            "Type failure in DaVinciGraphDisp.edgeLookup"
            ))

(edgeSetPrim,edgeLookupPrim,edgeDelete) =
   IOExts.unsafePerformIO makeLookupTable

nodeTypeSet :: Typeable value => DaVinci.Node -> DaVinciNodeType value 
   -> IO ()
nodeTypeLookup :: Typeable value => DaVinci.Node 
   -> IO (DaVinciNodeType value)
nodeTypeDelete :: DaVinci.Node -> IO ()

nodeTypeSet nodeType value = nodeTypeSetPrim nodeType (toDyn value)
nodeTypeLookup nodeType =
   do
      dyn <- nodeTypeLookupPrim nodeType
      case fromDyn dyn of
         Just value -> return value
         Nothing -> ioError(userError(
            "Type failure in DaVinciGraphDisp.nodeTypeLookup"
            ))

(nodeTypeSetPrim,nodeTypeLookupPrim,nodeTypeDelete) =
   IOExts.unsafePerformIO makeLookupTable

makeLookupTable :: Ord key =>
   IO (
      key -> Dyn -> IO (), -- set function
      key -> IO Dyn, 
         -- lookup function.  Raises match error if key not set
      key -> IO ()
         -- function that deletes key from map.
      ) 
makeLookupTable =
   do
      mapMVar <- newMVar emptyFM
      let
         set key dyn =
            do
               map <- takeMVar mapMVar
               putMVar mapMVar (addToFM map key dyn)
         lookup key = 
            do
               map <- takeMVar mapMVar
               dyn <- case lookupFM map key of
                  Just dyn -> return dyn
               putMVar mapMVar map
               return dyn
         delete key =
            do
               map <- takeMVar mapMVar
               putMVar mapMVar (delFromFM map key)
      return (set,lookup,delete)
