{- DisplayGraph.displayGraph displays something implementing the
   Graph interface with something implementing with GraphDisp interface.
   -}
module DisplayGraph(
   displayGraph,
   DisplayGraph
   ) where

import Concurrent(forkIO,killThread)
import Dynamics
import Registry
import Computation (done)
import Object

import Selective

import InfoBus
import SIM(lift,Destructible(..),IA)

import GraphDisp
import Graph

displayGraph ::
      (GraphAll dispGraph graphParms node nodeType nodeTypeParms 
            arc arcType arcTypeParms,
         Typeable nodeLabel,Typeable nodeTypeLabel,Typeable arcLabel,
         Typeable arcTypeLabel,
         Graph.Graph graph)
   => (GraphDisp.Graph dispGraph graphParms node nodeType nodeTypeParms
          arc arcType arcTypeParms)
   -> (graph nodeLabel nodeTypeLabel arcLabel arcTypeLabel)
   -> graphParms -- these are the parameters to use setting up the graph
   -> (DisplayGraph -> NodeType -> nodeTypeLabel 
          -> IO (nodeTypeParms Node))
                 -- this gets parameters for setting up a node type.
                 -- NB - we don't (and can't) recompute the parameters
                 -- if we get a SetNodeTypeLabel or SetArcTypeLabel update.
                 -- We provide the function with the DisplayGraph
                 -- this function will return, to make tying the knot easier
                 -- in versions/VersionGraph.hs
   -> (DisplayGraph -> ArcType -> arcTypeLabel 
         -> IO (arcTypeParms Arc))
                 -- see previous argument.
   -> IO DisplayGraph
displayGraph 
   (displaySort ::
       GraphDisp.Graph dispGraph graphParms node nodeType nodeTypeParms arc 
          arcType arcTypeParms)
   (graph :: graph nodeLabel nodeTypeLabel arcLabel arcTypeLabel)
   graphParms 
   (getNodeParms :: DisplayGraph -> NodeType -> nodeTypeLabel 
      -> IO (nodeTypeParms Node))
   (getArcParms :: DisplayGraph -> ArcType -> arcTypeLabel
      -> IO (arcTypeParms Arc)) =
   do
      msgQueue <- newMsgQueue

      let graphConnection = shareGraph graph

      GraphConnectionData {
         graphState = CannedGraph { updates = updates },
         deRegister = deRegister
         } <- graphConnection (sendIO msgQueue)

-- The nodes of the graph display will have the following types:
#define DispNodeType (nodeType Node)
#define DispNode (node Node)
#define DispArcType (arcType Arc)
#define DispArc (arc Arc Node Node)

      (nodeRegister :: Registry Node DispNode) <- newRegistry
      (nodeTypeRegister :: Registry NodeType DispNodeType)
         <- newRegistry
      (arcRegister :: Registry Arc DispArc) <- newRegistry
      (arcTypeRegister :: Registry ArcType DispArcType)
         <- newRegistry

      dispGraph <- GraphDisp.newGraph displaySort graphParms

      (destructionChannel :: Channel ()) <- newChannel

      oID <- newObject
    
      let
         displayGraph = DisplayGraph {
            oID = oID,
            destroyAction = destroy dispGraph,
            destroyedEvent = lift (receive destructionChannel)
            }

         handleUpdate :: Update nodeLabel nodeTypeLabel arcLabel arcTypeLabel
           -> IO ()
         handleUpdate (NewNodeType nodeType nodeTypeLabel) =
            do
               nodeTypeParms <- 
                  getNodeParms displayGraph nodeType nodeTypeLabel
               dispNodeType <- GraphDisp.newNodeType dispGraph nodeTypeParms
               setValue nodeTypeRegister nodeType dispNodeType
         handleUpdate (SetNodeTypeLabel _ _ ) = done
         handleUpdate (NewNode node nodeType nodeLabel) =
            do
               dispNodeType <- getRegistryValue nodeTypeRegister nodeType
               dispNode <- 
                  GraphDisp.newNode dispGraph dispNodeType node
               setValue nodeRegister node dispNode
         handleUpdate (DeleteNode node) =
            do
               dispNode <- getRegistryValue nodeRegister node
               deleteNode dispGraph dispNode
               deleteFromRegistry nodeRegister node
         handleUpdate (SetNodeLabel node nodeLabel) = done
         handleUpdate (NewArcType arcType arcTypeLabel) =
            do
               arcTypeParms <- 
                  getArcParms displayGraph arcType arcTypeLabel
               dispArcType <- GraphDisp.newArcType dispGraph arcTypeParms
               setValue arcTypeRegister arcType dispArcType
         handleUpdate (SetArcTypeLabel _ _) = done
         handleUpdate (NewArc arc arcType arcLabel source target) =
            do
               dispSource <- getRegistryValue nodeRegister source
               dispTarget <- getRegistryValue nodeRegister target
               dispArcType <- getRegistryValue arcTypeRegister arcType
               dispArc <- GraphDisp.newArc dispGraph dispArcType  
                  arc dispSource dispTarget
               setValue arcRegister arc dispArc
         handleUpdate (DeleteArc arc) =
            do
               dispArc <- getRegistryValue arcRegister arc
               deleteArc dispGraph dispArc
               deleteFromRegistry arcRegister arc
         handleUpdate (SetArcLabel arc arcLabel) = done

      sequence_ (map handleUpdate updates)

      redraw dispGraph

      let
         getAllQueued =
            do
               updateOpt <- poll (receive msgQueue)
               case updateOpt of
                  Nothing -> done
                  Just update ->
                     do
                        handleUpdate update
                        getAllQueued

      let
         monitorThread =
            sync(
                  (lift (receive msgQueue)) >>>=
                     (\ update ->
                        do
                           handleUpdate update
                           getAllQueued
                           redraw dispGraph
                           monitorThread
                         )
               +> (destroyed dispGraph) >>> (
                     do
                        deregisterTool displayGraph
                        deRegister
                        sendIO destructionChannel ()
                     )
               )

      forkIO monitorThread
  
      registerTool displayGraph

      return displayGraph

--------------------------------------------------------------------
-- The DisplayGraph type.  (We create this so that we can end
-- the display tidily.) 
--------------------------------------------------------------------

data DisplayGraph = DisplayGraph {
   oID :: ObjectID,
   destroyAction :: IO (), -- run this to end everything
   destroyedEvent :: IA ()
   }

instance Object DisplayGraph where
   objectID displayGraph = oID displayGraph

instance Destructible DisplayGraph where
   destroy displayGraph = destroyAction displayGraph
   destroyed displayGraph = destroyedEvent displayGraph




