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
import qualified Button
import qualified MenuButton
import qualified PulldownMenu

import GraphDisp


-- We follow the order of the GraphDisp file.

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


newtype DaVinciGraphParms = DaVinciGraphParms [Config DaVinci.Graph]

instance Graph DaVinciGraph where
   redraw (DaVinciGraph graph _) = DaVinci.redrawGraph graph

instance NewGraph DaVinciGraph DaVinciGraphParms where
   newGraph (DaVinciGraphParms graphParms) =
      do
         (daVinci :: DaVinci.DaVinci) <- DaVinci.davinci []
         graph <- DaVinci.newGraph (graphParms ++ [
            DaVinci.gapwidth 4,
            DaVinci.gapheight 40,
            DaVinci.dragging DaVinci.On
            ])

         DaVinci.displayGraph graph
         DaVinci.newSurveyView graph

         return (DaVinciGraph graph daVinci)

instance GraphParms DaVinciGraphParms where
   emptyGraphParms = DaVinciGraphParms []

instance GraphConfigParms GraphTitle DaVinciGraphParms where
   graphConfigUsed graphConfig graphParms = True
   graphConfig (GraphTitle graphTitle) (DaVinciGraphParms graphParms) =
      DaVinciGraphParms (HTk.text graphTitle : graphParms)

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

daVinciNodeTyCon = mkTyCon "DaVinciGraphDisp" "DaVinciNodeType"

instance Typeable value => Typeable (DaVinciNodeType value) where
   typeOf _ = daVinciNodeTypeTag
      where
         bot :: value
         bot = bot
         valueTag = typeOf bot
         daVinciNodeTypeTag = mkTypeTag daVinciNodeTyCon [valueTag] 

data DaVinciNodeTypeParms value = 
   DaVinciNodeTypeParms {
      nodeText :: (value -> IO String),
      nodeMenuConfig :: Maybe (DaVinciGraph -> IO (Config DaVinci.NodeType))
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

instance NodeType DaVinciNodeType where

instance NewNodeType DaVinciGraph DaVinciNodeType DaVinciNodeTypeParms where
   newNodeType (daVinciGraph@(DaVinciGraph graph daVinci)) 
         (DaVinciNodeTypeParms {
            nodeText = nodeText,
            nodeMenuConfig = nodeMenuConfig
            }) =
      do
         configList <- case nodeMenuConfig of
            Nothing -> return []
            Just getConfig ->
               do
                  config <- getConfig daVinciGraph
                  return [config] 
         nodeType <- DaVinci.newNodeType graph Nothing configList
         return (DaVinciNodeType nodeType nodeText)

instance NodeTypeParms DaVinciNodeTypeParms where
   emptyNodeTypeParms = DaVinciNodeTypeParms {
      nodeText = (\ value -> return ""),
      nodeMenuConfig = Nothing
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
      arcMenuConfig :: Maybe (DaVinciGraph -> IO (Config DaVinci.EdgeType))
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

instance ArcType DaVinciArcType where

instance NewArcType DaVinciGraph DaVinciArcType DaVinciArcTypeParms where
   newArcType (daVinciGraph@(DaVinciGraph graph daVinci)) 
         (DaVinciArcTypeParms {
            arcMenuConfig = arcMenuConfig
            }) =
      do
         configList <- case arcMenuConfig of
            Nothing -> return []
            Just getConfig ->
               do
                  config <- getConfig daVinciGraph
                  return [config]
         edgeType <- DaVinci.newEdgeType graph Nothing configList

         return (DaVinciArcType edgeType)

instance ArcTypeParms DaVinciArcTypeParms where
   emptyArcTypeParms = DaVinciArcTypeParms {
      arcMenuConfig = Nothing
      }

instance ArcTypeConfig arcTypeConfig 
   => ArcTypeConfigParms arcTypeConfig DaVinciArcTypeParms where

   arcTypeConfigUsed arcTypeConfig arcTypeParms = False
   arcTypeConfig arcTypeConfig arcTypeParms = arcTypeParms

------------------------------------------------------------------------
-- Menus
------------------------------------------------------------------------

instance NodeTypeConfigParms MenuButton DaVinciNodeTypeParms where
   nodeTypeConfigUsed _ _ = True

   nodeTypeConfig menuButton daVinciNodeTypeParms =
      daVinciNodeTypeParms {
         nodeMenuConfig =
            Just(
               \ (DaVinciGraph graph daVinci) ->
                  do
                     newMenu <- convertMenuButton menuButton nodeLookup
                     DaVinci.popupInterActor daVinci graph newMenu 
                        Nothing DaVinci.popupSelectionNode
                     return (DaVinci.menu newMenu)
               )
         }

instance ArcTypeConfigParms MenuButton DaVinciArcTypeParms where
   arcTypeConfigUsed _ _ = True

   arcTypeConfig menuButton daVinciArcTypeParms =
      daVinciArcTypeParms {
         arcMenuConfig =
            Just(
               \ (DaVinciGraph graph daVinci) ->
                  do
                     newMenu <- convertMenuButton menuButton edgeLookup
                     DaVinci.popupInterActor daVinci graph newMenu 
                        Nothing DaVinci.popupSelectionEdge
                     return (DaVinci.menu newMenu)
               )
         }


convertMenuButton :: Typeable value 
   => MenuButton value -> (object -> IO value) 
   -> IO (DaVinci.AppMenu object)
   -- object is either DaVinci.Node or DaVinci.Edge and the function
   -- will be nodeLookup or edgeLookup.
convertMenuButton (Button label valueSink) objectToValue =
-- We special-case just one button by making it a menu with no
-- name and just one button.  This is because NodeType and EdgeType
-- are instances of HasMenu but nothing else useful that I can see.
   do
      menu <- makeMenu Nothing Nothing
      attachButton menu objectToValue label valueSink
      return menu
convertMenuButton (Menu textOpt menuButtons) objectToValue =
   do
      menu <- makeMenu textOpt Nothing
      attachMenuButtons menu objectToValue menuButtons
      return menu

attachButton :: 
      (PulldownMenu.Menu (DaVinci.Graph -> object -> IO ())) -> 
      (object -> IO value) -> String -> (value -> IO ()) -> IO ()
-- attachButton menu objectToValue label valueSink
-- attaches a new button to the menu to be attached to the
-- think of type "object".  The button is to have label "label".
-- When the button is clicked on the object, we get the corresponding
-- value using objectToValue and then execute the action obtained from
-- valueSink.
attachButton menu objectToValue label valueSink =
   do
      let
         objectSink object =
            do
               value <- objectToValue object
               valueSink value
      Button.newButton [
         HTk.text label,
         makeCommand objectSink,
         HTk.parent menu
         ]
      done

attachMenuButtons :: 
      (PulldownMenu.Menu (DaVinci.Graph -> object -> IO ())) -> 
      (object -> IO value) -> [MenuButton value] -> IO ()
-- attachMenuButtons menu objectToValue menuButtons
-- converts a list of MenuButton objects to the enclosing menu, passing the
-- objectToValue function to the attachButton function when it converts a
-- button.
attachMenuButtons menu objectToValue [] = done
attachMenuButtons menu objectToValue (menuButton : rest) =
   do
      case menuButton of
         Button label valueSink -> 
            attachButton menu objectToValue label valueSink
         Menu textOpt menuButtons ->
            do
               innerMenu <- makeMenu textOpt (Just menu)
               attachMenuButtons innerMenu objectToValue menuButtons
      attachMenuButtons menu objectToValue rest

makeCommand :: (object -> IO ()) -> 
      Config (DaVinci.Button (DaVinci.Graph -> object -> IO ()))
-- HTk (and DaVinci?) allow a 
--    Button (DaVinci.Graph -> object -> IO ())
-- to have a command attached to it using the
--    command :: ( () -> IO (DaVinci.Graph -> object -> IO ()) ) ->
--        Config (Button (DaVinci.Graph -> object -> IO ()))
-- function (which comes out of
--    instance HasCommand () Button a
--    )
-- We construct such a config using the supplied action, to be 
-- executed when the object (a DaVinci node or edge) is clicked.
makeCommand objectSink =
   let
      graphObjectSink graph object = objectSink object
   in
      HTk.command (\ () -> return graphObjectSink)

makeMenu :: Maybe String -> Maybe (HTk.Menu a) -> IO (HTk.Menu a)
-- Makes a menu with header text (if specified) and parent menu
makeMenu textOpt (Just (parentMenu :: HTk.Menu a)) =
   do
      let
         confs1 = case textOpt of
            Nothing -> []
            Just text -> [HTk.text text]
         confs2 = (HTk.parent parentMenu) : confs1
      (menuButton::MenuButton.MenuButton a) <- MenuButton.newMenuButton confs2
      menu <- PulldownMenu.newMenu [HTk.parent menuButton]
      return menu
makeMenu Nothing Nothing = PulldownMenu.newMenu []
makeMenu (Just title) Nothing =
-- no real provision for this in HTk.  We create a sub-menu with the title.
   do
      menu <- PulldownMenu.newMenu []
      makeMenu (Just title) (Just menu)
   
      
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
