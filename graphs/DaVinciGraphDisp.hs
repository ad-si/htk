{- Here we fit DaVinci into the GraphDisp framework. -}
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
import Concurrent
import Dynamics

import SIM(destroy)

import qualified DaVinci
import qualified HTk
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
   DaVinciGraph DaVinci.Graph

newtype DaVinciGraphParms = DaVinciGraphParms [Config DaVinci.Graph]

instance Graph DaVinciGraph where
   redraw (DaVinciGraph graph) = DaVinci.redrawGraph graph

instance NewGraph DaVinciGraph DaVinciGraphParms where
   newGraph (DaVinciGraphParms graphParms) =
      do
         graph <- DaVinci.newGraph graphParms
         return (DaVinciGraph graph)

instance GraphParms DaVinciGraphParms where
   emptyGraphParms = DaVinciGraphParms []

------------------------------------------------------------------------
-- Nodes
------------------------------------------------------------------------

newtype DaVinciNode value = DaVinciNode DaVinci.Node

newtype DaVinciNodeType value = DaVinciNodeType DaVinci.NodeType

data DaVinciNodeTypeParms value = 
   DaVinciNodeTypeParms {
      nodeTitle :: Maybe String, -- title of the node 
      nodeMenuConfig :: Maybe (IO (Config DaVinci.NodeType))
         -- config option for node type which if present configures a menu
         -- for this node type.
      }

instance NewNode DaVinciGraph DaVinciNode DaVinciNodeType where
   newNode (DaVinciNodeType nodeType) (DaVinciGraph graph)
         value =
      do
         node <- DaVinci.newNode graph Nothing [DaVinci.nodetype nodeType] 
         nodeSet node value
         return (DaVinciNode node)
   readNode (DaVinciGraph graph) (DaVinciNode node) =
      do
         value <- nodeLookup node
         nodeType <- DaVinci.getNodeType node
         return (DaVinciNodeType nodeType,value)

instance DeleteNode DaVinciGraph DaVinciNode where
   deleteNode _ (DaVinciNode node) = 
      do
         destroy node
         nodeDelete node

instance Node DaVinciNode where

instance NodeType DaVinciNodeType where

instance NewNodeType DaVinciGraph DaVinciNodeType DaVinciNodeTypeParms where
   newNodeType (DaVinciGraph graph) 
         (DaVinciNodeTypeParms {
            nodeTitle = nodeTitle,
            nodeMenuConfig = nodeMenuConfig
            }) =
      do
         configList <- case nodeMenuConfig of
            Nothing -> return []
            Just getConfig ->
               do
                  config <- getConfig
                  return [config] 
         nodeType <- DaVinci.newNodeType graph nodeTitle configList
         return (DaVinciNodeType nodeType)

instance NodeTypeParms DaVinciNodeTypeParms where
   emptyNodeTypeParms = DaVinciNodeTypeParms {
      nodeTitle = Nothing,
      nodeMenuConfig = Nothing
      }

------------------------------------------------------------------------
-- Arcs
------------------------------------------------------------------------

newtype DaVinciArc value nodeFromValue nodeToValue = DaVinciArc DaVinci.Edge

newtype DaVinciArcType value = DaVinciArcType DaVinci.EdgeType

data DaVinciArcTypeParms value = 
   DaVinciArcTypeParms {
      arcTitle :: Maybe String, -- title of the node 
      arcMenuConfig :: Maybe (IO (Config DaVinci.EdgeType))
         -- config option for node type which if present configures a menu
         -- for this node type.
      }


instance NewArc DaVinciGraph DaVinciNode DaVinciNode DaVinciArc DaVinciArcType
      where
   newArc (DaVinciArcType edgeType) (DaVinciGraph graph)
         value (DaVinciNode nodeFrom) (DaVinciNode nodeTo) =
      do
         edge <- DaVinci.newEdge Nothing nodeFrom nodeTo 
            [DaVinci.edgetype edgeType]
         edgeSet edge value
         return (DaVinciArc edge)
   readArc (DaVinciGraph graph) (DaVinciArc edge) =
      do
         value <- edgeLookup edge
         edgeType <- DaVinci.getEdgeType edge
         nodeFrom <- DaVinci.getSource edge
         nodeTo <- DaVinci.getTarget edge
         return (DaVinciArcType edgeType,
            value,DaVinciNode nodeFrom,DaVinciNode nodeTo)

instance DeleteArc DaVinciGraph DaVinciArc where
   deleteArc _ (DaVinciArc edge) =
      do
         destroy edge
         edgeDelete edge

instance Arc DaVinciArc where

instance ArcType DaVinciArcType where

instance NewArcType DaVinciGraph DaVinciArcType DaVinciArcTypeParms where
   newArcType (DaVinciGraph graph) 
         (DaVinciArcTypeParms {
            arcTitle = arcTitle,
            arcMenuConfig = arcMenuConfig
            }) =
      do
         configList <- case arcMenuConfig of
            Nothing -> return []
            Just getConfig ->
               do
                  config <- getConfig
                  return [config] 
         edgeType <- DaVinci.newEdgeType graph arcTitle configList
         return (DaVinciArcType edgeType)

instance ArcTypeParms DaVinciArcTypeParms where
   emptyArcTypeParms = DaVinciArcTypeParms {
      arcTitle = Nothing,
      arcMenuConfig = Nothing
      }

------------------------------------------------------------------------
-- Menus
------------------------------------------------------------------------

instance NodeTypeConfigParms MenuButton DaVinciNodeTypeParms where
   nodeTypeConfigUsed _ _ = True

   nodeTypeConfig menuButton daVinciNodeTypeParms =
      daVinciNodeTypeParms {
         nodeMenuConfig =
            Just(
               do
                  newMenu <- convertMenuButton menuButton nodeLookup
                  return (DaVinci.menu newMenu)
               )
         }

instance ArcTypeConfigParms MenuButton DaVinciArcTypeParms where
   arcTypeConfigUsed _ _ = True

   arcTypeConfig menuButton daVinciArcTypeParms =
      daVinciArcTypeParms {
         arcMenuConfig =
            Just(
               do
                  newMenu <- convertMenuButton menuButton edgeLookup
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
-- Dynamic data for Nodes and Edges
-- 
-- This isn't in DaVinci (though I think it's a neat idea) so we
-- have - sadly - to keep it in gigantic FiniteMaps kept in global
-- variables. 
-----------------------------------------------------------------------

nodeSet :: Typeable value => DaVinci.Node -> value -> IO ()
nodeSet node value = nodeSetPrim node (toDyn value)

nodeLookup :: Typeable value => DaVinci.Node -> IO value
nodeLookup node =
   do
      dyn <- nodeLookupPrim node
      case fromDyn dyn of
         Just value -> return value
         Nothing -> ioError (userError (
            "DaVinciGraphDisp.nodeLookup type error!"
            ))

nodeSetPrim :: DaVinci.Node -> Dyn -> IO ()
nodeLookupPrim :: DaVinci.Node -> IO Dyn
nodeDelete :: DaVinci.Node -> IO ()
(nodeSetPrim,nodeLookupPrim,nodeDelete) =
   IOExts.unsafePerformIO makeDynamicMap

edgeSet :: Typeable value => DaVinci.Edge -> value -> IO ()
edgeSet edge value = edgeSetPrim edge (toDyn value)

edgeLookup :: Typeable value => DaVinci.Edge -> IO value
edgeLookup edge =
   do
      dyn <- edgeLookupPrim edge
      case fromDyn dyn of
         Just value -> return value
         Nothing -> ioError (userError (
            "DaVinciGraphDisp.edgeLookup type error!"
            ))

edgeSetPrim :: DaVinci.Edge -> Dyn -> IO ()
edgeLookupPrim :: DaVinci.Edge -> IO Dyn
edgeDelete :: DaVinci.Edge -> IO ()
(edgeSetPrim,edgeLookupPrim,edgeDelete) =
   IOExts.unsafePerformIO makeDynamicMap

makeDynamicMap :: Ord key =>
   IO (
      key -> Dyn -> IO (), -- set function
      key -> IO Dyn, 
         -- lookup function.  Raises match error if key not set
      key -> IO ()
         -- function that deletes key from map.
      ) 
makeDynamicMap =
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
               result <- case lookupFM map key of
                  Just dyn -> return dyn
               putMVar mapMVar map
               return result
         delete key =
            do
               map <- takeMVar mapMVar
               putMVar mapMVar (delFromFM map key)
      return (set,lookup,delete)
   