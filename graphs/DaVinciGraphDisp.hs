{- Here we fit DaVinci into the GraphDisp framework. -}
module DaVinciGraphDisp(
   DaVinciGraph,
   DaVinciGraphParms,
   DaVinciNode,
   DaVinciNodeType,
   DaVinciNodeTypeParms,
   DaVinciArc,
   DaVinciArcType,
   DaVinciArcTypeParms
   ) where

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

newtype DaVinciNode = DaVinciNode DaVinci.Node

newtype DaVinciNodeType = DaVinciNodeType DaVinci.NodeType

data DaVinciNodeTypeParms = 
   DaVinciNodeTypeParms {
      nodeTitle :: Maybe String, -- title of the node 
      nodeMenuConfig :: Maybe (IO (Config DaVinci.NodeType))
         -- config option for node type which if present configures a menu
         -- for this node type.
      }

instance NewNode DaVinciGraph DaVinciNode DaVinciNodeType where
   newNodePrim (DaVinciNodeType nodeType) (DaVinciGraph graph)
         dyn =
      do
         node <- DaVinci.newNode graph Nothing [DaVinci.nodetype nodeType] 
         nodeSet node dyn
         return (DaVinciNode node)
   readNodePrim (DaVinciGraph graph) (DaVinciNode node) =
      do
         dyn <- nodeLookup node
         nodeType <- DaVinci.getNodeType node
         return (DaVinciNodeType nodeType,dyn)

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

newtype DaVinciArc = DaVinciArc DaVinci.Edge

newtype DaVinciArcType = DaVinciArcType DaVinci.EdgeType

data DaVinciArcTypeParms = 
   DaVinciArcTypeParms {
      arcTitle :: Maybe String, -- title of the node 
      arcMenuConfig :: Maybe (IO (Config DaVinci.EdgeType))
         -- config option for node type which if present configures a menu
         -- for this node type.
      }


instance NewArc DaVinciGraph DaVinciNode DaVinciNode DaVinciArc DaVinciArcType
      where
   newArcPrim (DaVinciArcType edgeType) (DaVinciGraph graph)
         (DaVinciNode nodeFrom) (DaVinciNode nodeTo) dyn =
      do
         edge <- DaVinci.newEdge Nothing nodeFrom nodeTo 
            [DaVinci.edgetype edgeType]
         edgeSet edge dyn
         return (DaVinciArc edge)
   readArcPrim (DaVinciGraph graph) (DaVinciArc edge) =
      do
         dyn <- edgeLookup edge
         edgeType <- DaVinci.getEdgeType edge
         nodeFrom <- DaVinci.getSource edge
         nodeTo <- DaVinci.getTarget edge
         return (DaVinciArcType edgeType,
            DaVinciNode nodeFrom,DaVinciNode nodeTo,dyn)

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


convertMenuButton :: MenuButton -> (object -> IO Dyn) -> 
      IO (DaVinci.AppMenu object)
   -- object is either DaVinci.Node or DaVinci.Edge and the function
   -- will be nodeLookup or edgeLookup.
convertMenuButton (Button label dynSink) objectToDyn =
-- We special-case just one button by making it a menu with no
-- name and just one button.  This is because NodeType and EdgeType
-- are instances of HasMenu but nothing else useful that I can see.
   do
      menu <- makeMenu Nothing Nothing
      attachButton menu objectToDyn label dynSink
      return menu
convertMenuButton (Menu textOpt menuButtons) objectToDyn =
   do
      menu <- makeMenu textOpt Nothing
      attachMenuButtons menu objectToDyn menuButtons
      return menu

attachButton :: 
      (PulldownMenu.Menu (DaVinci.Graph -> object -> IO ())) -> 
      (object -> IO Dyn) -> String -> (Dyn -> IO ()) -> IO ()
-- attachButton menu objectToDyn label dynSink
-- attaches a new button to the menu to be attached to the
-- think of type "object".  The button is to have label "label".
-- When the button is clicked on the object, we get the corresponding
-- dyn value using objectToDyn and then execute the action obtained from
-- dynSink.
attachButton menu objectToDyn label dynSink =
   do
      let
         objectSink object =
            do
               dyn <- objectToDyn object
               dynSink dyn
      Button.newButton [
         HTk.text label,
         makeCommand objectSink,
         HTk.parent menu
         ]
      done

attachMenuButtons :: 
      (PulldownMenu.Menu (DaVinci.Graph -> object -> IO ())) -> 
      (object -> IO Dyn) -> [MenuButton] -> IO ()
-- attachMenuButtons menu objectToDyn menuButtons
-- converts a list of MenuButton objects to the enclosing menu, passing the
-- objectToDyn function to the attachButton function when it converts a
-- button.
attachMenuButtons menu objectToDyn [] = done
attachMenuButtons menu objectToDyn (menuButton : rest) =
   do
      case menuButton of
         Button label dynSink -> attachButton menu objectToDyn label dynSink
         Menu textOpt menuButtons ->
            do
               innerMenu <- makeMenu textOpt (Just menu)
               attachMenuButtons innerMenu objectToDyn menuButtons
      attachMenuButtons menu objectToDyn rest

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

nodeSet :: DaVinci.Node -> Dyn -> IO ()
nodeLookup :: DaVinci.Node -> IO Dyn
nodeDelete :: DaVinci.Node -> IO ()
(nodeSet,nodeLookup,nodeDelete) =
   IOExts.unsafePerformIO makeDynamicMap

edgeSet :: DaVinci.Edge -> Dyn -> IO ()
edgeLookup :: DaVinci.Edge -> IO Dyn
edgeDelete :: DaVinci.Edge -> IO ()
(edgeSet,edgeLookup,edgeDelete) =
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
   