{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverlappingInstances #-}

-- | This is the implementation of modules GraphDisp and GraphConfigure for
-- daVinci.   See those files for explanation of the names.
-- We encode, for example, the type parameter node as DaVinciNode,
-- and so on for other type parameters, prefixing with \"DaVinci\" and
-- capitalising the next letter.  But the only variable you should normally
-- need from this module is 'daVinciSort'.
module UDrawGraph.Graph(
   daVinciSort, -- Magic type parameter indicating we want to use daVinci.

   DaVinciGraph (pendingChangesLock),
   DaVinciGraphParms,

   DaVinciNode,
   DaVinciNodeType,
   DaVinciNodeTypeParms,

   DaVinciArc,
   DaVinciArcType,
   DaVinciArcTypeParms,
   getDaVinciGraphContext -- :: DaVinciGraph -> Context
   ) where

import Data.Maybe
import Data.Typeable

import Data.IORef
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Concurrent
import qualified Data.Dynamic
import qualified Data.List as List

import Util.Sources
import Util.Sink
import Util.Delayer
import qualified Util.UniqueString as UniqueString
import qualified Util.VariableList as VariableList

import Util.Computation (done)
import Util.Dynamics
import Util.Registry
import Util.ExtendedPrelude
import Util.Thread
import Util.CompileFlags
import Util.Messages

import Events.Channels
import Events.Events
import Events.Destructible
import Events.Synchronized

import Reactor.BSem

import qualified Graphs.GraphDisp as GraphDisp (Graph)
import Graphs.GraphDisp hiding (Graph)
import qualified Graphs.GraphConfigure as GConf (MenuPrim(Menu), Orientation(..))
import Graphs.GraphConfigure hiding (MenuPrim(Menu), Orientation(..))

import UDrawGraph.Types
import UDrawGraph.Basic


------------------------------------------------------------------------
-- How you refer to everything
------------------------------------------------------------------------

daVinciSort :: GraphDisp.Graph DaVinciGraph
   DaVinciGraphParms DaVinciNode DaVinciNodeType DaVinciNodeTypeParms
   DaVinciArc DaVinciArcType DaVinciArcTypeParms
daVinciSort = displaySort

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
      -- cause this list to be flushed, as does redrawPrim.
   pendingChangesLock :: BSem,
      -- This lock is acquired during, flushPendingChanges, newNodePrim,
      -- and setNodeTitle.
      -- Where both pendingChangesLock and pendingChangesMVar are needed,
      -- the first should be got first.

   globalMenuActions :: Registry MenuId (IO ()),
   otherActions :: Registry DaVinciAnswer (IO ()),
   -- The node and edge types contain other event handlers.

   lastSelectionRef :: IORef LastSelection,

   doImprove :: Bool,
   -- improveAll on redrawing graph.

   destructionChannel :: Channel (),

   destroyActions :: IO (),
   -- Various actions to be done when the graph is closed.

   redrawChannel :: Channel Bool,
   -- Sending True along this channel indicates that a
   -- redraw is desired.
   -- Sending False along it ends the appropriate thread.

   delayer :: Delayer,
   redrawAction :: DelayedAction
      -- this is the action that actually gets done when the user actually
      -- asks for a redraw.
   } deriving (Typeable)

data LastSelection = LastNone | LastNode NodeId | LastEdge EdgeId

data DaVinciGraphParms = DaVinciGraphParms {
   graphConfigs :: [DaVinciGraph -> IO ()], -- General setups
   surveyView :: Bool,
   configDoImprove :: Bool,
   configFileMenuActions
      :: Map.Map FileMenuOption (DaVinciGraph -> IO ()),
   configGlobalMenu :: Maybe GlobalMenu,
   configActionWrapper :: IO () -> IO (),
   graphTitleSource :: Maybe (SimpleSource GraphTitle),
   delayerOpt :: Maybe Delayer,
   configOrientation :: Maybe GConf.Orientation
   }

instance Eq DaVinciGraph where
   (==) = mapEq context

instance Ord DaVinciGraph where
   compare = mapOrd context

instance Destroyable DaVinciGraph where
   destroy (daVinciGraph @ DaVinciGraph {
         context = context,nodes = nodes,edges = edges,
         globalMenuActions = globalMenuActions,otherActions = otherActions,
         destroyActions = destroyActions,redrawChannel = redrawChannel,
         delayer = delayer,redrawAction = redrawAction}) =
      do
         cancelDelayedAct delayer redrawAction
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

getDaVinciGraphContext :: DaVinciGraph -> Context
getDaVinciGraphContext g = context g

signalDestruct :: DaVinciGraph -> IO ()
signalDestruct daVinciGraph =
   sync(noWait(send (destructionChannel daVinciGraph) ()))

-- | We run a separate thread for redrawing.  The idea is that when more than
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
                     doInContext (Menu(Layout(ImproveAll)))
                        context
                  else
                     done
               redrawThread daVinciGraph
         else
            done

instance HasDelayer DaVinciGraph where
   toDelayer daVinciGraph = delayer daVinciGraph

instance GraphClass DaVinciGraph where
   redrawPrim (daVinciGraph @ DaVinciGraph{
         delayer = delayer,redrawAction = redrawAction}) =
      delayedAct delayer redrawAction


instance NewGraph DaVinciGraph DaVinciGraphParms where
   newGraphPrim (DaVinciGraphParms {graphConfigs = graphConfigs,
         configDoImprove = configDoImprove,surveyView = surveyView,
         configFileMenuActions = configFileMenuActions,
         configGlobalMenu = configGlobalMenu,
         configActionWrapper = configActionWrapper,
         configOrientation = configOrientation,
         graphTitleSource = graphTitleSource,delayerOpt = delayerOpt}) =
      do
         nodes <- newRegistry
         edges <- newRegistry
         globalMenuActions <- newRegistry
         otherActions <- newRegistry
         lastSelectionRef <- newIORef LastNone

         graphMVar <- newEmptyMVar
            -- this will hold the graph when it's completed.  This is needed
            -- by some of the handler actions.


         let
            -- We now come to write the handler function for
            -- the context.  This is quite complex so we handle the
            -- various cases one by one, in separate functions.

            handler :: DaVinciAnswer -> IO ()
            handler daVinciAnswer
               = configActionWrapper (handler1 daVinciAnswer)
            --
            -- The handler needs to depend on the context, so that it
            -- can handle Close and Print events appropriately.
            handler1 :: DaVinciAnswer -> IO ()
            -- In general, the rule is that if we don't find
            -- a handler function, we do nothing.  We do, however,
            -- assume that where a menuId is quoted, there is
            -- an associated handler.  If not, this is (probably)
            -- a bug in daVinci.
            handler1 daVinciAnswer = case daVinciAnswer of
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
                           NodeData nodeDataData <- getValueHere nodes nodeId
                           (nodeDoubleClickAction (typeData nodeDataData))
                              (valueData nodeDataData)
                     _ -> error "DaVinciGraph: confusing node double click"

            edgeDoubleClick :: IO ()
            edgeDoubleClick =
               do
                  lastSelection <- readIORef lastSelectionRef
                  case lastSelection of
                     LastEdge edgeId ->
                        do
                           ArcData arcType arcValue
                              <- getValueHere edges edgeId
                           (arcDoubleClickAction arcType) arcValue
                     _ -> error "DaVinciGraph: confusing edge double click"
            actGlobalMenu :: MenuId -> IO ()
            actGlobalMenu (MenuId ('#':'%':fileMenuStr)) =
               case toFileMenuOption fileMenuStr of
                  Nothing -> alertMess ("Mysterious daVinci fileMenu "
                     ++ fileMenuStr ++ " ignored")
                  Just reservedMenuOption ->
                     case Map.lookup reservedMenuOption configFileMenuActions of
                        Nothing ->
                           if fileMenuStr == "close"
                              then
                                 alertMess ("The application has disabled "
                                    ++ " the close action for this window.")
                              else
                                 alertMess ("Unexpected daVinci fileMenu "
                                    ++ fileMenuStr ++ " ignored")
                        Just graphAction ->
                           do
                              graph <- readMVar graphMVar
                              graphAction graph
            actGlobalMenu menuId =
               do
                  action <- getValueHere globalMenuActions menuId
                  action

            actNodeMenu :: NodeId -> MenuId -> IO ()
            actNodeMenu nodeId menuId =
               do
                  NodeData nodeDataData <- getValueHere nodes nodeId
                  menuAction <- getValueHere (
                     nodeMenuActions (typeData nodeDataData)) menuId
                  menuAction (valueData nodeDataData)

            actEdgeMenu :: EdgeId -> MenuId -> IO ()
            actEdgeMenu edgeId menuId =
               do
                  ArcData arcType arcValue <- getValueHere edges edgeId
                  menuAction <- getValueHere (arcMenuActions arcType) menuId
                  menuAction arcValue

            -- We now do the drag-and-drops.  There is no special
            -- handler for the create node action, since this is
            -- done by the otherActions handler.
            actCreateNodeAndEdge nodeId =
               do
                  NodeData nodeDataData <- getValueHere nodes nodeId
                  (createNodeAndEdgeAction (typeData nodeDataData))
                     (valueData nodeDataData)

            actCreateEdge :: NodeId -> NodeId -> IO ()
            actCreateEdge nodeId1 nodeId2 =
               do
                  NodeData nodeDataData1 <- getValueHere nodes nodeId2
                  NodeData nodeDataData2 <- getValueHere nodes nodeId1

                  (createEdgeAction (typeData nodeDataData2))
                     (toDyn (valueData nodeDataData1))
                     (valueData nodeDataData2)

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
            setOrientation :: GConf.Orientation -> IO ()
            setOrientation orientation0 =
               let
                  orientation1 = case orientation0 of
                     GConf.TopDown -> TopDown
                     GConf.BottomUp -> BottomUp
                     GConf.LeftRight -> LeftRight
                     GConf.RightLeft -> RightLeft
               in
                  doInContext (Menu (Layout (
                        Orientation orientation1)))
                     context

         case configOrientation of
            Nothing -> done
            Just orientation -> setOrientation orientation

         -- Set up a delayer and a redraw action which uses it.
         delayer <- case delayerOpt of
            Just delayer -> return delayer
            Nothing -> newDelayer

         redrawAction
            <- newDelayedAction (sync(noWait(send redrawChannel True)))
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
                  redrawChannel = redrawChannel,
                  delayer = delayer,
                  redrawAction = redrawAction
                  }

         putMVar graphMVar daVinciGraph

         setValue otherActions Closed (signalDestruct daVinciGraph)
         setValue otherActions Quit (signalDestruct daVinciGraph)

         sequence_ (fmap ($ daVinciGraph) (reverse graphConfigs))

         -- Take control of File Menu events.
         doInContext (AppMenu (ControlFileEvents)) context
         let
            -- Work out which options to enable.
            fileMenuIds = fmap
               (\ (option,_) -> MenuId ("#%"++(fromFileMenuOption option)))
               (Map.toList configFileMenuActions)

         -- Attach globalMenu if necessary and get its menuids as well.
         -- (All global menu-ids need to be activated at once.)
         globalMenuIds <- case configGlobalMenu of
            Nothing -> return []
            Just globalMenu -> mkGlobalMenu daVinciGraph globalMenu

         -- Activate global menus.
         doInContext
            (AppMenu (ActivateMenus (fileMenuIds ++ globalMenuIds)))
            context

         addSink

         -- Do some initial commands.
         doInContext (DVSet(GapWidth 4)) context
         doInContext (DVSet(GapHeight 40)) context
         if surveyView
            then
               doInContext (Menu(View(OpenSurveyView))) context
            else
               done

         forkIODebug (redrawThread daVinciGraph)

         return daVinciGraph

instance GraphParms DaVinciGraphParms where
   emptyGraphParms = DaVinciGraphParms {
      graphConfigs = [],configDoImprove = False,surveyView = False,
      graphTitleSource = Nothing,delayerOpt = Nothing,
      configFileMenuActions = initialFileMenuActions,
      configActionWrapper = (\ act ->
         do
            forkIODebug act
            done
         ),
      configOrientation = Nothing,
      configGlobalMenu = Nothing
      }

initialFileMenuActions :: Map.Map FileMenuOption (DaVinciGraph -> IO ())
initialFileMenuActions = Map.fromList [
   (PrintMenuOption,
      (\ graph -> doInContext
         (Menu (File (Print Nothing))) (context graph))
      ),
   (CloseMenuOption,
      (\ graph ->
         do
            proceed <- confirmMess "Really close window?"
            if proceed then destroy graph else done
         )
      )
   ]

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


instance HasConfig Delayer DaVinciGraphParms where
   configUsed _ _  = True
   ($$) delayer graphParms = graphParms {delayerOpt = Just delayer}

instance HasConfig (SimpleSource GraphTitle) DaVinciGraphParms where
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

instance HasConfig AllowClose DaVinciGraphParms where
   configUsed _ _  = True
   ($$) (AllowClose closeDialogue) =
      let
         actFn (graph :: DaVinciGraph) =
            do
               proceed <- closeDialogue
               if proceed then destroy graph else done
      in
         ($$) (CloseMenuOption,Just actFn)

instance HasConfig FileMenuAct DaVinciGraphParms where
   configUsed _ _ = True
   ($$) (FileMenuAct option actFnOpt) =
      let
         graphActFnOpt = case actFnOpt of
            Nothing -> Nothing
            Just actFn ->
               let
                  graphActFn :: DaVinciGraph -> IO ()
                  graphActFn = const actFn
               in
                  Just graphActFn
      in
         ($$) (option,graphActFnOpt)

instance HasConfig (FileMenuOption,(Maybe (DaVinciGraph -> IO ())))
      DaVinciGraphParms where

   configUsed _ _ = True
   ($$) (option,actFnOpt) daVinciGraphParms =
      let
         configFileMenuActions0 = configFileMenuActions daVinciGraphParms
         configFileMenuActions1 = case actFnOpt of
            Nothing -> Map.delete option configFileMenuActions0
            Just actFn -> Map.insert option actFn configFileMenuActions0
      in
         daVinciGraphParms {configFileMenuActions = configFileMenuActions1}


instance HasConfig GConf.Orientation DaVinciGraphParms where
   configUsed _ _  = True
   ($$) orientation daVinciGraphParms =
      daVinciGraphParms {configOrientation = Just orientation}

instance HasConfig ActionWrapper DaVinciGraphParms where
   configUsed _ _ = True
   ($$) (ActionWrapper wrapper) daVinciGraphParms =
      daVinciGraphParms {configActionWrapper = wrapper}

instance HasConfig AllowDragging DaVinciGraphParms where
   configUsed _ _  = True

   ($$) (AllowDragging allowDragging) =
      addGraphConfigCmd (DragAndDrop
         (if allowDragging then DraggingOn else DraggingOff))

instance HasConfig GlobalMenu DaVinciGraphParms where
   configUsed _ _ = True
   ($$) globalMenu graphParms =
      graphParms {configGlobalMenu = Just globalMenu}

-- Create a global menu and return the ids of the menu-entries, which
-- still need to be activated.
mkGlobalMenu :: DaVinciGraph -> GlobalMenu -> IO [MenuId]
mkGlobalMenu daVinciGraph globalMenu =
   do
      menuEntries <- encodeGlobalMenu globalMenu daVinciGraph
      doInContext (AppMenu(CreateMenus menuEntries))
         (context daVinciGraph)
      return (getMenuIds menuEntries)

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

data DaVinciNode value = DaVinciNode NodeId deriving (Typeable)

-- | Tiresomely we need to make the \"real\" node type untyped.
-- This is so that the interactor which handles drag-and-drop
-- can get the type out without knowing what it is.
data DaVinciNodeType value = DaVinciNodeType {
   nodeType :: Type,
   nodeText :: value -> IO (SimpleSource String),
      -- how to compute the displayed name of the node
   fontStyle :: Maybe (value -> IO (SimpleSource FontStyle)),
      -- how to compute the font style of the node
   border :: Maybe (value -> IO (SimpleSource Border)),
      -- how to compute the border of the node
   nodeMenuActions :: Registry MenuId (value -> IO ()),
   nodeDoubleClickAction :: value -> IO (),
   createNodeAndEdgeAction :: value -> IO (),
   createEdgeAction :: Dyn -> value -> IO ()
   } deriving (Typeable)

data NodeData = forall value . Typeable value =>
   NodeData (NodeDataData value)

-- Extra type is necessary because GHC forbids named typed fields with
-- an existential type.
data NodeDataData value = NodeDataData {
   typeData :: DaVinciNodeType value,
   valueData :: value,
   sink :: SinkID
   }

data DaVinciNodeTypeParms value =
   DaVinciNodeTypeParms {
      nodeAttributes :: Attributes value,
      configNodeText :: value -> IO (SimpleSource String),
      configFontStyle :: Maybe (value -> IO (SimpleSource FontStyle)),
      configBorder :: Maybe (value -> IO (SimpleSource Border)),
      configNodeDoubleClickAction :: value -> IO (),
      configCreateNodeAndEdgeAction :: value -> IO (),
      configCreateEdgeAction :: Dyn -> value -> IO ()
      }

instance Eq1 DaVinciNode where
   eq1 (DaVinciNode n1) (DaVinciNode n2) = (n1 == n2)

instance Ord1 DaVinciNode where
   compare1 (DaVinciNode n1) (DaVinciNode n2) = compare n1 n2

instance Eq1 DaVinciNodeType where
   eq1 = mapEq nodeType


newNodePrim1 :: Typeable value
   => DaVinciGraph -> DaVinciNodeType value -> value -> NodeId
   -> IO (DaVinciNode value)
newNodePrim1 (daVinciGraph @ DaVinciGraph {context=context,nodes=nodes})
      nodeType1
      (value :: value) nodeId =
   do
      attributes <- setUpNodeType daVinciGraph nodeType1 value nodeId
      let
         (daVinciNode :: DaVinciNode value) = DaVinciNode nodeId

      synchronize (pendingChangesLock daVinciGraph) (
            addNodeUpdate daVinciGraph (
               NewNode nodeId (nodeType nodeType1) attributes)
         )
      return daVinciNode

-- | setUpNodeType is used for doing Haskell-side initialisations
-- either after (a) a new node has been created, or (b) we have changed
-- the type.
setUpNodeType :: Typeable value
   => DaVinciGraph -> DaVinciNodeType value -> value -> NodeId
   -> IO [Attribute]
setUpNodeType (daVinciGraph @ DaVinciGraph {context=context,nodes=nodes})
      (nodeType @ DaVinciNodeType {
         nodeType = daVinciNodeType,nodeText = nodeText,
         fontStyle = fontStyle,border = border})
      (value :: value) nodeId =
   do
      thisNodeTextSource <- nodeText value
      fontStyleSourceOpt <- case fontStyle of
         Nothing -> return Nothing
         Just getFontStyleSource ->
            do
               fontStyleSource <- getFontStyleSource value
               return (Just fontStyleSource)

      borderSourceOpt <- case border of
         Nothing -> return Nothing
         Just getBorderSource ->
            do
               borderSource <- getBorderSource value
               return (Just borderSource)
      let
         (daVinciNode :: DaVinciNode value) = DaVinciNode nodeId
      sinkID <- newSinkID
      transformValue nodes nodeId (\ nodeDataOpt ->
         do
            case nodeDataOpt of
               Nothing -> done
               Just (NodeData oldNodeData) -> invalidate (sink oldNodeData)
                  -- this prevents any more updates to these nodes.
            let
               newNodeData = NodeDataData {
                  typeData = nodeType,
                  valueData = value,
                  sink = sinkID
                  }
            return (Just (NodeData newNodeData),())
         )
      let
         addNodeAction
            :: SimpleSource a
            -> (DaVinciGraph -> DaVinciNode value -> a -> IO b)
            -> IO a
         addNodeAction source actFun =
            do
               let
                  updateFn a =
                     do
                        actFun daVinciGraph daVinciNode a
                        done
               (a,_) <- addNewSinkGeneral source updateFn sinkID
               return a

      synchronize (pendingChangesLock daVinciGraph) (
         do
            thisNodeText <- addNodeAction thisNodeTextSource setNodeTitle
            let
               attributes1 = [titleAttribute thisNodeText]

            attributes2 <-
               case fontStyleSourceOpt of
                  Nothing -> return attributes1
                  Just fontStyleSource ->
                     do
                         thisFontStyle
                            <- addNodeAction fontStyleSource setFontStyle
                         return (fontStyleAttribute thisFontStyle
                            : attributes1)
            attributes3 <-
               case borderSourceOpt of
                  Nothing -> return attributes2
                  Just borderSource ->
                     do
                         thisBorder <- addNodeAction borderSource setBorder
                         return (borderAttribute thisBorder
                            : attributes2)
            return attributes3
         )

instance NewNode DaVinciGraph DaVinciNode DaVinciNodeType where
   newNodePrim graph nodeType value =
      do
         nodeId <- newNodeId (context graph)
         newNodePrim1 graph nodeType value nodeId

   setNodeTypePrim graph (node@ (DaVinciNode nodeId) :: DaVinciNode value)
         nodeType1 =
      do
         -- Check first to see if the type really needs changing.
         goAhead <-
            do
               nodeDataOpt <- getValueOpt (nodes graph) nodeId
               return (case nodeDataOpt of
                  Nothing -> False -- Node seems to have been deleted anyway
                  Just (NodeData nodeData) ->
                     let
                        nodeType0 = typeData nodeData
                     in
                        nodeType nodeType0 /= nodeType nodeType1
                  )
         if goAhead
            then
               do
                  flushPendingChanges graph
                  value <- getNodeValuePrim graph node
                  attributes <- setUpNodeType graph nodeType1 value nodeId
                  synchronize (pendingChangesLock graph) (
                     do
                        doInContext
                           (Graph (ChangeType
                              [NodeType nodeId (nodeType nodeType1)]))
                           (context graph)
                        doInContext
                           (Graph (ChangeAttr [
                              Node nodeId attributes]))
                           (context graph)
                     )
            else
               done

instance DeleteNode DaVinciGraph DaVinciNode where
   deleteNodePrim (daVinciGraph @
            DaVinciGraph {context = context,nodes = nodes})
         (DaVinciNode nodeId) =
      transformValue nodes nodeId (\ nodeDataOpt ->
         case nodeDataOpt of
            Nothing -> return (nodeDataOpt,())
            Just (NodeData nodeDataData) ->
               do
                  invalidate (sink nodeDataData)
                  addNodeUpdate daVinciGraph (DeleteNode nodeId)
                  return (Nothing,())
            )

   getNodeValuePrim (daVinciGraph @ DaVinciGraph {
         context = context,nodes = nodes}) (DaVinciNode nodeId) =
      do
         (Just (NodeData nodeDataData)) <- getValueOpt nodes nodeId
         return (coDyn (valueData nodeDataData))

   setNodeValuePrim
         (daVinciGraph @ DaVinciGraph {context = context,nodes = nodes})
         (daVinciNode @ (DaVinciNode nodeId))
         newValue =
      do
         typeOpt <- transformValue nodes nodeId
            (\ nodeDataOpt ->
               return (
                  case nodeDataOpt of
                     Nothing -> (nodeDataOpt,Nothing)
                     Just (NodeData nodeDataData0) ->
                       let
                          nodeDataData1 = nodeDataData0 {
                             valueData = coDyn newValue}
                       in
                          (Just (NodeData nodeDataData1),
                              Just (coDyn (typeData nodeDataData1)))
                  )
               )

         case typeOpt of
            Nothing -> done -- node has disappeared
            Just nodeType ->
               do
                  newTitleSource <- nodeText nodeType newValue
                  newTitle <- readContents newTitleSource
                  setNodeTitle daVinciGraph daVinciNode newTitle
                  done

   getMultipleNodesPrim daVinciGraph mkAct =
      do
         channel <- newChannel

         lastSel <- newIORef Nothing

         let
            mapNode :: NodeId -> DaVinciNodeType value -> DaVinciNode value
            mapNode nodeId _ = DaVinciNode nodeId

            closeAct =
               do
                  errorMess
                     "Unexpected close interrupting multiple node selection!"
                  signalDestruct daVinciGraph

            newHandler :: DaVinciAnswer -> IO ()
            newHandler answer = case answer of
               NodeSelectionsLabels [nodeId]
                  -> writeIORef lastSel (Just nodeId)
               NodeSelectionsLabels _ -> done -- not a double click
               NodeDoubleClick ->
                  do
                     nodeIdOpt <- readIORef lastSel
                     case nodeIdOpt of
                        Nothing -> errorMess "Confusing node selection ignored"
                        Just nodeId ->
                           do
                              nodeDataOpt
                                 <- getValueOpt (nodes daVinciGraph) nodeId
                              case nodeDataOpt of
                                 Nothing -> errorMess
                                    "Confusing node selection ignored (2)"
                                 Just (NodeData nodeDataData) ->
                                    do
                                       let
                                          wrappedNode = WrappedNode
                                             (mapNode nodeId (
                                                typeData nodeDataData))
                                       sync(noWait(send channel wrappedNode))

               EdgeSelectionLabel _ -> done
               EdgeSelectionLabels _ _ -> done
               Closed -> closeAct
               Quit -> closeAct
               _ ->  errorMess
                  "Other user input ignored during multiple node selection"


            act = mkAct (receive channel)

         withHandler newHandler (context daVinciGraph) act

instance SetNodeFocus DaVinciGraph DaVinciNode where
   setNodeFocusPrim (daVinciGraph @
            DaVinciGraph {context = context,nodes = nodes})
         (DaVinciNode nodeId) =
      transformValue nodes nodeId (\ nodeDataOpt ->
         case nodeDataOpt of
            Nothing -> return (nodeDataOpt,())
            Just (NodeData nodeDataData) ->
               do
                  doInContext (Special (FocusNodeAnimated nodeId)) context
                  return (nodeDataOpt,())
            )


instance NodeClass DaVinciNode

instance NodeTypeClass DaVinciNodeType

instance NewNodeType DaVinciGraph DaVinciNodeType DaVinciNodeTypeParms where
   newNodeTypePrim
         (daVinciGraph@(DaVinciGraph {context = context}))
         (DaVinciNodeTypeParms {
            nodeAttributes = nodeAttributes,
            configNodeText = configNodeText,
            configFontStyle = configFontStyle,
            configBorder = configBorder,
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
            fontStyle = configFontStyle
            border = configBorder
            nodeDoubleClickAction = configNodeDoubleClickAction
            createNodeAndEdgeAction = configCreateNodeAndEdgeAction
            createEdgeAction = configCreateEdgeAction
         return (DaVinciNodeType {
            nodeType = nodeType,
            nodeText = nodeText,
            fontStyle = fontStyle,
            border = border,
            nodeMenuActions = nodeMenuActions,
            nodeDoubleClickAction = nodeDoubleClickAction,
            createNodeAndEdgeAction = createNodeAndEdgeAction,
            createEdgeAction = createEdgeAction
            })

instance NodeTypeParms DaVinciNodeTypeParms where
   emptyNodeTypeParms = DaVinciNodeTypeParms {
      nodeAttributes = emptyAttributes,
      configNodeText = const (return (staticSimpleSource "")),
      configFontStyle = Nothing,
      configBorder = Nothing,
      configNodeDoubleClickAction = const done,
      configCreateNodeAndEdgeAction = const done,
      configCreateEdgeAction = const (const done)
      }

   coMapNodeTypeParms coMapFn
      (DaVinciNodeTypeParms {
         nodeAttributes = nodeAttributes,
         configNodeText = configNodeText,
         configFontStyle = configFontStyle,
         configBorder = configBorder,
         configNodeDoubleClickAction = configNodeDoubleClickAction,
         configCreateNodeAndEdgeAction = configCreateNodeAndEdgeAction,
         configCreateEdgeAction = configCreateEdgeAction
         }) =
      DaVinciNodeTypeParms {
         nodeAttributes = coMapAttributes coMapFn nodeAttributes,
         configNodeText = configNodeText . coMapFn,
         configFontStyle = (fmap (. coMapFn) configFontStyle),
         configBorder = (fmap (. coMapFn) configBorder),
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
   ($$$) (ValueTitle nodeText') parms =
      let
         nodeText value =
            do
               initial <- nodeText' value
               return (staticSimpleSource initial)
      in
         parms { configNodeText = nodeText }

instance HasConfigValue ValueTitleSource DaVinciNodeTypeParms where
   configUsed' _ _ = True
   ($$$) (ValueTitleSource nodeText) parms =
      parms { configNodeText = nodeText }

instance HasConfigValue FontStyleSource DaVinciNodeTypeParms where
   configUsed' _ _ = True
   ($$$) (FontStyleSource fontStyleSource) parms =
      parms { configFontStyle = Just fontStyleSource }

instance HasConfigValue BorderSource DaVinciNodeTypeParms where
   configUsed' _ _ = True
   ($$$) (BorderSource borderSource) parms =
      parms { configBorder = Just borderSource }

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
         doInContext (Menu (
            Abstraction ((if hide then HideEdges else ShowEdges) [nodeId])
            )) (context daVinciGraph)
         case daVinciVersion of
            Just _ -> done
            Nothing ->
               -- work around daVinci 2 bug which causes edge hiding to be
               -- delayed
               doInContext (Graph (ChangeAttr ([Node nodeId []])))
                  (context daVinciGraph)

instance HasModifyValue Attribute DaVinciGraph DaVinciNode where
   modify attribute daVinciGraph (DaVinciNode nodeId) =
      do
         flushPendingChanges daVinciGraph
         doInContext
            (Graph (ChangeAttr [Node nodeId [attribute]]))
            (context daVinciGraph)

instance HasModifyValue (String,String) DaVinciGraph DaVinciNode where
   modify (key,value) = modify (A key value)


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

data DaVinciArc value = DaVinciArc EdgeId deriving (Typeable)

-- Like nodes, the "real" type is monomorphic.
data DaVinciArcType value = DaVinciArcType {
   arcType :: Type,
   arcMenuActions :: Registry MenuId (value -> IO ()),
   arcDoubleClickAction :: value -> IO (),
   arcArcText :: value -> IO (SimpleSource String)
--   arcTitleFunc :: value -> String
   } deriving (Typeable)

data DaVinciArcTypeParms value =
      DaVinciArcTypeParms {
         arcAttributes :: Attributes value,
         configArcDoubleClickAction :: value -> IO (),
         configArcText :: value -> IO (SimpleSource String)
--         configArcTitleFunc :: value -> String
         }
   |  InvisibleArcTypeParms

data ArcData = forall value . Typeable value
   => ArcData (DaVinciArcType value) value


instance Eq1 DaVinciArc where
   eq1 (DaVinciArc n1) (DaVinciArc n2) = (n1 == n2)

instance Ord1 DaVinciArc where
   compare1 (DaVinciArc n1) (DaVinciArc n2) = compare n1 n2

addArcGeneral :: Typeable value
    => DaVinciGraph -> DaVinciArcType value
    -> DaVinciArc value -> value
    -> DaVinciNode nodeFromValue -> DaVinciNode nodeToValue
    -> IO ()
addArcGeneral
      (daVinciGraph @ DaVinciGraph {edges = edges})
      daVinciArcType (DaVinciArc edgeId) value
      (DaVinciNode nodeFrom) (DaVinciNode nodeTo) =
   do
      if daVinciArcType `eq1` invisibleArcType
         then
            done
         else
            do
               s <- (arcArcText daVinciArcType) value
               arcText <- readContents(s)
               atts <- return ([titleAttribute arcText])
               setValue edges edgeId (ArcData daVinciArcType value)
               addEdgeUpdate daVinciGraph
                  (NewEdge edgeId (arcType daVinciArcType) atts nodeFrom nodeTo)

instance NewArc DaVinciGraph DaVinciNode DaVinciNode DaVinciArc
         DaVinciArcType
      where
   newArcPrim daVinciGraph daVinciArcType value nodeFrom nodeTo =
      do
         edgeId <- newEdgeId (context daVinciGraph)
         let
            newArc = DaVinciArc edgeId

         addArcGeneral daVinciGraph daVinciArcType newArc value
            nodeFrom nodeTo

         return newArc

   newArcListDrawerPrim
         (daVinciGraph @ DaVinciGraph {context = context,edges = edges})
         nodeFrom =
      -- We ignore positional data for now, since daVinci does too.

      let
         newPos _ aOpt =
            do
               edgeId <- newEdgeId context
               let
                  newArc = DaVinciArc edgeId
               case aOpt of
                  Nothing -> done
                  Just (daVinciArcType,value,WrappedNode nodeTo) ->
                     addArcGeneral daVinciGraph daVinciArcType newArc
                        value nodeFrom nodeTo
               return newArc

         setPos (arc@(DaVinciArc edgeId)) aOpt =
            do
               -- Delete the old, if present
               delPos arc

               -- Add the new
               case aOpt of
                  Nothing -> done
                  Just (daVinciArcType,value,WrappedNode nodeTo) ->
                     addArcGeneral daVinciGraph daVinciArcType arc
                        value nodeFrom nodeTo

         delPos (DaVinciArc edgeId) =
            do
               -- Delete the old, if present
               deleteOld <- deleteFromRegistryBool edges edgeId
               if deleteOld
                  then
                     addEdgeUpdate daVinciGraph (DeleteEdge edgeId)
                  else
                     done

         redraw' = redrawPrim daVinciGraph

         listDrawer = VariableList.ListDrawer {
            VariableList.newPos = newPos,VariableList.setPos = setPos,
            VariableList.delPos = delPos,VariableList.redraw = redraw'}
      in
         listDrawer

instance SetArcType DaVinciGraph DaVinciArc DaVinciArcType where
   setArcTypePrim daVinciGraph (davinciArc@(DaVinciArc edgeId))
         daVinciArcType =
      error "Sorry, setArcType is not implemented for daVinci"

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

instance ArcClass DaVinciArc

instance Eq1 DaVinciArcType where
   eq1 = mapEq arcType

instance Ord1 DaVinciArcType where
   compare1 = mapOrd arcType

instance ArcTypeClass DaVinciArcType where
   invisibleArcType = DaVinciArcType {
      arcType = Type (UniqueString.newNonUnique "Invisible"),
      arcMenuActions = error "daVinciGraph.invisible1",
      arcDoubleClickAction = error "daVinciGraph.invisible2",
      arcArcText = error "daVinciGraph.invisible3"
      }

instance NewArcType DaVinciGraph DaVinciArcType DaVinciArcTypeParms where
   newArcTypePrim _ InvisibleArcTypeParms = return invisibleArcType

   newArcTypePrim
         (daVinciGraph@DaVinciGraph{context = context})
         (DaVinciArcTypeParms{arcAttributes = arcAttributes,
            configArcDoubleClickAction = configArcDoubleClickAction,
            configArcText = configArcText
--            configArcTitleFunc = configArcTitleFunc
            }) =
      do
         arcType <- newType context
         (arcMenuActions,attributes)
            <- encodeAttributes arcAttributes daVinciGraph
         doInContext (Visual(AddRules [ER arcType attributes])) context
         let
            arcDoubleClickAction = configArcDoubleClickAction
            arcArcText = configArcText
         return (DaVinciArcType {
            arcType = arcType,
            arcMenuActions = arcMenuActions,
            arcDoubleClickAction = arcDoubleClickAction,
            arcArcText = arcArcText
            })

instance ArcTypeParms DaVinciArcTypeParms where
   emptyArcTypeParms = DaVinciArcTypeParms {
      arcAttributes = emptyAttributes,
      configArcDoubleClickAction = const done,
      configArcText = const (return (staticSimpleSource ""))
      }

   invisibleArcTypeParms = InvisibleArcTypeParms

   coMapArcTypeParms coMapFn
      (DaVinciArcTypeParms {
         arcAttributes = arcAttributes,
         configArcDoubleClickAction = configArcDoubleClickAction,
         configArcText = configArcText
         }) =
      (DaVinciArcTypeParms {
         arcAttributes = coMapAttributes coMapFn arcAttributes,
         configArcDoubleClickAction = configArcDoubleClickAction . coMapFn,
         configArcText = configArcText . coMapFn
         })
   coMapArcTypeParms coMapFn InvisibleArcTypeParms = InvisibleArcTypeParms


instance HasConfigValue Color DaVinciArcTypeParms where
   configUsed' _ _ = True
   ($$$) (Color colorName) parms =
      parms {arcAttributes = (Att "EDGECOLOR" colorName) $$$
         (arcAttributes parms)}

instance HasConfigValue EdgeDir DaVinciArcTypeParms where
   configUsed' _ _ = True
   ($$$) (Dir dirStr) parms =
        parms {arcAttributes = (Att "_DIR" dirStr) $$$
           (arcAttributes parms)}

instance HasConfigValue Head DaVinciArcTypeParms where
   configUsed' _ _ = True
   ($$$) (Head headStr) parms =
        parms {arcAttributes = (Att "HEAD" headStr) $$$
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

{--
instance HasConfigValue TitleFunc DaVinciArcTypeParms where
   configUsed' _ _ = True
   ($$$) (TitleFunc func) parms =
      parms {configArcTitleFunc = func}
--}

instance HasConfigValue ValueTitle DaVinciArcTypeParms where
   configUsed' _ _ = True
   ($$$) (ValueTitle arcText') parms =
      let
         arcText value =
            do
               initial <- arcText' value
               return (staticSimpleSource initial)
      in
         parms { configArcText = arcText }


------------------------------------------------------------------------
-- Attributes in general
-- The Attributes type encodes the attributes in a DaVinciNodeTypeParms
-- or a DaVinciArcTypeParms
------------------------------------------------------------------------

data Attributes value = Attributes {
   options :: Map.Map String String,
   menuOpt :: Maybe (LocalMenu value)
   }

emptyAttributes :: Attributes value
emptyAttributes = Attributes {
   options = Map.empty,
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
         options = Map.insert key value (options attributes)
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
            fmap
               (\ (key,value) -> A key value)
               (Map.toList (options attributes))
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
      (LocalMenu (menuPrim0 :: GConf.MenuPrim (Maybe String) (value -> IO ())))
      (DaVinciGraph {context = context}) =
   do
      registry <- newRegistry
      (menuPrim1 :: GConf.MenuPrim (Maybe String) MenuId) <-
         mapMMenuPrim
            (\ valueToAct ->
               do
                  menuId <- newMenuId context
                  setValue registry menuId valueToAct
                  return menuId
               )
            menuPrim0
      (menuPrim2 :: GConf.MenuPrim (Maybe String,MenuId) MenuId) <-
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
         SubmenuEntryMne menuId _ menuEntries _ ->
             menuId : getMenuIds menuEntries
         BlankMenuEntry -> []
         _ -> error "DaVinciGraph: (Sub)MenuEntryDisabled not yet handled."

encodeGlobalMenu :: GlobalMenu -> DaVinciGraph -> IO [MenuEntry]
-- This constructs a global menu.  The menuId actions are written
-- directly into the graphs globalMenuActions registry.
encodeGlobalMenu (GlobalMenu
                  (menuPrim0 :: GConf.MenuPrim (Maybe String) (IO ())))
      (DaVinciGraph {context = context,globalMenuActions = globalMenuActions})
       =
   do
      (menuPrim1 :: GConf.MenuPrim (Maybe String) MenuId) <-
         mapMMenuPrim
            (\ action ->
               do
                  menuId <- newMenuId context
                  setValue globalMenuActions menuId action
                  return menuId
               )
            menuPrim0
      (menuPrim2 :: GConf.MenuPrim (Maybe String,MenuId) MenuId) <-
         mapMMenuPrim'
            (\ stringOpt ->
               do
                  menuId <- newMenuId context
                  return (stringOpt,menuId)
               )
            menuPrim1
      return (encodeDaVinciMenu menuPrim2)

encodeDaVinciMenu :: GConf.MenuPrim (Maybe String,MenuId) MenuId -> [MenuEntry]
-- Used for encoding all menus.  The MenuId in the first argument is
-- used as all submenus need to have a unique menuId, even though
-- daVinci can't send that as an event.
encodeDaVinciMenu menuHead =
   case menuHead of
      GConf.Menu (Nothing,_) menuPrims ->
         encodeMenuList menuPrims
      GConf.Menu (Just label,menuId) menuPrims ->
         [SubmenuEntry menuId (MenuLabel label) (encodeMenuList menuPrims)]
      single -> [encodeMenuItem single]

   where
      encodeMenuList :: [GConf.MenuPrim (Maybe String,MenuId) MenuId]
                     -> [MenuEntry]
      encodeMenuList menuPrims = fmap encodeMenuItem menuPrims

      encodeMenuItem :: GConf.MenuPrim (Maybe String,MenuId) MenuId
                     -> MenuEntry
      encodeMenuItem (Button label menuId) = MenuEntry menuId (MenuLabel label)
      encodeMenuItem (GConf.Menu (labelOpt,menuId) menuItems) =
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
         Graph(UpdateMixed (reverse pendingChanges))
      else
         sortPendingChanges1 pendingChanges

sortPendingChanges1 :: [MixedUpdate] -> DaVinciCmd
sortPendingChanges1 pendingChanges =
   let
      (nodeUpdates :: [NodeUpdate],edgeUpdates1 :: [EdgeUpdate]) =
         foldr -- so that the nodes are in the same order as in list.
            (\ change (nodesSF,edgesSF) ->
               case change of
                  NU(n @ (NewNode _ _ _)) -> (n:nodesSF,edgesSF)
                  NU(n @ (DeleteNode _)) -> (n:nodesSF,edgesSF)
                  EU(e @ (NewEdge _ _ _ _ _)) -> (nodesSF,e:edgesSF)
                  EU(e @ (DeleteEdge _)) -> (nodesSF,e:edgesSF)
               )
            ([],[])
            pendingChanges

      -- We need to eliminate NewEdge updates for edges
      -- containing deleted nodes, and DeleteEdge updates for these
      -- eliminated edges.
      finalState = toFinalState pendingChanges

      deletedNodes :: Set.Set NodeId
      deletedNodes = Set.fromList (mapMaybe
         (\ update -> case update of
               NU (DeleteNode nodeId) -> Just nodeId
               _ -> Nothing
            )
         finalState
         )

      (edgeUpdates2 :: [EdgeUpdate],obsoleteEdges :: Set.Set EdgeId) =
         foldl
            (\ (eSF,oSF) e -> case e of
               (NewEdge edgeId _ _ nodeFrom nodeTo) ->
                  if (Set.member nodeFrom deletedNodes) ||
                     (Set.member nodeTo deletedNodes)
                     then (eSF,Set.insert edgeId oSF)
                     else (e:eSF,oSF)
               _ -> (e:eSF,oSF)
               )
            ([],Set.empty)
            edgeUpdates1

      (edgeUpdates3 :: [EdgeUpdate]) = List.filter
         (\ e -> case e of
            DeleteEdge edgeId -> not (Set.member edgeId obsoleteEdges)
            _ -> True
            )
         (reverse edgeUpdates2)
      in
         Graph(Update nodeUpdates edgeUpdates3)

removeNullifyingChanges :: [MixedUpdate]  -> [MixedUpdate]
removeNullifyingChanges [] = []
removeNullifyingChanges (update:r) = case update of
  NU (DeleteNode nid) -> case findN nid of
    (r,[]) -> update:removeNullifyingChanges r
    (h,_:t) -> removeNullifyingChanges $ h ++ t
  EU (DeleteEdge eid) -> case findE eid of
    (r,[]) -> update:removeNullifyingChanges r
    (h,_:t) -> removeNullifyingChanges $ h ++ t
  -- EU (NewEdgeBehind eid _ _ _ _ _) ->
  _ -> update:removeNullifyingChanges r
  where
    findN i = span (\ mu -> case mu of
                              NU (NewNode nid' _ _) -> i /= nid'
                              _ -> True) r
    findE i = span (\ mu -> case mu of
                              EU (NewEdge eid' _ _ _ _) -> i /= eid'
                              _ -> True) r

flushPendingChanges :: DaVinciGraph -> IO ()
flushPendingChanges (DaVinciGraph {context = context,nodes = nodes,
      edges = edges,pendingChangesMVar = pendingChangesMVar,
      pendingChangesLock = pendingChangesLock}) =
   synchronize pendingChangesLock (
      do
         pendingChanges <- takeMVar pendingChangesMVar
         putMVar pendingChangesMVar []
         case removeNullifyingChanges pendingChanges of
            [] -> done
            ps -> do
              let isDelete u = case u of
                    NU (DeleteNode _) -> True
                    EU (DeleteEdge _) -> True
                    _ -> False
                  splitUp = List.groupBy (\ u1 u2 ->
                      isDelete u1 == isDelete u2)
              mapM_ (\ p -> doInContext (sortPendingChanges p) context)
                    $ reverse $ splitUp ps
         -- Delete registry entries for all now-irrelevant node and edge
         -- entries.
         -- NB.  This will miss deleting entries for edges which are
         -- attached to nodes which get deleted without being
         -- deleted themselves, but I can't be bothered now to do
         -- anything about this.
         sequence_ (fmap
            (\ pendingChange -> case pendingChange of
               NU (DeleteNode nodeId) -> deleteFromRegistry nodes nodeId
               EU (DeleteEdge edgeId) -> deleteFromRegistry edges edgeId
               _ -> done
               )
            (toFinalState pendingChanges)
            )
      )

-- For each node or edge created or destroyed in the list, delete all but
-- the first operation applied to it (== the last added to the list)
toFinalState :: [MixedUpdate] -> [MixedUpdate]
toFinalState = uniqOrdByKeyOrder toId
   where
      toId :: MixedUpdate -> Either NodeId EdgeId
      toId (NU (DeleteNode nodeId)) = Left nodeId
      toId (NU (NewNode nodeId _ _)) = Left nodeId
      toId (EU (DeleteEdge edgeId)) = Right edgeId
      toId (EU (NewEdge edgeId _ _ _ _)) = Right edgeId
      toId (EU (NewEdgeBehind _ edgeId _ _ _  _)) = Right edgeId

-- -----------------------------------------------------------------------
-- Setting node titles and font styles.
-- -----------------------------------------------------------------------

-- | This is called internally, by the function set up by newNodePrim.
-- The function returns False to indicate that this function failed as
-- the node has been deleted.
-- (This behaviour may now be useless anyway but I can't be bothered
-- to change it.)
setNodeTitle :: Typeable value => DaVinciGraph -> DaVinciNode value -> String
   -> IO Bool
setNodeTitle daVinciGraph (daVinciNode@(DaVinciNode nodeId)) newTitle =
   do
      flushPendingChanges daVinciGraph
      nodeDataOpt <- getValueOpt (nodes daVinciGraph) nodeId
      case nodeDataOpt of
         Nothing -> return False
         Just (nodeData :: NodeData) ->
            do
               modify (titleAttribute newTitle) daVinciGraph daVinciNode
               return True

titleAttribute :: String -> Attribute
titleAttribute title = A "OBJECT" title

-- | This function similarly changes the font style.
setFontStyle :: Typeable value => DaVinciGraph -> DaVinciNode value
   -> FontStyle -> IO Bool
setFontStyle daVinciGraph (daVinciNode@(DaVinciNode nodeId)) fontStyle =
   do
      flushPendingChanges daVinciGraph
      nodeDataOpt <- getValueOpt (nodes daVinciGraph) nodeId
      case nodeDataOpt of
         Nothing -> return False
         Just (nodeData :: NodeData) ->
            do
               modify (fontStyleAttribute fontStyle) daVinciGraph daVinciNode
               return True

fontStyleAttribute :: FontStyle -> Attribute
fontStyleAttribute fontStyle =
   let
      fontStyleStr = case fontStyle of
         NormalFontStyle -> "normal"
         BoldFontStyle -> "bold"
         ItalicFontStyle -> "italic"
         BoldItalicFontStyle -> "bold_italic"
   in
      A "FONTSTYLE" fontStyleStr

-- | This function similarly changes the border.
setBorder :: Typeable value => DaVinciGraph -> DaVinciNode value
   -> Border -> IO Bool
setBorder daVinciGraph (daVinciNode@(DaVinciNode nodeId)) border =
   do
      flushPendingChanges daVinciGraph
      nodeDataOpt <- getValueOpt (nodes daVinciGraph) nodeId
      case nodeDataOpt of
         Nothing -> return False
         Just (nodeData :: NodeData) ->
            do
               modify (borderAttribute border) daVinciGraph daVinciNode
               return True

borderAttribute :: Border -> Attribute
borderAttribute border =
   let
      borderStr = case border of
         NoBorder -> "none"
         SingleBorder -> "single"
         DoubleBorder -> "double"
   in
      A "BORDER" borderStr

-- -----------------------------------------------------------------------
-- Turning a FileMenuOption into a String and vice-versa
-- -----------------------------------------------------------------------

fromFileMenuOption :: FileMenuOption -> String
fromFileMenuOption option =
   case lookup option menuOptionList of
      Just s -> s

toFileMenuOption :: String -> Maybe FileMenuOption
toFileMenuOption s =
   lookup s (fmap (\ (o,s) -> (s,o)) menuOptionList)

menuOptionList :: [(FileMenuOption,String)]
menuOptionList = [
   (NewMenuOption,   "new"),
   (OpenMenuOption,  "open"),
   (SaveMenuOption,  "save"),
   (SaveAsMenuOption,"saveas"),
   (PrintMenuOption, "print"),
   (CloseMenuOption, "close"),
   (ExitMenuOption,  "exit")
   ]

-- -----------------------------------------------------------------------
-- Miscellaneous functions
-- -----------------------------------------------------------------------

-- Transforming one type to another when we know they are
-- actually identical . . .
coDyn :: (Typeable a,Typeable b) => a -> b
coDyn valueA =
   case cast valueA of
      Just valueB -> valueB

-- ---------------------------------------------------------------------
-- A safer version of getValue
-- ---------------------------------------------------------------------

getValueHere :: GetSetRegistry registry from to => registry -> from -> IO to
getValueHere =
   if isDebug
      then
         getValueSafe "DaVinciGraph getValue"
      else
         getValue
