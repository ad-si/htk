-- |
-- Description: Displaying the Structure Graph
-- 
-- DisplayView displays all the objects in a particular view according to a 
-- particular display type. 
module DisplayView(
   DisplayedView,
   displayView,
   addCloseDownAction,
   addNewObjectType,

   openGeneralDisplay,
   ) where

import Maybe

import Data.FiniteMap
import Control.Concurrent.MVar

import Dynamics
import ExtendedPrelude(mapEq,mapOrd)
import Registry
import Thread
import Computation
import Sources
import Delayer
import VariableList hiding (redraw)
import VariableSet
import Sink
import qualified VariableList

import Destructible
import Events (sync)

import GraphDisp
import GraphConfigure


import VersionDB (catchAccessError)
import DisplayTypes
import ObjectTypes
import Link
import LinkDrawer hiding (deleteNode)
import qualified LinkDrawer
import View
import NoAccessObject

-- -----------------------------------------------------------------------
-- Data types
-- -----------------------------------------------------------------------

-- | Contains DisplayedObjectType for all object types in the view
-- We use a Registry as sometime we\'ll have to address the problem of
-- dynamically adding types, perhaps.
-- If an object type does not appear in the registry, it means values of
-- that type are not to be displayed.
newtype AllDisplayedObjectTypes =
   AllDisplayedObjectTypes (UntypedRegistry (Keyed WrappedObjectType))

-- -----------------------------------------------------------------------
-- DisplayedObjectType and DisplayedObjectTypes is the information we need 
-- for all object types in a view.
-- -----------------------------------------------------------------------

-- | This is the data stored by a display for every stored object type.
-- They mostly have the same meaning as the records in NodeDisplayData
data DisplayedObjectType object graph node nodeType arcType =
   DisplayedObjectType {
      arcTypes' :: FiniteMap ArcType (arcType ()),
      nodeTypes' :: FiniteMap NodeType (nodeType (Link object)),
      getNodeType' :: object -> NodeType,
      getNodeLinks' :: Link object -> IO ArcEnds,
      specialNodeActions' :: object 
         -> SimpleSource (graph -> node (Link object) -> IO ())
      }

type TransmittedAction graph node object =
   (graph -> node (Link object) -> IO ())

instance Typeable5_00111 DisplayedObjectType where
   typeOf5_00111 _ = mkTypeRep "DisplayView" "DisplayedObjectType"


-- | Constructs a AllDisplayedObjectTypes for a particular graph, view and
-- wrapped display type.  Also compute all the top links for that type.
-- It also takes as argument a sink for all subequently-created object types.
getAllDisplayedObjectTypes :: 
   GraphAllConfig graph graphParms node nodeType nodeTypeParms 
      arc arcType arcTypeParms
   => (Graph graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms) 
   -> View -> WrappedDisplayType 
   -> IO (DisplayedView graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms)
   -> Sink WrappedObjectType
   -> IO (AllDisplayedObjectTypes,[WrappedLink])
getAllDisplayedObjectTypes 
      (graph :: Graph graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms) 
      view wrappedDisplayType displayedViewAction sink =
   do  
      -- (1) get all object types
      objectTypes <- getAllObjectTypesSinked view sink
      -- (2) initialise the registry
      (registry :: UntypedRegistry (Keyed WrappedObjectType)) <- newRegistry
      let
         allDisplayedObjectTypes = AllDisplayedObjectTypes registry
      -- (3) Now compute it.
      (topLinksList :: [[WrappedLink]]) 
         <- mapM (addNewObjectTypeInner graph view
            allDisplayedObjectTypes wrappedDisplayType displayedViewAction) 
         objectTypes

      -- And return it.                      
      return (AllDisplayedObjectTypes registry,concat topLinksList) 

-- ----------------------------------------------------------------------
-- Add an object type to AllDisplayedObjectTypes and compute its
-- top links
-- -----------------------------------------------------------------------

addNewObjectTypeInner :: forall graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms .
   GraphAllConfig graph graphParms node nodeType nodeTypeParms 
      arc arcType arcTypeParms
   => (Graph graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms) 
   -> View
   -> AllDisplayedObjectTypes 
   -> WrappedDisplayType
   -> IO (DisplayedView graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms)
   -> WrappedObjectType
   -> IO [WrappedLink]
addNewObjectTypeInner 
      (graph :: Graph graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms) 
      view (AllDisplayedObjectTypes registry) wrappedDisplayType 
      displayedViewAction
      ((wrappedObjectType @ (WrappedObjectType (objectType :: objectType)))) 
      =
   do
      nodeDisplayDataOpt <- getNodeDisplayData1 graph view 
         wrappedDisplayType objectType displayedViewAction
      case nodeDisplayDataOpt of
         Nothing -> return []
         Just (nodeDisplayData) ->
            do
               -- Create node types map
               let 
                  nodeTypesList = nodeTypes nodeDisplayData
               graphNodeTypes <- mapM
                  (\ (nodeTypeRep,nodeTypeParms) ->
                     do
                        nodeType <- newNodeType graph nodeTypeParms
                        return (nodeTypeRep,nodeType)
                     )
                  nodeTypesList
               let 
                  nodeTypes' = listToFM graphNodeTypes

               -- Create arc types map
               let 
                  arcTypesList = arcTypes nodeDisplayData
               graphArcTypes <- mapM
                  (\ (arcTypeRep,arcTypeParms) ->
                     do
                        arcType <- newArcType graph arcTypeParms
                        return (arcTypeRep,arcType)
                     )
                  arcTypesList
               let 
                  arcTypes' = listToFM graphArcTypes
                  displayedObjectType = DisplayedObjectType {
                     nodeTypes' = nodeTypes',
                     arcTypes' = arcTypes',
                     getNodeType' = getNodeType nodeDisplayData,
                     getNodeLinks' = getNodeLinks nodeDisplayData,
                     specialNodeActions' = specialNodeActions nodeDisplayData
                     }

                  wrappedLinks :: [WrappedLink]
                  wrappedLinks = 
                     map WrappedLink (topLinks nodeDisplayData)
               setValue registry (Keyed wrappedObjectType) 
                  displayedObjectType
               return wrappedLinks

-- ----------------------------------------------------------------------
-- Add a new object type.
-- -----------------------------------------------------------------------

addNewObjectType :: 
   GraphAllConfig graph graphParms node nodeType nodeTypeParms 
      arc arcType arcTypeParms
   => DisplayedView graph graphParms node nodeType nodeTypeParms 
      arc arcType arcTypeParms
   -> WrappedObjectType 
   -> IO ()
addNewObjectType (displayedView @ DisplayedView {
      graph = graph,
      view = view,
      allDisplayedObjectTypes = allDisplayedObjectTypes,
      allTopLinksSet = allTopLinksSet,
      wrappedDisplayType = wrappedDisplayType
      }) wrappedObjectType =
   do
      topLinks <- addNewObjectTypeInner graph view allDisplayedObjectTypes
         wrappedDisplayType (return displayedView) wrappedObjectType
      mapM_
         (\ topLink -> updateSet allTopLinksSet (AddElement topLink))
         topLinks

-- -----------------------------------------------------------------------
-- DisplayedView is the type representing a single displayed view, possibly
-- under construction.
-- -----------------------------------------------------------------------

data DisplayedView graph graphParms node nodeType nodeTypeParms arc arcType 
      arcTypeParms = 
   DisplayedView {
      graph :: Graph graph graphParms node nodeType nodeTypeParms 
         arc arcType arcTypeParms,
      view :: View,
      linkDrawer :: LinkDrawer WrappedLink (WrappedNode node) ArcType 
         (WrappedArc arc),
      allTopLinksSet :: VariableSet WrappedLink,
      allDisplayedObjectTypes :: AllDisplayedObjectTypes,
      closeDownActions :: MVar [IO ()],
         -- Actions to be done when the graph is closed.  (We repeatedly
         -- remove an action from the list and do it until the list is empty).
      wrappedDisplayType :: WrappedDisplayType
      }

instance 
   GraphAll graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms 
   => Eq (DisplayedView graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms)
   where

   (==) = mapEq graph

instance 
   GraphAll graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms 
   => Ord (DisplayedView graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms)
   where
   
   compare = mapOrd graph

-- ----------------------------------------------------------------------
-- Functions for manipulating DisplayedView
-- -----------------------------------------------------------------------

displayView ::
   GraphAllConfig graph graphParms node nodeType nodeTypeParms 
      arc arcType arcTypeParms
   => (Graph graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms) 
   -> WrappedDisplayType -> View 
   -> IO (DisplayedView graph graphParms node nodeType nodeTypeParms 
         arc arcType arcTypeParms)
displayView 
      (displaySort :: Graph 
      graph graphParms node nodeType nodeTypeParms arc arcType arcTypeParms) 
      wrappedDisplayType view =
   do
      -- (0) Where the displayed view will go, when we've got it
      displayedViewMVar <- newEmptyMVar

      -- (1) get the graph parameters and set up the graph
      graphPars <- graphParms displaySort view wrappedDisplayType
      graph <- newGraph displaySort graphPars 

      -- (2) Create a sink for adding new object types.
      -- We don't actually set the action until the DisplayView
      -- is constructed.
      (objectTypesSink,writeAction) <- newParallelDelayedSink


      -- (3) initialise the types.
      (allDisplayedObjectTypes,allTopLinks) <-
         getAllDisplayedObjectTypes graph view wrappedDisplayType 
            (readMVar displayedViewMVar) objectTypesSink

      let
         AllDisplayedObjectTypes nodeTypesRegistry = allDisplayedObjectTypes

         stopNewObjectTypes = invalidate objectTypesSink

      -- (4) initialise the top links set
      allTopLinksSet <- newVariableSet allTopLinks

      -- (5) define the function to be passed to the LinkDrawer
      let
         -- | newNodeAct allows restart.  If the operation (readLink) fails
         -- with an access error, we construct a NoAccessObject for the link,
         -- and use that instead.
         newNodeAct :: WrappedLink 
            -> IO (NodeData WrappedLink (WrappedNode node) ArcType 
               (WrappedArc arc))
         newNodeAct (wrappedLink @ (WrappedLink (link :: Link object))) =
            do
               (objectOpt :: Maybe object) 
                  <- catchAccessError (readLink view link)
               case objectOpt of
                  Just object -> newNodeAct1 link object
                  Nothing ->
                     do
                        noAccessObject <- createNoAccessObject view wrappedLink
                        newNodeAct1 (coerceLink link) noAccessObject

         newNodeAct1 :: forall objectType object . ObjectType objectType object
            => Link object -> object 
            -> IO (NodeData WrappedLink (WrappedNode node) ArcType 
               (WrappedArc arc))
         newNodeAct1 link object =
            do
               -- (2) get its type
               let
                  wrappedObjectType = WrappedObjectType 
                     (getObjectTypePrim object)

               -- (3) get the DisplayedObjectType
               displayedObjectTypeOpt 
                  <- getValueOpt nodeTypesRegistry (Keyed wrappedObjectType)
               let
                  displayedObjectType = fromMaybe
                     (error ("Type of object to be displayed has no defined "
                        ++ "display data, type registry is "
                        ++ objectTypeTypeId wrappedObjectType))
                     displayedObjectTypeOpt

                  (DisplayedObjectType {
                     arcTypes' = arcTypes',
                     nodeTypes' = (nodeTypes' 
                        :: FiniteMap NodeType (nodeType (Link object))),
                     getNodeType' = getNodeType',
                     getNodeLinks' = getNodeLinks',
                     specialNodeActions' = specialNodeActions'
                     } :: DisplayedObjectType object 
                        graph node nodeType arcType) = displayedObjectType

               -- (4) construct the physical node
               let
                  nodeTypeRep = getNodeType' object
                  (nodeType :: nodeType (Link object)) = lookupWithDefaultFM 
                     nodeTypes'
                     (error "DisplayView: unmatched node type tag")
                     nodeTypeRep
               graphNode <- GraphDisp.newNode graph nodeType link
               let
                  nodeArg = WrappedNode graphNode

               -- (5) arrange to have the special node actions performed.
               
                  thisNodeActions :: SimpleSource (
                     TransmittedAction graph node object)
                  thisNodeActions = specialNodeActions' object

                  (Graph primGraph) = graph 

                  actFn :: TransmittedAction graph node object
                     -> IO ()
                  actFn nodeAction 
                     = nodeAction primGraph graphNode

               (act,sink) <- addNewSink thisNodeActions actFn

               -- (6) construct the list drawer
               let
                  listDrawer0 :: ListDrawer (arcType (),(),WrappedNode node)
                     (arc ())
                  listDrawer0 = newArcListDrawer graph graphNode

                  listDrawer1 :: ListDrawer (arcType (),(),WrappedNode node) 
                     (WrappedArc arc)
                  listDrawer1 = map2ListDrawer toWrappedArc fromWrappedArc
                     listDrawer0

                  listDrawer2 :: ListDrawer (WrappedNode node,ArcType) 
                     (WrappedArc arc)
                  listDrawer2 = coMapListDrawer
                     (\ (wrappedNode,arcType0) ->
                        let
                           arcType1 = lookupWithDefaultFM
                              arcTypes'
                              (error "DisplayView: unmatched arc type tag")
                              arcType0
                        in
                           (arcType1,(),wrappedNode)
                        )
                     listDrawer1

               (listDrawer3 :: ListDrawer (WrappedNode node,ArcType) 
                  (WrappedArc arc)) 
                     <- addDelayerIO (toDelayer view) listDrawer2
             

               -- (7) Construct the NodeData
               outArcs <- getNodeLinks' link

               let
                  deleteNode1 =
                     do
                        invalidate sink
                        GraphDisp.deleteNode graph graphNode

                  nodeData :: NodeData WrappedLink (WrappedNode node) ArcType 
                     (WrappedArc arc)
                  nodeData = NodeData {
                     nodeArg = nodeArg,
                     outArcs = outArcs,
                     innerListDrawer = listDrawer3,
                     LinkDrawer.deleteNode = deleteNode1
                     } 

               -- (8) and return
               return nodeData

      -- (6) create the new link drawer
      linkDrawer <- newLinkDrawer (toSource allTopLinksSet) newNodeAct

      -- (7) construct closeDownActions.
      closeDownActions <- newCloseDownActions

      let 
         displayedView = DisplayedView {
            graph = graph,
            view = view,
            linkDrawer = linkDrawer,
            allTopLinksSet = allTopLinksSet,
            allDisplayedObjectTypes = allDisplayedObjectTypes,
            closeDownActions = closeDownActions,
            wrappedDisplayType = wrappedDisplayType
            }

      --- (8) Tie various knots
      writeAction
         (\ wrappedObjectType -> 
            addNewObjectType displayedView wrappedObjectType
            )

      putMVar displayedViewMVar displayedView

      -- (9) Deal with the endgame.
      addCloseDownAction displayedView stopNewObjectTypes

      forkIO (
         do
            sync (destroyed graph)
            doCloseDownActions displayedView
         )
      return displayedView

-- ------------------------------------------------------------------------    
-- Close down actions for a DisplayedView
-- ------------------------------------------------------------------------

newCloseDownActions :: IO (MVar [IO ()])
newCloseDownActions = newMVar []

-- | Add an action to be done when the view is closed.
addCloseDownAction :: 
   DisplayedView graph graphParms node nodeType nodeTypeParms arc arcType 
      arcTypeParms
   -> IO () -> IO ()
addCloseDownAction displayedView action =
   do
      let mVar = closeDownActions displayedView
      list <- takeMVar mVar
      putMVar mVar (action : list)

-- | Do all such actions.
doCloseDownActions :: 
   DisplayedView graph graphParms node nodeType nodeTypeParms arc arcType 
      arcTypeParms
   -> IO ()
doCloseDownActions displayedView =
   do
      let mVar = closeDownActions displayedView
      list <- takeMVar mVar
      case list of
         [] -> putMVar mVar []
         (action:rest) ->
            do
               putMVar mVar rest
               action
               doCloseDownActions displayedView

-- ------------------------------------------------------------------------    
-- Function suitable for using in DisplayTypes.openDisplayMenuItemPrim
-- ------------------------------------------------------------------------    

openGeneralDisplay :: 
   (  GraphAllConfig graph graphParms node nodeType nodeTypeParms 
         arc arcType arcTypeParms,
      DisplayType displayType
      )
   => (Graph graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms) 
   -> displayType 
   -> View
   -> IO (Maybe (DisplayedView graph graphParms node 
      nodeType nodeTypeParms arc arcType arcTypeParms))
openGeneralDisplay displaySort displayType view =
   do
      displayedView <- displayView displaySort 
         (WrappedDisplayType displayType) view
      return (Just displayedView)

-- ------------------------------------------------------------------------    
-- WrappedArc's.  Needed so we can compare arcs.
-- ------------------------------------------------------------------------    

newtype WrappedArc arc = WrappedArc (arc ())

fromWrappedArc :: WrappedArc arc -> arc ()
fromWrappedArc (WrappedArc arc) = arc

toWrappedArc :: arc () -> WrappedArc arc
toWrappedArc = WrappedArc

instance Eq1 arc => Eq (WrappedArc arc) where
   (==) (WrappedArc arc1) (WrappedArc arc2) = eq1 arc1 arc2

instance Ord1 arc => Ord (WrappedArc arc) where
   compare (WrappedArc arc1) (WrappedArc arc2) = compare1 arc1 arc2
