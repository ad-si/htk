#if (__GLASGOW_HASKELL__ >= 503)
#define NEW_GHC 
#else
#undef NEW_GHC
#endif

{- DisplayView displays all the objects in a particular view according to a 
   particular display type. -}
module DisplayView(
   DisplayedView,
   displayView,
   focusLink,
   addCloseDownAction,
   ) where

import IOExts
import FiniteMap
import Concurrent

import Dynamics
import Registry
import Thread
import Computation
import Sink
import VariableSet

import Events
import Destructible

import BSem

import GraphDisp
import GraphConfigure

import DisplayTypes
import ObjectTypes
import Link
import View

-- -----------------------------------------------------------------------
-- DisplayedObjectType and DisplayedObjectTypes is the information we need 
-- for all object types in a view.
-- -----------------------------------------------------------------------

---
-- This is the data stored by a display for every stored object type.
-- They mostly have the same meaning as the records in NodeDisplayData
data DisplayedObjectType objectType object nodeType arcType =
   DisplayedObjectType {
      arcTypes' :: FiniteMap ArcType (arcType ()),
      nodeTypes' :: FiniteMap NodeType (nodeType (String,Link object)),
      getNodeType' :: object -> NodeType,
      mustFocus' :: Link object -> IO Bool,
      focus' :: Link object -> IO (VariableSetSource (WrappedLink,ArcType),
         VariableSetSource (WrappedLink,ArcType)),
      closeDown' :: IO ()
      }

#ifdef NEW_GHC
displayedObjectTypeTyRep = mkTyRep "DisplayView" "DisplayedObjectType"

instance HasTyRep4_0011 DisplayedObjectType where
   tyRep4_0011 _ = displayedObjectTypeTyRep
#endif

---
-- Contains DisplayedObjectType for all object types in the view
-- We use a Registry as sometime we'll have to address the problem of
-- dynamically adding types, perhaps.
-- If an object type does not appear in the registry, it means values of
-- that type are not to be displayed.
newtype AllDisplayedObjectTypes =
   AllDisplayedObjectTypes (UnsafeRegistry (Keyed WrappedObjectType))

---
-- Constructs a AllDisplayedObjectTypes for a particular graph, view and
-- wrapped display type.  Also compute all the top links for that type.
getAllDisplayedObjectTypes :: 
   GraphAllConfig graph graphParms node nodeType nodeTypeParms 
      arc arcType arcTypeParms
   => (Graph graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms) 
   -> View -> WrappedDisplayType 
   -> IO (DisplayedView graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms)
   -> IO (AllDisplayedObjectTypes,[WrappedLink])
getAllDisplayedObjectTypes 
      (graph :: Graph graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms) 
      view wrappedDisplayType displayedViewAction =
   do  
      -- (1) get all object types
      objectTypes <- getAllObjectTypes view
      -- (2) initialise the registry
      (registry :: UnsafeRegistry (Keyed WrappedObjectType)) <- newRegistry
      -- (3) Now compute it.
      (topLinksList :: [[WrappedLink]]) <- mapM
         (\ (wrappedObjectType @ (WrappedObjectType 
               (objectType :: objectType))) ->
            do
               nodeDisplayDataOpt <- getNodeDisplayData view 
                  wrappedDisplayType objectType displayedViewAction
               case nodeDisplayDataOpt of
                  Nothing -> return []
                  Just (nodeDisplayData :: NodeDisplayData nodeTypeParms 
                        arcTypeParms objectType object) ->
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

                           displayedObjectType :: DisplayedObjectType 
                                 objectType object 
                                 nodeType arcType
                           displayedObjectType = DisplayedObjectType {
                              nodeTypes' = nodeTypes',
                              arcTypes' = arcTypes',
                              getNodeType' = getNodeType nodeDisplayData,
                              mustFocus' = mustFocus nodeDisplayData,
                              focus' = focus nodeDisplayData,
                              closeDown' = closeDown nodeDisplayData
                              }

                           wrappedLinks :: [WrappedLink]
                           wrappedLinks = 
                              map WrappedLink (topLinks nodeDisplayData)
                        setValue registry (Keyed wrappedObjectType) 
                           displayedObjectType
                        return wrappedLinks
            )
            objectTypes                  
      -- And return it.                      
      return (AllDisplayedObjectTypes registry,concat topLinksList) 

-- -----------------------------------------------------------------------
-- DisplayedView is the type representing a single displayed view, possibly
-- under construction.
-- -----------------------------------------------------------------------

type NodeData node object = (node (String,Link object),MVar Bool,SinkID)

data DisplayedView graph graphParms node nodeType nodeTypeParms arc arcType 
      arcTypeParms = 
   DisplayedView {
      graph :: Graph graph graphParms node nodeType nodeTypeParms 
         arc arcType arcTypeParms,
      view :: View,
      nodes :: UnsafeRegistry (Keyed WrappedLink), 
         -- The registry should map to values of type
         -- NodeData node object

         -- all nodes in the graph displayed so far.
         -- The MVar indicates if the node has been focussed.
         -- We attach the SinkID to variable sets containing arcs, so that
         -- when the node is deleted, we can stop the arc sets being
         -- deleted.
      allDisplayedObjectTypes :: AllDisplayedObjectTypes,
      closeDownActions :: MVar [IO ()]
         -- Actions to be done when the graph is closed.  (We repeatedly
         -- remove an action from the list and do it until the list is empty).
      }


-- -----------------------------------------------------------------------
-- WrappedNode
-- -----------------------------------------------------------------------

class (HasTyRep1 node,ObjectType objectType object) 
   => WrappedNodeContents node object objectType 

instance (HasTyRep1 node,ObjectType objectType object) 
   => WrappedNodeContents node object objectType 

data WrappedNode node = 
   forall object objectType . WrappedNodeContents node object objectType 
   => WrappedNode (node (String,Link object))
                                  
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
      graphPars <- graphParms view wrappedDisplayType
      graph <- newGraph displaySort graphPars

      -- (2) construct the nodes registry
      nodes <- newRegistry

      -- (3) construct allDisplayedObjectTypes and get the top links.
      (allDisplayedObjectTypes,allTopLinks) 
         <- getAllDisplayedObjectTypes graph view wrappedDisplayType
            (readMVar displayedViewMVar)

      -- (4) construct closeDownActions.
      closeDownActions <- newCloseDownActions

      let 
         displayedView = DisplayedView {
            graph = graph,
            view = view,
            nodes = nodes,
            allDisplayedObjectTypes = allDisplayedObjectTypes,
            closeDownActions = closeDownActions
            }

      putMVar displayedViewMVar displayedView

      -- (5) display the nodes.
      mapM_ (displayNode displayedView) allTopLinks

      -- (6) redraw
      redraw graph

      -- (6) Handle destruction of the window
      forkIO (
         do
            sync (destroyed graph)
            doCloseDownActions displayedView
         )
      return displayedView

---
-- Displays a particular node.  In detail
-- (1) if the node type is not displayed in this window (getNodeDisplayData
-- returned Nothing), return Nothing.
-- (2) if the node has not already been displayed, display it, and return the
--     new graph node.
-- (3) otherwise return the graph node.
displayNode :: 
   GraphAllConfig graph graphParms node nodeType nodeTypeParms 
      arc arcType arcTypeParms
   => (DisplayedView graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms) 
   -> WrappedLink -> IO (Maybe (WrappedNode node))
displayNode displayedView (WrappedLink link) =
   do
      nodeOpt <- displayNodeUnWrapped displayedView link
      case nodeOpt of
         Nothing -> return Nothing
         Just node -> return (Just (WrappedNode node))

displayNodeUnWrapped :: 
   (GraphAllConfig graph graphParms node nodeType nodeTypeParms 
      arc arcType arcTypeParms,ObjectType objectType object)
   => (DisplayedView graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms) 
   -> Link object -> IO (Maybe (node (String,Link object)))
displayNodeUnWrapped 
      (displayedView@DisplayedView{view=view,graph=graph,nodes=nodes,
         allDisplayedObjectTypes=AllDisplayedObjectTypes nodeTypesRegistry} 
         :: DisplayedView graph graphParms node nodeType nodeTypeParms arc 
            arcType arcTypeParms) 
      (link :: Link object) =
   do
      object <- readLink view link
      let 
         wrappedObjectType = WrappedObjectType (getObjectTypePrim object)

         getDisplayedObjectTypeOpt :: ObjectType objectType object =>
            IO (Maybe (DisplayedObjectType objectType object nodeType arcType))
         getDisplayedObjectTypeOpt = 
            getValueOpt nodeTypesRegistry (Keyed wrappedObjectType)

      displayedObjectTypeOpt <- getDisplayedObjectTypeOpt
      case displayedObjectTypeOpt of
         Nothing -> return Nothing
         Just (displayedObjectType @ DisplayedObjectType{
            arcTypes' = arcTypes',
            nodeTypes' = nodeTypes',
            getNodeType' = getNodeType',
            mustFocus' = mustFocus',
            focus' = focus',
            closeDown' = closeDown'
            } :: DisplayedObjectType objectType object nodeType arcType) ->
            do
               (graphNode :: node (String,Link object),considerFocus) <- 
                  transformValue nodes (Keyed (WrappedLink link))
                  (\ (nodeDataOpt :: Maybe (NodeData node object)) ->
                     case nodeDataOpt of
                        Just (graphNode,_,_) ->
                           return (nodeDataOpt,
                              (graphNode,False))
                        Nothing ->
                           do
                              let
                                 nodeTypeRep = getNodeType' object
                                 nodeType = case 
                                       lookupFM nodeTypes' nodeTypeRep of
                                    Nothing -> error 
                                       "DisplayView: unmatched node type tag"
                                    Just nodeType -> nodeType
                              graphNode <- 
                                 newNode graph nodeType 
                                    (nodeTitlePrim object,link)
                              mVar <- newMVar False
                              sinkID <- newSinkID
                              return (Just (graphNode,mVar,sinkID),
                                 (graphNode,True))
                     )
               doFocus <- if considerFocus 
                  then
                     mustFocus' link
                  else
                     return False
               if doFocus
                  then
                     focusLinkInner displayedView displayedObjectType link 
                        graphNode
                  else
                     done
               return (Just graphNode)

---
-- focusLink is the version of focusLinkInner to be used externally.  It
-- also extracts the node type and graph node.
focusLink :: 
   (GraphAllConfig graph graphParms node nodeType nodeTypeParms 
      arc arcType arcTypeParms,ObjectType objectType object)
   => (DisplayedView graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms) 
   -> Link object -> IO ()
focusLink       
      (displayedView@DisplayedView{graph=graph,view=view,nodes=nodes,
            allDisplayedObjectTypes = AllDisplayedObjectTypes nodeTypeRegister
            } 
         :: DisplayedView graph graphParms node nodeType nodeTypeParms arc 
            arcType arcTypeParms) 
      (link :: Link object) =
   do
      let
         wrappedLink = WrappedLink link
      wrappedObject <- wrapReadLink view wrappedLink
      let
         wrappedObjectType = getObjectType wrappedObject
      (Just displayedObjectType) <- getValueOpt nodeTypeRegister 
         (Keyed wrappedObjectType)

      Just ((graphNode,_,_) :: NodeData node object)
         <- getValueOpt nodes (Keyed wrappedLink)

      focusLinkInner displayedView displayedObjectType link graphNode
      redraw graph


---
-- focusLinkInner focusses a particular node, if that hasn't already been done.
focusLinkInner :: 
   (GraphAllConfig graph graphParms node nodeType nodeTypeParms 
      arc arcType arcTypeParms,ObjectType objectType object)
   => (DisplayedView graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms) 
   -> DisplayedObjectType objectType object nodeType arcType
   -> Link object -> node (String,Link object) -> IO ()
focusLinkInner       
      (displayedView@DisplayedView{view=view,graph=graph,nodes=nodes,
         allDisplayedObjectTypes=AllDisplayedObjectTypes nodeTypesRegistry} 
         :: DisplayedView graph graphParms node nodeType nodeTypeParms arc 
            arcType arcTypeParms) 
      (DisplayedObjectType {focus' = focus',arcTypes' = arcTypes'})
      link node =
   do
      nodeInfoOpt <- getValueOpt nodes (Keyed (WrappedLink link))
      (mVar,sinkID) <- case nodeInfoOpt of
         Just ((_,mVar,sinkID) :: NodeData node object)
            -> return (mVar,sinkID)
      nodeFocussed <- takeMVar mVar
      putMVar mVar True
      if nodeFocussed 
         then
            done
         else
            do
               (arcsFrom,arcsTo) <- focus' link

               let
                  -- processArcs is the generic function for arcsFrom or
                  -- arcsTo.  The second argument is the function which
                  -- adds an arc.
                  processArcs :: VariableSetSource (WrappedLink,ArcType)
                     -> (forall value . Typeable value =>
                           arcType () -> node value 
                           -> IO (arc ()))
                     -> IO ()
                  processArcs setSource addArc =
                     do
                        (arcsRegistry :: Registry (Keyed WrappedLink,ArcType) 
                              (arc ())) <-
                           newRegistry

                        arcsBSem <- newBSem
                        let
                           handleArc :: VariableSetUpdate (WrappedLink,ArcType)
                              -> IO ()
                           handleArc (AddElement 
                                 (wrappedLink2 @ (WrappedLink link2),
                                    arcTypeRep)) =
                              do
                                 node2opt <- displayNodeUnWrapped 
                                    displayedView link2 
                                 case node2opt of
                                    Nothing -> done
                                    Just node2 ->
                                       do
                                          let
                                             graphArcType = lookupWithDefaultFM
                                                arcTypes'
                                                (error "DisplayView: \ 
                                                   \undefined ArcType")
                                                arcTypeRep
                                          graphArc <- addArc graphArcType node2
                                          transformValue arcsRegistry 
                                             (Keyed wrappedLink2,arcTypeRep)
                                             (\ arcOpt -> case arcOpt of
                                                Nothing -> 
                                                   return (Just graphArc,())
                                                Just arc -> error 
                                                   "DisplayView: \ 
                                                      \duplicate arcs"
                                                ) 
                           handleArc (DelElement (wrappedLink2,arcTypeRep)) =
                              do
                                 arcOpt <- transformValue arcsRegistry
                                    (Keyed wrappedLink2,arcTypeRep)
                                    (\ arcOpt -> return (Nothing,arcOpt))
                                 case arcOpt of
                                    Nothing -> done
                                       -- arc never inserted or already deleted
                                    Just (arc :: arc ()) -> deleteArc graph arc

                           -- handleArcSafe delays until the arcsBSem can be
                           -- obtained; this prevents it clashing with the 
                           -- initial setup.
                           handleArcSafe update = 
                              do
                                 acquire arcsBSem
                                 release arcsBSem
                                 handleArc update
                                 redraw graph

                        acquire arcsBSem
                        (currentArcs,sink) 
                           <- addNewSinkGeneral setSource handleArcSafe sinkID
                        mapM_
                           (\ arc -> handleArc (AddElement arc))
                           currentArcs
                        release arcsBSem
                        redraw graph
                        addCloseDownAction displayedView (invalidate sink)
               processArcs arcsFrom 
                  (\ arcType node2 -> newArc graph arcType () node node2) 
               processArcs arcsTo 
                  (\ arcType node2 -> newArc graph arcType () node2 node) 

-- ------------------------------------------------------------------------    
-- Close down actions for a DisplayedView
-- ------------------------------------------------------------------------

newCloseDownActions :: IO (MVar [IO ()])
newCloseDownActions = newMVar []

---
-- Add an action to be done when the view is closed.
addCloseDownAction :: 
   DisplayedView graph graphParms node nodeType nodeTypeParms arc arcType 
      arcTypeParms
   -> IO () -> IO ()
addCloseDownAction displayedView action =
   do
      let mVar = closeDownActions displayedView
      list <- takeMVar mVar
      putMVar mVar (action : list)

---
-- Do all such actions.
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

