{- This module contains the code for tracing and drawing objects.
   The main thing it has to do is keep track of what objects have
   already been drawn, and which are no longer accessible. -}
module LinkDrawer(
   LinkDrawer, -- instance of Destroyable.

   newLinkDrawer, 
      -- :: Ord node 
      -- => VariableSet node -- ^ Set of root nodes.
      -- -> (node -> IO (NodeData node arc pos))
      -- -> IO (LinkDrawer node arc pos)

   NodeData(..),
      -- information the user needs to provide about nodes
   ArcData,
      -- information the user needs to provide about arcs
      -- instance of Eq and Ord

   toArcData, -- :: node -> arc -> Bool -> ArcData node arc
      -- making an ArcData

   destination, -- :: ArcData node arc -> node
      -- get an ArcData's destination.
   ) where

import Concurrent
import IOExts

import Object
import Computation
import Registry
import Sink
import Sources
import ReferenceCount
import ExtendedPrelude

import VariableSet
import VariableList

import Destructible

-- -----------------------------------------------------------------
-- The types
-- -----------------------------------------------------------------

data NodeData node nodeArg arc pos = NodeData {
   nodeArg :: nodeArg, 
      -- ^ Extra data the caller provides which is passed
      -- back to the arcUpdate function.
   outArcs :: VariableList (ArcData node arc),
      -- ^ Arcs out of this node
   innerListDrawer :: ListDrawer (nodeArg,arc) pos,
      -- ^ Function provided for physically drawing arcs. 
   deleteNode :: IO ()
      -- ^ physically delete the node.
      -- NB.  We don't do this unless all arcs either to or from
      -- this node have been deleted.
   }

data ArcData node arc = ArcData {
   destination :: node,
   arcInfo :: arc,
   mustFollow :: Bool
   } deriving (Eq,Ord)


-- | The following invariants are maintained, except when we are in the
-- middle of doing an update:
-- 
-- Arcs which are known about (returned by outArcs or inArcs) have either 
-- mustFollow=True or mustFollow=False.
-- 
-- Nodes which are known about (at the end of an arc which is known about,
-- or in the root set are either displayed or not displayed.
-- 
-- The nodes which are displayed are precisely those in the root set, or those
-- which are the destination of some current arc with mustFollow=True.
-- 
-- For nodes which are \"displayed\" this means (a) we have called the 
-- user-supplied updateFn to get the NodeData for that node; 
-- (b) we have called the arcUpdate function to draw the arcs from it.
-- 
-- For a node which never has been displayed, neither of these will be true.
-- It follows from the above rules that a node ceases to be displayed
-- when it ceases to be in either the root set, or the destination of an
-- arc with mustFollow=True.  When this happens, we call the node\'s deleteNode
-- function, delete all arcs from the node, and forget about the outArcs\/inArcs
-- for the node (which may provoke further deletions).
-- 

-- | Here is the LinkDrawer itself.
data LinkDrawer node nodeArg arc pos = LinkDrawer {
   allNodes :: Registry node (NodeRecord node nodeArg arc pos),
      -- ^ Gives all information about nodes referenced so far, but not
      -- deleted
   updateFn :: node -> IO (NodeData node nodeArg arc pos),
      -- ^ The updateFn passed to newLinkDrawer
   globalSinkID :: SinkID,
      -- ^ SinkID to be used for the whole LinkDrawer.
   parallelX :: ParallelExec
   }

data NodeRecord node nodeArg arc pos = NodeRecord {
   ifDisplayed :: Maybe (NodeDisplayed node nodeArg arc pos),
      -- If the node is displayed, NodeDisplayed data for it.
   optionalArcs :: [(pos,ArcData node arc,node)]
      -- All arcs pointing to this node with mustFollow = False.
   }

-- | describes information about a node already displayed
data NodeDisplayed node nodeArg arc pos = 
   NodeDisplayed {
      nodeData :: NodeData node nodeArg arc pos,
      listDrawer :: ListDrawer (ArcData node arc) pos,
      dependentArcs :: [(pos,ArcData node arc)],
         -- ^ All arcs which are referenced from this node, whether or not
         -- they have been drawn.
      referenceCount :: RefCount,
      stopUpdatesIORef :: IORef (IO ())
         -- ^ Contains action to be performed when node is deleted to
         -- stop the arcs being updated.
      }
      
-- -----------------------------------------------------------------
-- The functions
-- -----------------------------------------------------------------

-- | Create an ArcData
toArcData :: node -> arc -> Bool -> ArcData node arc
toArcData destination arcInfo mustFollow = ArcData {
   destination = destination,arcInfo = arcInfo,mustFollow = mustFollow
   }

-- | Create a link drawer 
newLinkDrawer :: (Ord pos,Ord node) 
  => VariableSetSource node -- ^ Set of root nodes.
  -> (node -> IO (NodeData node nodeArg arc pos))
   -> IO (LinkDrawer node nodeArg arc pos)
newLinkDrawer 
      (roots :: VariableSetSource node) 
      (updateFn :: node -> IO (NodeData node nodeArg arc pos)) =
   do
      -- Constructing the link drawer is the easy bit; the hard bit is
      -- setting up the functions to update the various data structures.
      -- We construct the link drawer first.
      (allNodes :: Registry node (NodeRecord node nodeArg arc pos))
         <- newRegistry
      globalSinkID <- newSinkID
      parallelX <- newParallelExec

      let
         linkDrawer = LinkDrawer {
            allNodes = allNodes,
            updateFn = updateFn,
            globalSinkID = globalSinkID,
            parallelX = parallelX
            }

         -- add a particular node if necessary, and retrieve its nodeArg.
         addNode :: node -> IO nodeArg
         addNode thisNode =
            do
               -- We try to do as much as possible when allNodes is not locked,
               -- since that means we can look at allNodes again.  Thus we 
               -- return an action, to be done afterwards.
               afterAct <- transformValue allNodes thisNode (\ nodeRecordOpt 
                  -> case nodeRecordOpt of
                     Just (NodeRecord {ifDisplayed = Just nodeDisplayed}
                        :: NodeRecord node nodeArg arc pos) ->
                        do
                           addRef (referenceCount nodeDisplayed)
                           return (nodeRecordOpt,
                              return (nodeArg (nodeData nodeDisplayed)))
                     _ ->
                        do
                           -- The node needs to be constructed.
                           (nodeData @ NodeData {nodeArg = thisNodeArg,
                              outArcs = outArcs,
                              innerListDrawer = innerListDrawer,
                              deleteNode = deleteNode}) <-
                              updateFn thisNode

                           referenceCount <- newLinkedRefCount
                           stopUpdatesIORef <- newIORef (error "LinkDrawer.3")

                           let
                              listDrawer = mapListDrawer thisNode thisNodeArg
                                 innerListDrawer

                              nodeDisplayed = NodeDisplayed {
                                 nodeData = nodeData,
                                 listDrawer = listDrawer,
                                 dependentArcs = [],
                                 referenceCount = referenceCount,
                                 stopUpdatesIORef = stopUpdatesIORef
                                 }

                              optionalArcs0 = case nodeRecordOpt of
                                 Just nodeRecord -> optionalArcs nodeRecord
                                 Nothing -> []

                              nodeRecord = NodeRecord {
                                 ifDisplayed = Just nodeDisplayed,
                                 optionalArcs = optionalArcs0
                                 }

                              afterAct =
                                 do
                                    -- (1) draw optional arcs
                                    mapM_
                                       (\ (pos,arcData,parent) ->
                                          do
                                             innerListDrawerOpt <-
                                                getInnerListDrawerOpt parent
                                             case innerListDrawerOpt of
                                                Nothing -> done
                                                Just innerListDrawer ->
                                                   setPos innerListDrawer pos
                                                      (Just (thisNodeArg,
                                                         arcInfo arcData))
                                          )
                                       optionalArcs0

                                    -- (2) arrange for this node's arcs to be
                                    -- displayed.
                                    stopUpdatesAction <- attachListOp 
                                       outArcs parallelX listDrawer
                                    writeIORef stopUpdatesIORef 
                                       stopUpdatesAction

                                    -- (3) return
                                    return thisNodeArg

                           return (Just nodeRecord,afterAct)
                  )

               afterAct
               -- end of addNode

         ---
         -- Decrement a node's reference counter and if necessary delete it.
         unAddNode :: node -> IO ()
         unAddNode thisNode =
            do
               refCount <- getRefCount thisNode
               doDelete <- remRef refCount
               if doDelete
                  then
                     do
                        (dependentArcs,optionalArcs,stopUpdatesIORef,
                           deleteNode,thisInnerListDrawer) 
                              <- transformValue allNodes thisNode 
                              (\ nodeRecordOpt -> case nodeRecordOpt of
                                 Just (NodeRecord {
                                    optionalArcs = optionalArcs,
                                    ifDisplayed = Just (NodeDisplayed {
                                       dependentArcs = dependentArcs,
                                       stopUpdatesIORef = stopUpdatesIORef,
                                       nodeData = NodeData {
                                          deleteNode = deleteNode,
                                          innerListDrawer 
                                             = thisInnerListDrawer
                                          }
                                       })
                                    } :: NodeRecord node nodeArg arc pos) 
                                    -> return (
                                    Nothing,
                                    (dependentArcs,optionalArcs,
                                       stopUpdatesIORef,deleteNode,
                                       thisInnerListDrawer)
                                    )
                                 )
                        -- (1) stop updates
                        stopUpdates <- readIORef stopUpdatesIORef
                        stopUpdates

                        -- (2) physically remove optional arcs, and remove
                        -- them from their parents' dependentArcs list.
                        mapM_ 
                           (\ (pos,arcData,parent) ->
                              if parent == thisNode -- we have a loop
                                 then
                                    delPos thisInnerListDrawer pos
                                 else
                                    do
                                       innerListDrawerOpt 
                                          <- getInnerListDrawerOpt parent
                                       case innerListDrawerOpt of
                                          Just innerListDrawer ->
                                             do
                                                delPos innerListDrawer pos
                                                remDependentArc pos parent
                                                done
                                          Nothing -> done
                                             -- arc is not currently drawn,
                                             -- because parent isn't.
                              )
                           optionalArcs

                        -- (3) physically remove dependent arcs and create 
                        --     action for unAdding the targets for mustFollow
                        --     arcs.  We will do that 
                        --     action at the end, so that the data structures
                        --     are consistent.
                        unAddActions <- mapM
                           (\ (pos,arcData) ->
                              do
                                 delPos thisInnerListDrawer pos

                                 if mustFollow arcData
                                    then
                                       return (unAddNode (
                                          destination arcData))
                                    else
                                       do
                                          remOptionalArc pos arcData
                                          return done
                              )
                           dependentArcs

                        -- (4) physically delete node
                        deleteNode

                        -- (5) do unAddActions
                        sequence_ unAddActions  
                  else
                     done

         -- Map the list drawer supplied to the user for a particular node to
         -- one which we feed to the VariableList for the user.
         mapListDrawer :: node -> nodeArg 
            -> ListDrawer (nodeArg,arc) pos
            -> ListDrawer (ArcData node arc) pos
         mapListDrawer thisNode thisNodeArg (ListDrawer {
               newPos = newPos0,setPos = setPos0,delPos = delPos0,
                  redraw = redraw0}) =
            let
               newPos1 :: Maybe pos -> Maybe (ArcData node arc) -> IO pos
               newPos1 posOpt Nothing = newPos0 posOpt Nothing
               newPos1 posOpt (Just arcData) =
                  do
                     pos <- if mustFollow arcData 
                        then
                           do
                              nodeArg <- addNode (destination arcData)
                              newPos0 posOpt 
                                 (Just (nodeArg,arcInfo arcData))
                        else
                           do
                              nodeArgOpt 
                                 <- getNodeArgOpt (destination arcData)
                              pos <- newPos0 posOpt
                                 (fmap
                                    (\ nodeArg -> (nodeArg,arcInfo arcData))
                                    nodeArgOpt
                                    )
                              -- Add pos to the child's optionalArcs.
                              addOptionalArc pos arcData thisNode
                              return pos
                     -- add pos to this node's dependentArcs.
                     addDependentArc pos arcData thisNode
                     return pos

               setPos1 = error "LinkDrawer : I didn't think I'd need setPos!"

               delPos1 :: pos -> IO ()
               delPos1 pos =
                  do
                     -- Remove this arc's dependent arcs.  If the arc is
                     -- a mustFollow, delink the destination (which may in turn
                     -- cause other deletions), otherwise delete from the 
                     -- optional arcs
                     arcDataOpt <- remDependentArc pos thisNode
                     case arcDataOpt of
                        Nothing -> done
                        Just arcData ->
                           do
                              if mustFollow arcData
                                 then
                                    unAddNode (destination arcData)
                                 else
                                    remOptionalArc pos arcData

                              -- Physically remove the arc
                              delPos0 pos

               redraw1 :: IO ()
               redraw1 = redraw0
            in
               ListDrawer {
                  newPos = newPos1,setPos = setPos1,delPos = delPos1,
                     redraw = redraw0
                  }


         addOptionalArc :: pos -> ArcData node arc -> node -> IO ()
         addOptionalArc pos arcData parent =
            transformValue allNodes (destination arcData) (\ nodeRecordOpt ->
               case nodeRecordOpt of
                  Nothing -> -- create node record too
                     let
                        nodeRecord = NodeRecord {
                           ifDisplayed = Nothing,
                           optionalArcs = [optionalArcData]
                           }
                     in
                        return (Just nodeRecord,())
                  Just (nodeRecord0 :: NodeRecord node nodeArg arc pos) ->
                     let
                        nodeRecord1 = nodeRecord0 {
                           optionalArcs = optionalArcData 
                              : (optionalArcs nodeRecord0)
                           }
                     in
                        return (Just nodeRecord1,())
               )
            where
               optionalArcData = (pos,arcData,parent)

         remOptionalArc :: pos -> ArcData node arc -> IO ()
         remOptionalArc pos0 arcData =
            transformValue allNodes (destination arcData) (\ nodeRecordOpt ->
               case nodeRecordOpt of
                  Just (nodeRecord0 :: NodeRecord node nodeArg arc pos) ->
                     let
                        optionalArcs1 =
                           deleteFirst 
                              (\ (pos1,_,_) -> pos1 == pos0)
                              (optionalArcs nodeRecord0)

                        nodeRecord1 = nodeRecord0 {
                           optionalArcs = optionalArcs1
                           }
                     in
                        case (optionalArcs1,ifDisplayed nodeRecord0) of
                           ([],Nothing) -> -- can forget this node
                              return (Nothing,())
                           _ -> return (Just nodeRecord0,())
               )   
                      
         addDependentArc :: pos -> ArcData node arc -> node -> IO ()
         addDependentArc pos arcData parent =
            transformValue allNodes parent (\ nodeRecordOpt -> case 
                  nodeRecordOpt of
               Just (nodeRecord0 :: NodeRecord node nodeArg arc pos) ->
                  case ifDisplayed nodeRecord0 of
                     Just nodeDisplayed0 ->
                        let
                           dependentArcs1 = 
                              (pos,arcData)
                              : (dependentArcs nodeDisplayed0)

                           nodeDisplayed1 = nodeDisplayed0 {
                              dependentArcs = dependentArcs1
                              }

                           nodeRecord1 = nodeRecord0 {
                              ifDisplayed = Just nodeDisplayed1
                              }
                        in
                           return (Just nodeRecord1,())
               )

         -- remDependentArc removes the arc and also returns its ArcData.
         -- if used on a non-existent arc (for example, one already deleted)
         -- it returns Nothing.
         remDependentArc :: pos -> node -> IO (Maybe (ArcData node arc))
         remDependentArc pos parent =
            transformValue allNodes parent (\ nodeRecordOpt -> case
                  nodeRecordOpt of
               Just (nodeRecord0 :: NodeRecord node nodeArg arc pos) ->
                  case ifDisplayed nodeRecord0 of
                     Just nodeDisplayed0 ->
                        return (
                           case
                              deleteAndFindFirstOpt
                                 (\ (pos1,_) -> pos1 == pos)
                                 (dependentArcs nodeDisplayed0)
                                 of
                              Just ((pos1,arcData1),dependentArcs1) ->
                                 let
                                    nodeDisplayed1 = nodeDisplayed0 {
                                       dependentArcs = dependentArcs1
                                       }

                                    nodeRecord1 = nodeRecord0 {
                                       ifDisplayed = Just nodeDisplayed1
                                       }
                                 in 
                                     (Just nodeRecord1,Just arcData1)
                              Nothing ->
                                 (Just nodeRecord0,Nothing)
                           )
                     Nothing -> return (Just nodeRecord0,Nothing)
               Nothing -> return (Nothing,Nothing)
               )

         getNodeArg :: node -> IO nodeArg
         getNodeArg node =
            do
               (Just nodeArg) <- getNodeArgOpt node
               return nodeArg

         -- getNodeArgOpt returns nodeArg if the node is displayed, otherwise 
         -- Nothing.
         getNodeArgOpt :: node -> IO (Maybe nodeArg)
         getNodeArgOpt node =
            do
               nodeDisplayedOpt <- getNodeDisplayedOpt node
               return (fmap (nodeArg . nodeData) nodeDisplayedOpt)

         -- getRefCount returns a node's reference count.
         getRefCount :: node -> IO RefCount
         getRefCount node =
            do
               (Just nodeDisplayed) <- getNodeDisplayedOpt node
               return (referenceCount nodeDisplayed)

         -- getListDrawerOpt gets a node's list drawer, if it has one.
         getListDrawerOpt :: node 
            -> IO (Maybe (ListDrawer (ArcData node arc) pos))
         getListDrawerOpt node =
            do
               nodeDisplayedOpt <- getNodeDisplayedOpt node
               return (fmap
                  listDrawer 
                  nodeDisplayedOpt
                  )

         -- getInnerListDrawerOpt gets a node's inner list drawer, if it has 
         -- one.
         getInnerListDrawerOpt :: node 
            -> IO (Maybe (ListDrawer (nodeArg,arc) pos))
         getInnerListDrawerOpt node  =
            do
               nodeDisplayedOpt <- getNodeDisplayedOpt node
               return (fmap
                  (innerListDrawer . nodeData) 
                  nodeDisplayedOpt
                  )

         -- Extract a node's nodeDisplayed, if any
         getNodeDisplayedOpt :: node 
            -> IO (Maybe (NodeDisplayed node nodeArg arc pos))
         getNodeDisplayedOpt node =
            do
               nodeRecordOpt <- getNodeRecordOpt node
               return (case nodeRecordOpt of
                  Just nodeRecord -> ifDisplayed nodeRecord
                  Nothing -> Nothing
                  )

         -- Extract a node's nodeRecord, if any
         getNodeRecordOpt :: node 
            -> IO (Maybe (NodeRecord node nodeArg arc pos))
         getNodeRecordOpt node = getValueOpt allNodes node
 

      -- Attach addNode and unAddNode to roots.
      let
         updateFn :: VariableSetUpdate node -> IO ()
         updateFn (AddElement node) = 
            do
               addNode node
               done
         updateFn (DelElement node) = unAddNode node
      (mVar :: MVar [node]) <- newEmptyMVar 
         -- this will contain the initial nodes
      let
         doInitialNodes =
            do
               nodes <- takeMVar mVar
               mapM_ addNode nodes

      -- This ensures that the initial nodes are added before anything else
      -- is done.
      parallelExec parallelX doInitialNodes

      (initialNodes,_) 
         <- addNewSinkVeryGeneral roots updateFn globalSinkID parallelX
      putMVar mVar initialNodes
      return linkDrawer

-- -----------------------------------------------------------------------     
-- Instance of Destroyable
-- -----------------------------------------------------------------------     

instance (Ord node,Ord pos) 
   => Destroyable (LinkDrawer node nodeArc arc pos) where
   destroy linkDrawer =
      -- We only stop updates happening; we don't bother to destroy the
      -- physical structure.
      do
         invalidate (globalSinkID linkDrawer)
         parallelExec (parallelX linkDrawer) (
            do
               allNodePairs <- listRegistryContents (allNodes linkDrawer)
               mapM_
                  (\ (node,nodeRecord) ->
                     case nodeRecord of
                        NodeRecord {ifDisplayed = Just nodeDisplayed} ->
                           do
                              stopUpdates 
                                 <- readIORef (stopUpdatesIORef nodeDisplayed) 
                              stopUpdates
                        _ -> done
                     )  
                  allNodePairs
            )
