-- | This module implements a VersionDag, a graph which is used for
-- displaying versions within the Workbench.
--
-- The main differences between this and normal 'SimpleGraph.SimpleGraph''s 
-- are that
--   (1) the parents of a node are fixed when it is created, as are
--       all arc labels and arc type labels.
--   (2) it is possible to selectively "hide" nodes from being displayed.
--       We intelligently display the structure between these nodes.
--   (3) it is not permitted to delete a node with children.  (Though it
--       may be hidden.)
module VersionDag(
   newVersionDag,
   addVersion,
   addVersions,
   setNodeInfo,
   changeIsHidden,
   ) where

import Data.FiniteMap
import Control.Concurrent.MVar

import Graph
import PureGraph
import SimpleGraph(ClientData(..))

-- --------------------------------------------------------------------------
-- Data types
-- --------------------------------------------------------------------------

newtype VersionDag nodeKey nodeInfo arcInfo 
   = VersionDag (MVar (VersionDagState nodeKey nodeInfo arcInfo))

data VersionDagState nodeKey nodeInfo arcInfo = VersionDagState {
   inPureGraph :: PureGraph nodeKey arcInfo,
   outPureGraph :: PureGraph nodeKey (Maybe arcInfo),
   nodeInfoDict :: FiniteMap nodeKey nodeInfo,
   isHidden :: nodeInfo -> Bool,
   toNodeKey :: nodeInfo -> nodeKey,
   toParents :: nodeInfo -> [(arcInfo,nodeKey)],
   clients :: [ClientData nodeInfo () (Maybe arcInfo) ()]
   }

-- --------------------------------------------------------------------------
-- Create 
-- --------------------------------------------------------------------------

newVersionDag :: (Ord nodeKey,Ord arcInfo)
   => (nodeInfo -> Bool) 
   -> (nodeInfo -> nodeKey) 
   -> (nodeInfo -> [(arcInfo,nodeKey)]) 
   -> IO (VersionDag nodeKey nodeInfo arcInfo)
newVersionDag isHidden0 toNodeKey0 toParents0 =
   do
      let
         versionDagState = VersionDagState {
            inPureGraph = emptyPureGraph,
            outPureGraph = emptyPureGraph,
            nodeInfoDict = emptyFM,
            isHidden = isHidden0,
            toNodeKey = toNodeKey0,
            toParents = toParents0,
            clients = []
            }

      mVar <- newMVar versionDagState
      return (VersionDag mVar)

-- --------------------------------------------------------------------------
-- Modifications
-- --------------------------------------------------------------------------


addVersion :: (Ord nodeKey,Ord arcInfo)
   => VersionDag nodeKey nodeInfo arcInfo -> nodeInfo -> IO ()
addVersion versionDag nodeInfo = addVersions versionDag [nodeInfo]

addVersions :: (Ord nodeKey,Ord arcInfo)
   => VersionDag nodeKey nodeInfo arcInfo -> [nodeInfo] -> IO ()
addVersions 
      (VersionDag mVar :: VersionDag nodeKey nodeInfo arcInfo) 
      nodeInfos =
   modifyMVar_ mVar (\ state0 ->
      let
         inPureGraph0 = inPureGraph state0
         inPureGraph1 = foldl
            (\ pg0 nodeInfo -> 
               addNode pg0 (toNodeKey state0 nodeInfo) 
                  (toParents state0 nodeInfo)
               )
            inPureGraph0 
            nodeInfos

         nodeInfoDict0 = nodeInfoDict state0
         nodeInfoDict1 = 
            addListToFM
               nodeInfoDict0
               (map
                  (\ nodeInfo -> (toNodeKey state0 nodeInfo,nodeInfo))
                  nodeInfos
                  )
         state1 = state0 {
            inPureGraph = inPureGraph1,
            nodeInfoDict = nodeInfoDict1
            }
      in
         recomputeGraph state1
      )

-- | Change the nodeInfo of something already added.
setNodeInfo :: (Ord nodeKey,Ord arcInfo) 
   => VersionDag nodeKey nodeInfo arcInfo -> nodeInfo -> IO ()
setNodeInfo = addVersion
          

-- | Change the hidden function
changeIsHidden :: (Ord nodeKey,Ord arcInfo)
   => VersionDag nodeKey nodeInfo arcInfo 
   -> (nodeInfo -> Bool) -> IO ()
changeIsHidden (VersionDag mVar) isHidden1 =
   modifyMVar_ mVar (\ state0 ->
      let
         state1 = state0 {isHidden = isHidden1}
      in
         recomputeGraph state1
      )

-- --------------------------------------------------------------------------
-- Broadcasting
-- --------------------------------------------------------------------------

-- | Send the complete contents of a graph to a new client and
-- update the clients field.
addNewClient :: Ord nodeKey
   => VersionDagState nodeKey nodeInfo arcInfo
   -> ClientData nodeInfo () (Maybe arcInfo) ()
   -> IO (VersionDagState nodeKey nodeInfo arcInfo)
addNewClient = error "aNC"

-- | Given an old VersionDagState in which either inPureGraph, nodeInfoDict
-- or isHidden has changed, broadcast the changes
-- to the clients, update the fields, and return the new VersionDagState.
recomputeGraph :: (Ord nodeKey,Ord arcInfo)
   => VersionDagState nodeKey nodeInfo arcInfo 
   -> IO (VersionDagState nodeKey nodeInfo arcInfo)
recomputeGraph = error "RR"


