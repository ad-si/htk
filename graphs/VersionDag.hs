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
   toDisplayedGraph,
   ) where

import Data.FiniteMap
import Control.Concurrent.MVar

import Sources
import Broadcaster

import Graph
import PureGraph
import SimpleGraph(ClientData(..))
import NewNames
import PureGraphPrune
import PureGraphToGraph

-- --------------------------------------------------------------------------
-- Data types
-- --------------------------------------------------------------------------

data VersionDag nodeKey nodeInfo arcInfo = VersionDag {
   stateBroadcaster :: SimpleBroadcaster (
      VersionDagState nodeKey nodeInfo arcInfo),
   toNodeKey :: nodeInfo -> nodeKey,
   toParents :: nodeInfo -> [(arcInfo,nodeKey)]
   }

data VersionDagState nodeKey nodeInfo arcInfo = VersionDagState {
   inPureGraph :: PureGraph nodeKey arcInfo,
   nodeInfoDict :: FiniteMap nodeKey nodeInfo,
   isHidden :: nodeInfo -> Bool
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
         state = VersionDagState {
            inPureGraph = emptyPureGraph,
            nodeInfoDict = emptyFM,
            isHidden = isHidden0
            }

      stateBroadcaster <- newSimpleBroadcaster state

      return (VersionDag {
         stateBroadcaster = stateBroadcaster,
         toNodeKey = toNodeKey0,
         toParents = toParents0
         })

-- --------------------------------------------------------------------------
-- Modifications
-- --------------------------------------------------------------------------

addVersion :: (Ord nodeKey,Ord arcInfo)
   => VersionDag nodeKey nodeInfo arcInfo -> nodeInfo -> IO ()
addVersion versionDag nodeInfo = addVersions versionDag [nodeInfo]

addVersions :: (Ord nodeKey,Ord arcInfo)
   => VersionDag nodeKey nodeInfo arcInfo -> [nodeInfo] -> IO ()
addVersions versionDag nodeInfos =
   applySimpleUpdate (stateBroadcaster versionDag) 
      (\ state0 ->
         let
            inPureGraph0 = inPureGraph state0
            inPureGraph1 = foldl
               (\ pg0 nodeInfo -> 
                  addNode pg0 (toNodeKey versionDag nodeInfo) 
                     (toParents versionDag nodeInfo)
                  )
               inPureGraph0 
               nodeInfos
          
            nodeInfoDict0 = nodeInfoDict state0

            nodeInfoDict1 = 
               addListToFM
                  nodeInfoDict0
                  (map
                     (\ nodeInfo -> (toNodeKey versionDag nodeInfo,nodeInfo))
                     nodeInfos
                     )
            state1 = state0 {
               inPureGraph = inPureGraph1,
               nodeInfoDict = nodeInfoDict1
               }
         in
            state1
         )

-- | Change the nodeInfo of something already added.
setNodeInfo :: (Ord nodeKey,Ord arcInfo) 
   => VersionDag nodeKey nodeInfo arcInfo -> nodeInfo -> IO ()
setNodeInfo = addVersion
          

-- | Change the hidden function
changeIsHidden :: (Ord nodeKey,Ord arcInfo)
   => VersionDag nodeKey nodeInfo arcInfo 
   -> (nodeInfo -> Bool) -> IO ()
changeIsHidden versionDag isHidden1 =
   applySimpleUpdate (stateBroadcaster versionDag)
      (\ state0 -> state0 {isHidden = isHidden1})

-- --------------------------------------------------------------------------
-- Getting the pruned graph out
-- --------------------------------------------------------------------------

toDisplayedGraph :: (Ord nodeKey,Ord arcInfo)
   => VersionDag nodeKey nodeInfo arcInfo 
   -> GraphConnection (nodeInfo,Bool) () (Maybe arcInfo) ()
toDisplayedGraph (versionDag :: VersionDag nodeKey nodeInfo arcInfo) =
   let
      transform :: VersionDagState nodeKey nodeInfo arcInfo 
         -> (PureGraph nodeKey (Maybe arcInfo),nodeKey -> (nodeInfo,Bool))
      transform state =
         let
            toNodeInfo :: nodeKey -> nodeInfo
            toNodeInfo nodeKey = 
               lookupWithDefaultFM
                  (nodeInfoDict state)
                  (error "VersionDag: nodeKey encountered with no nodeInfo")
                  nodeKey

            isHidden0 :: nodeInfo -> Bool
            isHidden0 = isHidden state

            isHidden1 :: nodeKey -> Bool
            isHidden1 = isHidden0 . toNodeInfo

            toNodeInfo1 :: nodeKey -> (nodeInfo,Bool)
            toNodeInfo1 nodeKey =
               let
                  nodeInfo = toNodeInfo nodeKey
               in
                  (nodeInfo,isHidden0 nodeInfo)

            outPureGraph :: PureGraph nodeKey (Maybe arcInfo)
            outPureGraph = pureGraphPrune isHidden1 (inPureGraph state)
         in
            (outPureGraph,toNodeInfo1)
   in
      pureGraphToGraph
         (fmap transform (toSimpleSource (stateBroadcaster versionDag)))