{-# LANGUAGE ScopedTypeVariables #-}

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
module Graphs.VersionDag(
   VersionDag,

   newVersionDag,
   addVersion,
   addVersions,
   deleteVersion,
   setNodeInfo,
   changeIsHidden,
   toDisplayedGraph,
   toInputGraph,
   getInputGraphBack,
   nodeKeyExists,
   lookupNodeKey,
   getNodeInfos,
   ) where

import Data.Maybe

import qualified Data.Map as Map

import Util.Sources
import Util.Broadcaster

import Graphs.Graph
import Graphs.PureGraph
import Graphs.FindCommonParents
import Graphs.PureGraphPrune
import Graphs.PureGraphToGraph
import Graphs.PureGraphMakeConsistent

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
   nodeInfoDict :: Map.Map nodeKey nodeInfo,
   isHidden :: nodeInfo -> Bool
   }

-- --------------------------------------------------------------------------
-- Create
-- --------------------------------------------------------------------------

newVersionDag :: (Ord nodeKey,Ord arcInfo,Eq nodeInfo)
   => (nodeInfo -> Bool)
   -> (nodeInfo -> nodeKey)
   -> (nodeInfo -> [(arcInfo,nodeKey)])
   -> IO (VersionDag nodeKey nodeInfo arcInfo)
newVersionDag isHidden0 toNodeKey0 toParents0 =
   do
      let
         state = VersionDagState {
            inPureGraph = emptyPureGraph,
            nodeInfoDict = Map.empty,
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

addVersion :: (Ord nodeKey,Ord arcInfo,Eq nodeInfo)
   => VersionDag nodeKey nodeInfo arcInfo -> nodeInfo -> IO ()
addVersion versionDag nodeInfo = addVersions versionDag [nodeInfo]

addVersions :: (Ord nodeKey,Ord arcInfo,Eq nodeInfo)
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
               foldr (uncurry Map.insert)
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

deleteVersion :: Ord nodeKey
   => VersionDag nodeKey nodeInfo arcInfo -> nodeKey -> IO ()
deleteVersion versionDag nodeKey =
   applySimpleUpdate (stateBroadcaster versionDag)
      (\ state0 ->
         let
            inPureGraph0 = inPureGraph state0
            inPureGraph1 = deleteNode inPureGraph0 nodeKey

            nodeInfoDict0 = nodeInfoDict state0

            nodeInfoDict1 = Map.delete nodeKey nodeInfoDict0
            state1 = state0 {
               inPureGraph = inPureGraph1,
               nodeInfoDict = nodeInfoDict1
               }
         in
            state1
         )


-- | Change the nodeInfo of something already added.
setNodeInfo :: (Ord nodeKey,Ord arcInfo,Eq nodeInfo)
   => VersionDag nodeKey nodeInfo arcInfo -> nodeInfo -> IO ()
setNodeInfo = addVersion


-- | Change the hidden function
changeIsHidden :: (Ord nodeKey,Ord arcInfo,Eq nodeInfo)
   => VersionDag nodeKey nodeInfo arcInfo
   -> (nodeInfo -> Bool) -> IO ()
changeIsHidden versionDag isHidden1 =
   applySimpleUpdate (stateBroadcaster versionDag)
      (\ state0 -> state0 {isHidden = isHidden1})

-- --------------------------------------------------------------------------
-- Queries
-- --------------------------------------------------------------------------

nodeKeyExists :: Ord nodeKey
   => VersionDag nodeKey nodeInfo arcInfo -> nodeKey -> IO Bool
nodeKeyExists versionDag nodeKey =
   do
      nodeInfoOpt <- lookupNodeKey versionDag nodeKey
      return (isJust nodeInfoOpt)

lookupNodeKey :: Ord nodeKey
   => VersionDag nodeKey nodeInfo arcInfo -> nodeKey -> IO (Maybe nodeInfo)
lookupNodeKey versionDag nodeKey =
   do
      state <- readContents (stateBroadcaster versionDag)
      return (Map.lookup nodeKey (nodeInfoDict state))

getNodeInfos :: Ord nodeKey
   => VersionDag nodeKey nodeInfo arcInfo -> IO [nodeInfo]
getNodeInfos versionDag =
   do
      state <- readContents (stateBroadcaster versionDag)
      return (Map.elems (nodeInfoDict state))


-- --------------------------------------------------------------------------
-- Getting the pruned graph out
-- --------------------------------------------------------------------------

toDisplayedGraph :: (Ord nodeKey,Ord arcInfo,Eq nodeInfo)
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
               Map.findWithDefault
                  (error "VersionDag: nodeKey encountered with no nodeInfo")
                  nodeKey
                  (nodeInfoDict state)

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

            inPureGraph0 = inPureGraph state
            inPureGraph1 = pureGraphMakeConsistent inPureGraph0

            outPureGraph :: PureGraph nodeKey (Maybe arcInfo)
            outPureGraph = pureGraphPrune isHidden1 inPureGraph1
         in
            (outPureGraph,toNodeInfo1)
   in
      pureGraphToGraph
         (fmap transform (toSimpleSource (stateBroadcaster versionDag)))

-- --------------------------------------------------------------------------
-- Getting the input graph out
-- --------------------------------------------------------------------------


-- | Get the input graph in the form of FindCommonParents.GraphBack.
-- NB.
-- (1) the confusion in the type variable "nodeKey" as used in
--     FindCommonParents is not the same as our "nodeKey".
-- (2) we get a snapshot of the state of the input graph at a particular
--     time
getInputGraphBack :: (Ord nodeKey,Ord arcInfo,Eq nodeInfo)
   => VersionDag nodeKey nodeInfo arcInfo
   -> (nodeKey -> nodeInfo -> graphBackNodeKey)
   -> IO (GraphBack nodeKey graphBackNodeKey)
getInputGraphBack
      (versionDag :: VersionDag nodeKey nodeInfo arcInfo)
      (toGraphBackNodeKey :: nodeKey -> nodeInfo -> graphBackNodeKey) =
   do
      state <- readContents (stateBroadcaster versionDag)
      let
         inPureGraph0 :: PureGraph nodeKey arcInfo
         inPureGraph0 = inPureGraph state

         nodeInfoDict0 :: Map.Map nodeKey nodeInfo
         nodeInfoDict0 = nodeInfoDict state

         getAllNodes :: [nodeKey]
         getAllNodes = toAllNodes inPureGraph0

         getKey :: nodeKey -> Maybe graphBackNodeKey
         getKey nodeKey =
            do
               nodeInfo <- Map.lookup nodeKey nodeInfoDict0
               return (toGraphBackNodeKey nodeKey nodeInfo)

         getParents :: nodeKey -> Maybe [nodeKey]
         getParents = toNodeParents inPureGraph0

      return (GraphBack {
         getAllNodes = getAllNodes,
         getKey = getKey,
         getParents = getParents
         })


toInputGraph :: (Ord nodeKey,Ord arcInfo,Eq nodeInfo)
   => VersionDag nodeKey nodeInfo arcInfo
   -> GraphConnection nodeInfo () arcInfo ()
toInputGraph (versionDag :: VersionDag nodeKey nodeInfo arcInfo) =
   let
      transform :: VersionDagState nodeKey nodeInfo arcInfo
         -> (PureGraph nodeKey arcInfo,nodeKey -> nodeInfo)
      transform state =
         let
            toNodeInfo :: nodeKey -> nodeInfo
            toNodeInfo nodeKey =
               Map.findWithDefault
                  (error "VersionDag: nodeKey encountered with no nodeInfo")
                  nodeKey
                  (nodeInfoDict state)
         in
            (inPureGraph state,toNodeInfo)
   in
      pureGraphToGraph (fmap transform (
         toSimpleSource (stateBroadcaster versionDag)))
