-- | This is the module that writes a BundleNode into the repository.
-- It is assumed that the bundle is the output of 
-- MMiSSBundleDissect.dissectBundle and that the bundle has passed
-- the MMiSSBundleValidate.validateBundle function and 
-- MMiSSBundleNodeCheckTypes.checkBundleNodeTypes.
module MMiSSBundleNodeWrite(
      writeBundleNode,
      -- :: View -> InsertionPoint -> BundleNode -> IO ()
   ) where

import Data.FiniteMap
import Control.Monad.State
import Control.Monad.Trans

import Computation
import Thread

import EntityNames

import View
import Link
import LinkManager
import ObjectTypes

import MMiSSInsertionPoint
import MMiSSBundle
import MMiSSSplitLink
import MMiSSBundleSimpleUtils
import MMiSSImportExportErrors
import MMiSSBundleNodeWriteClass
import MMiSSFileType
import MMiSSPackageFolder
import MMiSSBundleNodeWriteObject
import {-# SOURCE #-} MMiSSObjectTypeInstance 

-- -------------------------------------------------------------------------
-- Datatypes
-- -------------------------------------------------------------------------

-- Information we need to collect about nodes preliminary to writing them.
data PreWriteNodeData = PreWriteNodeData {
   node :: BundleNode,
   thisLink :: SplitLink, -- ^ Where to put this node
   parentLink :: Maybe (SplitLink,EntityName)
       -- ^ If set, we should make this child a parent of the given node,
       -- with the given name
       -- after all the nodes have been written.
   }

newtype PreWriteData = PreWriteData {
   preWriteFM :: FiniteMap [EntityName] PreWriteNodeData
   }

-- -------------------------------------------------------------------------
-- Creating the PreWriteData
-- -------------------------------------------------------------------------

mkPreWriteData :: View -> InsertionPoint -> BundleNode -> IO PreWriteData
mkPreWriteData view insertionPoint bundleNode =
   do
      -- The top node needs special handling.  We cannot for example
      -- assume that it has a parent; we may be importing directly
      -- into the top node.
      (preWriteNodeData0,ancestorInfo0,nameOpt) <- case insertionPoint of
         Right (folderLink,name) ->
            do
               -- This object is new and should be written to this folder.
               thisLink <- newEmptySplitLink view (WrappedLink folderLink) (
                  base . objectType . fileLoc $ bundleNode)

               let
                  preWriteNodeData0 = PreWriteNodeData {
                     node = bundleNode,
                     thisLink = thisLink,
                     parentLink = Just (FolderC folderLink,name)
                     }
                  ancestorInfo0 = AncestorInfo {
                     names = [],
                     parent = thisLink,
                     parentExists = False
                     }
               return (preWriteNodeData0,ancestorInfo0,Just name)
         Left linkedObject ->
            let
               thisLink = splitLinkedObject linkedObject

               preWriteNodeData0 = PreWriteNodeData {
                  node = bundleNode,
                  thisLink = thisLink,
                  parentLink = Nothing
                  }
               ancestorInfo0 = AncestorInfo {
                  names = [],
                  parent = thisLink,
                  parentExists = True
                  }
            in
               return (preWriteNodeData0,ancestorInfo0,Nothing)

      let
         preWriteData0 = PreWriteData {
            preWriteFM =listToFM [([],preWriteNodeData0)]
            }

         bundleFoldFn :: AncestorInfo -> BundleNode 
            -> PreWriteMonad AncestorInfo
         bundleFoldFn ancestorInfo0 bundleNode0 =
            do
               let
                  thisName = alwaysNameFileLoc (fileLoc bundleNode0)

                  createEmpty =
                     do
                        thisLink <- newEmptySplitLink view 
                           (wrapSplitLink (parent ancestorInfo0))
                           (base . objectType . fileLoc $ bundleNode0)
                        return (thisLink,False)

                  mkThisLinkExist :: IO (SplitLink,Bool)
                  mkThisLinkExist = if parentExists ancestorInfo0
                     then
                        -- we need to special-case preambles, which can't be
                        -- got from the object's contents by name, unlike the
                        -- other objects
                        case base . objectType . fileLoc $ bundleNode0 of
                           MMiSSPreambleEnum ->
                              do
                                 let
                                    MMiSSPackageFolderC packageFolderLink =
                                       parent ancestorInfo0
                                 packageFolder 
                                    <- readLink view packageFolderLink
                                 return (
                                    MMiSSPreambleC (
                                       toMMiSSPreambleLink packageFolder),
                                    True)
                           _ ->
                              do
                                 parentLinkedObject <- readSplitLink view
                                    (parent ancestorInfo0)

                                 thisLinkedObjectOpt <- lookupNameInFolder 
                                    parentLinkedObject thisName
                                 case thisLinkedObjectOpt of
                                    Nothing -> createEmpty
                                    Just thisLinkedObject -> 
                                       return (
                                          splitLinkedObject thisLinkedObject,
                                          True
                                           )
                        else
                           createEmpty

               (thisLink,thisExists) <- lift mkThisLinkExist

               let
                  preWriteNodeData = PreWriteNodeData {
                     node = bundleNode0,
                     thisLink = thisLink,
                     parentLink = 
                        if thisExists 
                           then 
                              Nothing
                           else
                              Just (parent ancestorInfo0,thisName) 
                     }

                  names1 = thisName : names ancestorInfo0

               preWriteData0 <- get
               put (PreWriteData {
                  preWriteFM =
                     addToFM (preWriteFM preWriteData0) names1 
                        preWriteNodeData
                  })

               let
                  ancestorInfo1 = AncestorInfo {
                     names = names1,
                     parent = thisLink,
                     parentExists = thisExists
                     }
               return ancestorInfo1

      preWriteData1 <- execStateT
         (bundleNodeFoldM1 bundleFoldFn ancestorInfo0 bundleNode)
         preWriteData0

      return preWriteData1

         
   
-- This is the information passed down by bundleFoldM.   
data AncestorInfo = AncestorInfo {
   names :: [EntityName],
   parent :: SplitLink, 
   parentExists :: Bool
   }

type PreWriteMonad = StateT PreWriteData IO


-- -------------------------------------------------------------------------
-- Writing the PreWriteData
-- -------------------------------------------------------------------------

writePreWriteData :: View -> PreWriteData -> IO ()
writePreWriteData view preWriteData =
   do
      let
         bnl = toBundleNodeLocations preWriteData

      postActions <- mapMConcurrentExcep
         (\ (names,preWriteNodeData) ->
            let
               do1 :: HasBundleNodeWrite object => Link object -> IO (IO ())
               do1 link =
                  bundleNodeWrite1 view bnl names (node preWriteNodeData) link
            in
               case thisLink preWriteNodeData of
                  FolderC link -> do1 link
                  FileC link -> do1 link
                  MMiSSPackageFolderC link -> do1 link
                  MMiSSObjectC link -> do1 link
                  MMiSSPreambleC link -> do1 link
                  MMiSSFileC link -> do1 link
            )
         (fmToList (preWriteFM preWriteData))

      -- Connect everything
      makeConnections view preWriteData

      -- do the post actions
      sequence_ postActions

toBundleNodeLocations :: PreWriteData -> BundleNodeLocations
toBundleNodeLocations preWriteData =
   BundleNodeLocations (
      mapFM
         (\ _ preWriteNodeData -> BundleNodeExtraData {
            location = wrapSplitLink (thisLink preWriteNodeData)})
         (preWriteFM preWriteData)
      )

-- -------------------------------------------------------------------------
-- Setting up the topology
-- -------------------------------------------------------------------------

makeConnections :: View -> PreWriteData -> IO ()
   -- The following subtleties should be noted.
   -- (1) we don't connect preambles at all.  (It's stored elsewhere
   -- in the package folder.
   -- (2) we connect the objects in reverse depth-first order.  This
   -- makes it easier for the link drawer, which doesn't see new nodes until
   -- all their children have been created.
makeConnections view preWriteData =
   do
      let
         preWriteFM1 = preWriteFM preWriteData

         connect :: PreWriteNodeData -> IO ()
         connect nodeData =
            case (base . objectType . fileLoc . node $ nodeData,
                    parentLink nodeData) of
               (MMiSSPreambleEnum,_) -> done
               (_,Nothing) -> done
               (_,Just (parentLink1,name1)) ->
                  do
                     parentLinkedObject <- readSplitLink view parentLink1
                     thisLinkedObject <- readSplitLink view (thisLink nodeData)
                     resultWE <- moveObject thisLinkedObject 
                        (Just (mkInsertion parentLinkedObject name1))
                     coerceImportExportIO resultWE

         Just (PreWriteNodeData {node = headNode}) =
            lookupFM preWriteFM1 []

      mapM_
         (\ (locInfo,_) ->
            do
               let
                  Just preWriteNodeData1 =
                     lookupFM preWriteFM1 (packagePath locInfo)
               connect preWriteNodeData1
            )
         (getAllNodes1 (Bundle [(error "Bad package id",headNode)]))
 
-- -------------------------------------------------------------------------
-- writeBundleNode
-- -------------------------------------------------------------------------

writeBundleNode :: View -> InsertionPoint -> BundleNode -> IO ()
writeBundleNode view insertionPoint bundleNode =
   do
      preWriteData <- mkPreWriteData view insertionPoint bundleNode
      writePreWriteData view preWriteData


