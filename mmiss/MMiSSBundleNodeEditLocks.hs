-- | acquireBundleNodeEditLocks acquires all the edit locks required for a 
-- BundleNode, returning the action to release them if successful. 
-- 
-- It assumes checkBundleNodeTypes has already successfully returned. 
module MMiSSBundleNodeEditLocks(
   acquireBundleNodeEditLocks,
   ) where

import Monad

import Computation

import View
import Link

import LinkManager

import MMiSSEditLocks
import MMiSSInsertionPoint
import MMiSSBundle
import MMiSSBundleSimpleUtils
import {-# SOURCE #-} MMiSSObjectTypeInstance

acquireBundleNodeEditLocks :: View -> LockSet -> InsertionPoint -> BundleNode 
   -> IO (WithError (IO ()))
acquireBundleNodeEditLocks view lockSet insertionPoint bundleNode =
   do
      (lockLocations :: [LockLocation]) <-
         case insertionPoint of
            Right folder -> return []
               -- nodes do not exist.
            Left linkedObject ->
               getLockLocations view (Just linkedObject) bundleNode

      acquireResult <- acquireMultipleLocks view lockSet lockLocations
      return (fmap
         (\ (act,_) -> act)
         acquireResult
         )

getLockLocations 
   :: View -> Maybe LinkedObject -> BundleNode -> IO [LockLocation]
getLockLocations view Nothing node = return []
getLockLocations view (Just linkedObject) node =
   case (bundleNodeData node,base . objectType . fileLoc $ node) of
      (Dir bundleNodes,_) ->
         do
            let
               getSubNodes :: BundleNode -> IO [LockLocation]
               getSubNodes bundleNode1 =
                  case fromWithError (nameFileLocOpt (fileLoc bundleNode1)) of
                     Right (Just name) ->
                        do
                           linkedObjectOpt 
                              <- lookupNameInFolder linkedObject name
                           getLockLocations view linkedObjectOpt node
                     _ -> return []

            (lockLocations1 :: [[LockLocation]]) 
               <- mapM getSubNodes bundleNodes
            return (concat lockLocations1)
      (Object variants,MMiSSObjectEnum) ->
         do
            let
               wrappedLink = toWrappedLink linkedObject
               Just objectLink = unpackWrappedLinkToMMiSSObject wrappedLink
            mmissObject <- readLink view objectLink
            return (map
               (\ (Just variantSpec,_) -> (mmissObject,variantSpec))
               variants
               )
      _ -> return []