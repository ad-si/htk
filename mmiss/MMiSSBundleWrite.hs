{- This module puts together all the bundle-filling-in, validation and
   writing functions, to present the general function for writing a bundle.
   -}
module MMiSSBundleWrite(
   writeBundle,
   writeBundle1,
   ) where

import Control.Exception

import IO
import List
import Maybe

import Delayer

import EntityNames

import View

import LaTeXParser(PackageId(..))

import MMiSSInsertionPoint
import MMiSSBundle
import MMiSSBundleFillIn
import MMiSSImportExportErrors
import MMiSSBundleDissect
import MMiSSBundleValidate
import MMiSSBundleReadFiles
import MMiSSBundleNodeCheckTypes
import MMiSSBundleNodeWrite
import MMiSSEditLocks
import MMiSSBundleNodeEditLocks

-- | Errors are provoked by importExportError, and can be caught that way.
writeBundle :: 
   Bundle 
      -- ^ the bundle
   -> Maybe PackageId 
      -- ^ which node in the bundle to write.  If unset we take the first one.
   -> Maybe FilePath
      -- ^ where to look for additional files.  If unspecified, we don't. 
   -> View  
      -- ^ which view to write the node to
   -> InsertionPoint 
   -> IO ()
writeBundle bundle packageIdOpt filePathOpt view insertionPoint =
   writeBundle1 bundle packageIdOpt filePathOpt view emptyLockSet 
      insertionPoint

-- | Errors are provoked by importExportError, and can be caught that way.
writeBundle1 :: 
   Bundle 
      -- ^ the bundle
   -> Maybe PackageId 
      -- ^ which node in the bundle to write.  If unset we take the first one.
   -> Maybe FilePath
      -- ^ where to look for additional files.  If unspecified, we don't. 
   -> View  
      -- ^ which view to write the node to
   -> LockSet
      -- ^ Locks we have already acquired for variants we can write again.
   -> InsertionPoint 
   -> IO ()
writeBundle1 (Bundle []) _ _ _ _ _ = 
   importExportError "Attempt to write empty bundle"
writeBundle1 (bundle0 @ (Bundle ((packageId0,_):_)))
      packageIdOpt filePathOpt view lockSet insertionPoint =
   delay view (
      do
         let
            packageId = fromMaybe packageId0 packageIdOpt

         bundle1 <- fillInBundle (toInsertionPointName insertionPoint) bundle0
         coerceImportExportIO (validateBundle bundle1)
         let
            bundle2WE = dissectBundle bundle1
         bundle2 <- coerceImportExportIO bundle2WE
         bundle3 <- case filePathOpt of
            Nothing -> return bundle2
            Just filePath -> readFiles filePath bundle2

         let
            Bundle packageBundleNodes = bundle3

         bundleNode <- case List.lookup packageId packageBundleNodes of
            Just bundleNode -> return bundleNode
            Nothing -> 
               importExportError ("PackageId " ++ packageIdStr packageId
                  ++ " not found")

         checkTypesWE <- checkBundleNodeTypes view insertionPoint bundleNode
         coerceImportExportIO checkTypesWE

         releaseActWE <- acquireBundleNodeEditLocks view lockSet 
            insertionPoint bundleNode
         releaseAct <- coerceImportExportIO releaseActWE

         finally
            (writeBundleNode view insertionPoint bundleNode)
            releaseAct
      )

       
            
