{- This module contains the code for exporting an MMiSS object or file
   with a particular variant. -}
module MMiSSExportVariantBundle(
   exportLinkedObjectVariant, 
      -- :: View -> LinkedObject -> ExportOpts -> MMiSSVariantSearch 
      -- -> IO Bundle
   ) where

import Text.XML.HaXml.Types

import Messages
import Computation
import AtomString
import ExtendedPrelude(uniqOrd)
import Sources(readContents)

import EntityNames(EntityFullName)

import View
import LinkManager
import Link
import Folders

import LaTeXParser

import MMiSSPreamble
import MMiSSImportExportErrors
import MMiSSObjectExtract
import MMiSSVariant
import MMiSSBundle
import MMiSSBundleTypes
import MMiSSSplitLink
import MMiSSObjectType
import MMiSSFileType
import MMiSSBundleUtils
import MMiSSBundleSimpleUtils

import {-# SOURCE #-} MMiSSExportFiles
import {-# SOURCE #-} MMiSSReadObject
import MMiSSPackageFolder

-- --------------------------------------------------------------------------
-- Splitting
-- --------------------------------------------------------------------------

exportLinkedObjectVariant 
   :: View -> LinkedObject -> ExportOpts -> MMiSSVariantSearch -> IO Bundle
exportLinkedObjectVariant view linkedObject exportOpts variantSearch =
   do
      packageId <- mkPackageId view linkedObject
      let
         wrapBundleNode bundleNode =
            do
               packageId <- mkPackageId view linkedObject
               return (Bundle [(packageId,bundleNode)])
      case splitLinkedObject linkedObject of
         MMiSSFileC fileLink -> 
            exportMMiSSFileVariant view fileLink exportOpts variantSearch
         MMiSSObjectC objectLink ->
            exportMMiSSObjectVariant view objectLink exportOpts variantSearch
         _ ->     
            do
               errorMess ("Unable to extract " ++ packageIdStr packageId)
               bundleNode <- getUnknownBundleNode view linkedObject
               wrapBundleNode bundleNode


-- --------------------------------------------------------------------------
-- Exporting an MMiSSObject
-- --------------------------------------------------------------------------


exportMMiSSObjectVariant :: View -> Link MMiSSObject -> ExportOpts 
   -> MMiSSVariantSearch -> IO Bundle
exportMMiSSObjectVariant view mmissObjectLink exportOpts variantSearch =
   do
      (elementBundleNodeData :: BundleNodeData,exportFiles :: ExportFiles) <-
         if getText exportOpts
            then
               do
                  extractedWE <- extractMMiSSObject1 view mmissObjectLink 
                        (Just variantSearch) exportOpts

                  (objectString :: String,exportFiles :: ExportFiles)
                     <- coerceImportExportIO extractedWE
                  let
                     bundleText = BundleString {
                        contents = fromString objectString,
                        charType = Unicode
                        }

                     bundleNodeData = Object [(Nothing,bundleText)]
                        -- we don't supply variants, which will be encoded
                        -- in the objectString anyway.
                  return (bundleNodeData,exportFiles)
            else
               return (NoData,[])
                  -- not clear why anyone would want this, but we support it
                  -- for reasons of consistency.
            
      mmissObject <- readLink view mmissObjectLink
      let
         mmissObjectType = mmissObjectAsFileType (format exportOpts)

      elementBundle <- wrapContainingMMiSSPackage2 
         view mmissObject mmissObjectType elementBundleNodeData

      (containedFiles0 :: [[(Link MMiSSFile,MMiSSVariantSearch)]]) <-
         mapM
            (\ (packageFolder,fullName,variantSearch) ->
               do
                  (foundFiles :: [(Link MMiSSFile,EntityFullName,String)])
                     <- findMMiSSFilesInRepository packageFolder fullName
                  return (map
                     (\ (link,_,_) -> (link,variantSearch))
                     foundFiles
                     )
               )
            exportFiles

      let
         containedFiles1 :: [(Link MMiSSFile,MMiSSVariantSearch)]
         containedFiles1 = uniqOrd (concat containedFiles0)

      (fileBundles :: [Bundle]) <-
         mapM
            (\ (fileLink,variantSearch) ->
               exportMMiSSFileVariant view fileLink exportOpts variantSearch
               )
            containedFiles1

      let
         completeBundleWE = mergeBundles (elementBundle : fileBundles)
      coerceImportExportIO completeBundleWE

        


 


-- --------------------------------------------------------------------------
-- Exporting an MMiSSFile
-- --------------------------------------------------------------------------

exportMMiSSFileVariant :: View -> Link MMiSSFile -> ExportOpts
   -> MMiSSVariantSearch -> IO Bundle
exportMMiSSFileVariant view link exportOpts variantSearch = 
   do
      mmissFile <- readLink view link
      fileLoc1 <- getFileLoc view (toLinkedObject mmissFile)
      bundleNodeData1 <- getBundleNodeDataForVariant view mmissFile exportOpts
         variantSearch

      wrapContainingMMiSSPackage2 view mmissFile (objectType fileLoc1) 
         bundleNodeData1

-- --------------------------------------------------------------------------
-- Utility Functions
-- --------------------------------------------------------------------------

wrapContainingMMiSSPackage2 :: HasLinkedObject object 
   => View -> object -> BundleType -> BundleNodeData -> IO Bundle
wrapContainingMMiSSPackage2 view object bundleType bundleNodeData =
   do
      packageFolderAndNameWE <- getMMiSSPackageFolderAndName 
         view object
      (packageFolder,name) <- coerceImportExportIO packageFolderAndNameWE
      packageId <- mkPackageId view (toLinkedObject packageFolder)

      packageFolderNameOpt <- readContents (getLinkedObjectTitleOpt (
         toLinkedObject packageFolder))
      let
         bundleNodeWE = wrapContainingMMiSSPackage packageFolderNameOpt name 
            bundleType bundleNodeData
      bundleNode <- coerceImportExportIO bundleNodeWE
      return (Bundle [(packageId,bundleNode)])