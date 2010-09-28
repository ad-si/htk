-- | This module contains the code for exporting an MMiSS object or file
-- with a particular variant.
module MMiSS.ExportVariantBundle(
   exportLinkedObjectVariant,
      -- :: View -> LinkedObject -> ExportOpts -> MMiSSVariantSearch
      -- -> IO Bundle
   ) where

import Util.Messages
import Util.Computation
import Util.AtomString
import Util.ExtendedPrelude(uniqOrd)
import Util.Sources(readContents)

import Imports.EntityNames(EntityFullName)

import Types.View
import Types.LinkManager
import Types.Link

import MMiSS.ImportExportErrors
import MMiSS.ObjectExtract
import MMiSS.Variant
import MMiSS.Bundle
import MMiSS.BundleTypes
import MMiSS.SplitLink
import MMiSS.ObjectType
import MMiSS.FileType
import MMiSS.BundleUtils
import MMiSS.BundleSimpleUtils
import MMiSS.BundleValidate

import MMiSS.ExportFiles
import MMiSS.PackageFolder

-- --------------------------------------------------------------------------
-- Splitting
-- --------------------------------------------------------------------------

exportLinkedObjectVariant
   :: View -> LinkedObject -> ExportOpts -> MMiSSVariantSearch -> IO Bundle
exportLinkedObjectVariant view linkedObject exportOpts variantSearch =
   do
      bundle <-
         case splitLinkedObject linkedObject of
            MMiSSFileC fileLink ->
               exportMMiSSFileVariant view fileLink exportOpts variantSearch
            MMiSSObjectC objectLink ->
               exportMMiSSObjectVariant view
                  objectLink exportOpts variantSearch
            _ ->
               do
                  packageId <- mkPackageId view linkedObject
                  errorMess ("Unable to extract " ++ packageIdStr packageId)
                  bundleNode <- getUnknownBundleNode view linkedObject
                  return (Bundle [(packageId,bundleNode)])
      validateBundleOut bundle
      return bundle


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
                  extractedWE <- extractMMiSSObject1 view True
                        mmissObjectLink (Just variantSearch) exportOpts

                  (objectString :: String,exportFiles :: ExportFiles)
                     <- coerceImportExportIO extractedWE
                  let
                     bundleText = BundleString {
                        contents = fromString objectString,
                        charType = Unicode
                        }

                     bundleNodeData = Object [(Just emptyMMiSSVariantSpec,
                        bundleText)]
                        -- The real variants will be encoded
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
