{- This module contains the code for exporting an MMiSS object or file
   with a particular variant. -}
module MMiSSExportVariantBundle(
   exportLinkedObjectVariant, 
      -- :: View -> LinkedObject -> ExportOpts -> MMiSSVariantSearch 
      -- -> IO Bundle
   ) where

import Messages

import View
import LinkManager
import Link
import Folders

import MMiSSVariant
import MMiSSBundle
import MMiSSSplitLink
import MMiSSObjectType
import MMiSSFileType
import MMiSSBundleUtils

-- --------------------------------------------------------------------------
-- Splitting
-- --------------------------------------------------------------------------

exportLinkedObjectVariant 
   :: View -> LinkedObject -> ExportOpts -> MMiSSVariantSearch -> IO Bundle
exportLinkedObjectVariant view linkedObject exportOpts variantSearch =
   do
      packageId <- mkPackageId view linkedObject
      bundleNode <-
         case splitLinkedObject linkedObject of
            MMiSSObjectC objectLink
               -> exportMMiSSObjectVariant view objectLink exportOpts variantSearch
            MMiSSFileC fileLink
               -> exportMMiSSFileVariant view fileLink exportOpts variantSearch
            _ ->
               do
                  errorMess ("Unable to extract " ++ packageIdStr packageId)
                  getUnknownBundleNode view linkedObject
      return (Bundle [(packageId,bundleNode) ])
      


-- --------------------------------------------------------------------------
-- Exporting an MMiSSObject
-- --------------------------------------------------------------------------

exportMMiSSObjectVariant :: View -> Link MMiSSObject -> ExportOpts 
   -> MMiSSVariantSearch -> IO BundleNode
exportMMiSSObjectVariant = error "TBD"

-- --------------------------------------------------------------------------
-- Exporting an MMiSSFile
-- --------------------------------------------------------------------------

exportMMiSSFileVariant :: View -> Link MMiSSFile -> ExportOpts
   -> MMiSSVariantSearch -> IO BundleNode
exportMMiSSFileVariant view link exportOpts variantSearch = 
   do
      mmissFile <- readLink view link
      fileLoc1 <- getFileLoc view (toLinkedObject mmissFile)
      bundleNodeData1 <- getBundleNodeDataForVariant view mmissFile exportOpts
         variantSearch

      return (BundleNode {
         fileLoc = fileLoc1,
         bundleNodeData = bundleNodeData1
         })
