{- This module contains the code for exporting an MMiSS object or file
   with a particular variant. -}
module MMiSSExportVariantBundleNode(
   exportLinkedObjectVariant, 
      -- :: View -> LinkedObject -> ExportOpts -> MMiSSVariantSearch 
      -- -> IO BundleNode
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

-- --------------------------------------------------------------------------
-- Splitting
-- --------------------------------------------------------------------------

exportLinkedObjectVariant 
   :: View -> LinkedObject -> ExportOpts -> MMiSSVariantSearch -> IO BundleNode
exportLinkedObjectVariant view linkedObject exportOpts variantSearch =
   case splitLinkedObject linkedObject of
      MMiSSObjectC objectLink
         -> exportMMiSSObjectVariant view objectLink exportOpts variantSearch
      MMiSSFileC fileLink
         -> exportMMiSSFileVariant view fileLink exportOpts variantSearch
      _ ->
         do
            linkedObjectStr <- describeLinkedObject view linkedObject
            errorMess ("Unable to extract " ++ linkedObjectStr)
            bundleNode <- getUnknownBundleNode linkedObject
            return bundleNode


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
exportMMiSSFileVariant = error "TBD"