{- This module contains the top-level functions for importing and exporting
   bundles. -}
module MMiSSImportExportBundleNode(
   importBundleNode,
      -- :: View -> LinkedObject -> BundleNode -> IO ()
      -- This corresponds to mmiss/MMiSSRequest.dtd's putObject element.
   exportBundleNode, 
      -- :: View -> LinkedObject -> ExportOpts -> Maybe MMiSSVariantSearch
      -- -> IO BundleNode
      -- This corresponds to mmiss/MMiSSRequest.dtd's getObject element.
   ) where

import Messages
import Computation

import View
import LinkManager
import Folders

import MMiSSSplitLink
import MMiSSBundle
import MMiSSVariant
import MMiSSExportVariantBundleNode
import MMiSSExportEntireBundleNode

-- --------------------------------------------------------------------------
-- Importing
-- --------------------------------------------------------------------------

-- | This corresponds to mmiss/MMiSSRequest.dtd's putObject element.
importBundleNode :: View -> LinkedObject -> BundleNode -> IO ()
importBundleNode = error "TBD"

-- --------------------------------------------------------------------------
-- Exporting
-- --------------------------------------------------------------------------

-- | This corresponds to mmiss/MMiSSRequest.dtd's getObject element.
exportBundleNode :: View -> LinkedObject -> ExportOpts 
   -> Maybe MMiSSVariantSearch -> IO BundleNode
exportBundleNode view linkedObject exportOpts variantSearchOpt =
   case variantSearchOpt of
      Just variantSearch ->
         exportLinkedObjectVariant view linkedObject exportOpts variantSearch
      Nothing ->
         exportEntireLinkedObject view linkedObject exportOpts
