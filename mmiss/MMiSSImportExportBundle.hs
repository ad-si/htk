{- This module contains the top-level functions for importing and exporting
   bundles. -}
module MMiSSImportExportBundle(
   importBundle,
      -- :: View -> LinkedObject -> Bundle -> IO ()
      -- This corresponds to mmiss/MMiSSRequest.dtd's putObject element.
   exportBundle, 
      -- :: View -> LinkedObject -> ExportOpts -> Maybe MMiSSVariantSearch
      -- -> IO Bundle
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
import MMiSSExportVariantBundle
import MMiSSExportEntireBundle

-- --------------------------------------------------------------------------
-- Importing
-- --------------------------------------------------------------------------

-- | This corresponds to mmiss/MMiSSRequest.dtd's putObject element.
importBundle :: View -> LinkedObject -> Bundle -> IO ()
importBundle = error "TBD"

-- --------------------------------------------------------------------------
-- Exporting
-- --------------------------------------------------------------------------

-- | This corresponds to mmiss/MMiSSRequest.dtd's getObject element.
exportBundle :: View -> LinkedObject -> ExportOpts 
   -> Maybe MMiSSVariantSearch -> IO Bundle
exportBundle view linkedObject exportOpts variantSearchOpt =
   case variantSearchOpt of
      Just variantSearch ->
         exportLinkedObjectVariant view linkedObject exportOpts variantSearch
      Nothing ->
         exportEntireLinkedObject view linkedObject exportOpts
