-- | This module contains the top-level functions for importing and exporting
-- bundles. 
module MMiSSImportExportBundle(
   importBundle,
      -- :: View -> LinkedObject -> Bundle -> IO ()
      -- This corresponds to mmiss/MMiSSRequest.dtd's putObject element.
   exportBundle, 
      -- :: View -> LinkedObject -> ExportOpts -> Maybe MMiSSVariantSearch
      -- -> IO Bundle
      -- This corresponds to mmiss/MMiSSRequest.dtd's getObject element.
   ) where

import EntityNames

import View
import LinkManager

import MMiSSBundle
import MMiSSVariant
import MMiSSExportVariantBundle
import MMiSSExportEntireBundle
import MMiSSBundleWrite
import MMiSSInsertionPoint
import MMiSSImportExportErrors

-- --------------------------------------------------------------------------
-- Importing
-- --------------------------------------------------------------------------

-- | This corresponds to mmiss\/MMiSSRequest.dtd\'s putObject element.
importBundle :: View -> EntityFullName -> Maybe PackageId -> Bundle -> IO ()
importBundle view fullName packageIdOpt bundle =
   do
      insertionPointWE <- getInsertionPoint view fullName 
      insertionPoint <- coerceImportExportIO insertionPointWE
      writeBundle bundle packageIdOpt Nothing view insertionPoint

-- --------------------------------------------------------------------------
-- Exporting
-- --------------------------------------------------------------------------

-- | This corresponds to mmiss\/MMiSSRequest.dtd\'s getObject element.
exportBundle :: View -> LinkedObject -> ExportOpts 
   -> Maybe MMiSSVariantSearch -> IO Bundle
exportBundle view linkedObject exportOpts variantSearchOpt =
   case variantSearchOpt of
      Just variantSearch ->
         exportLinkedObjectVariant view linkedObject exportOpts variantSearch
      Nothing ->
         exportEntireLinkedObject view linkedObject exportOpts
