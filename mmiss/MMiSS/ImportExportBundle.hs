-- | This module contains the top-level functions for importing and exporting
-- bundles.
module MMiSS.ImportExportBundle(
   importBundle,
      -- :: View -> LinkedObject -> Bundle -> IO ()
      -- This corresponds to mmiss/MMiSSRequest.dtd's putObject element.
   exportBundle,
      -- :: View -> LinkedObject -> ExportOpts -> Maybe MMiSSVariantSearch
      -- -> IO Bundle
      -- This corresponds to mmiss/MMiSSRequest.dtd's getObject element.
   ) where

import Imports.EntityNames

import Types.View
import Types.LinkManager

import MMiSS.Bundle
import MMiSS.Variant
import MMiSS.ExportVariantBundle
import MMiSS.ExportEntireBundle
import MMiSS.BundleWrite
import MMiSS.InsertionPoint
import MMiSS.ImportExportErrors

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
