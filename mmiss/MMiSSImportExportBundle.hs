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

import View
import LinkManager

import MMiSSBundle
import MMiSSVariant

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
exportBundle :: View -> LinkedObject -> ExportOpts -> Maybe MMiSSVariantSearch
   -> IO Bundle
exportBundle = error "TBD"    
