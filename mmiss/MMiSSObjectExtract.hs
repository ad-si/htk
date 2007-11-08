-- | This module contains extractMMiSSObject, which extracts an object from
-- the repository, reassembling it with its component objects, and
-- converts it to a given format.
module MMiSSObjectExtract(
   extractMMiSSObject,
      -- :: View -> Link MMiSSObject -> Format -> IO (WithError String)

   extractMMiSSObject1,
      -- :: View -> Link MMiSSObject -> Maybe MMiSSVariantSearch -> ExportOpts
      -- -> IO (WithError (String,ExportFiles))
   ) where

import Computation
import IntPlus

import View
import Link

import MMiSSObjectType
import MMiSSFormat
import MMiSSVariant
import MMiSSEditFormatConverter
import MMiSSImportExportErrors
import MMiSSBundle
import {-# SOURCE #-} MMiSSReadObject
import {-# SOURCE #-} MMiSSExportFiles

extractMMiSSObject :: View -> Link MMiSSObject -> Format
   -> IO (WithError (String,ExportFiles))
extractMMiSSObject view link format1 =
   extractMMiSSObject1 view True link Nothing
      (ExportOpts {getText = True,format = format1,recurseDepth = infinity})

extractMMiSSObject1 ::
   View
   -> Bool -- ^ if True include \documentclass header.
   -> Link MMiSSObject -> Maybe MMiSSVariantSearch -> ExportOpts
   -> IO (WithError (String,ExportFiles))
extractMMiSSObject1 view includeHeader link maybeVariant exportOpts =
   do
      if getText exportOpts
         then
            done
         else
            importExportError "Unexpected getText=False in extractMMiSSObject1"
      extractedWE <-
         readMMiSSObject view link maybeVariant (recurseDepth exportOpts) True
      case fromWithError extractedWE of
         Left mess -> return (hasError mess)
         Right (element,packageFolders,exportFiles0) ->
            do
               strWE <-
                  exportElement1 view (format exportOpts) includeHeader
                     packageFolders element
               return (mapWithError
                  (\ str -> (str,exportFiles0))
                  strWE
                  )
