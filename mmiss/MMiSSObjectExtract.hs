{- This module contains extractMMiSSObject, which extracts an object from
   the repository, reassembling it with its component objects, and
   converts it to a given format. -}
module MMiSSObjectExtract(
   extractMMiSSObject,
      -- :: View -> Link MMiSSObject -> Format -> IO (WithError String)
   ) where

import Computation
import IntPlus

import View
import Link

import MMiSSObjectType
import MMiSSFormat
import MMiSSVariant
import MMiSSReAssemble
import MMiSSEditFormatConverter
import {-# SOURCE #-} MMiSSReadObject
import {-# SOURCE #-} MMiSSExportFiles

extractMMiSSObject :: View -> Link MMiSSObject -> Format 
   -> IO (WithError (String,ExportFiles))
extractMMiSSObject view link format =
   do
      extractedWE <- readMMiSSObject view link Nothing infinity True
      case fromWithError extractedWE of
         Left mess -> return (hasError mess)
         Right (element,preambleLinks,exportFiles0) ->
            do
               strWE <- exportElement view format preambleLinks element
               return (mapWithError
                  (\ str -> (str,exportFiles0))
                  strWE
                  )
