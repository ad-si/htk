{- We define functions which convert an object into a text form in a format
   specified by MMiSSFormat.hs -}
module MMiSSEditFormatConverter(
   EditFormatConverter(..), -- The functions provided
   toEditFormatConverter, -- :: Format -> EditFormatConverter

   exportElement,
      -- :: View -> Format -> [MMiSSPackageFolder] -> Element 
      -- -> IO (WithError String)
   ) where

import Maybe

import Data.FiniteMap

import Computation 
import Debug(debugString)

import ViewType
import Link
import LinkManager

import EmacsContent

import Text.XML.HaXml.Types

import LaTeXParser

import MMiSSEditXml(TypedName)
import MMiSSFormat
import MMiSSEditXml
import MMiSSPreamble
import MMiSSFileSystemExamples
import MMiSSPackageFolder

-- ----------------------------------------------------------------------
-- The types
-- ----------------------------------------------------------------------

---
-- For EditFormatConvert, the String's are the file name (made available
-- for error messages).
--
data EditFormatConverter = EditFormatConverter {
   toEdit :: String -> Element 
      -> WithError (EmacsContent (TypedName,[Attribute])),
   fromEdit :: String -> EmacsContent (TypedName,[Attribute]) 
      -> IO (WithError Element)
   }

-- ----------------------------------------------------------------------
-- The functions
-- ----------------------------------------------------------------------

toEditFormatConverter :: Format -> EditFormatConverter
toEditFormatConverter XML = 
   EditFormatConverter {
      toEdit = (\ str elem -> hasValue (toEditableXml str elem)),
      fromEdit = fromEditableXml
      }
toEditFormatConverter LaTeX =
   EditFormatConverter {
      toEdit = (\ preambleOpt element -> makeMMiSSLatexContent element False []),
      fromEdit = (\ fileName content 
         ->
            do
               let 
                  str = mkLaTeXString content

                  fileSystem = oneFileFileSystem fileName str


               parsedWE <- parseMMiSSLatex fileSystem fileName

--             debugString ("START|"++str++"|END")

               return (mapWithError 
                  (\ (element,_) -> element) 
                  parsedWE
                  )
         )
      }

exportElement :: View -> Format 
   -> [MMiSSPackageFolder] -> Element -> IO (WithError String)
exportElement _ XML _ element = return (hasValue (toExportableXml element))
exportElement view LaTeX packageFolders element =
   do
      (laTeXPreambles :: [(MMiSSLatexPreamble,PackageId)]) 
         <- mapM
            (\ packageFolder -> 
               do
                  let
                     preambleLink = toMMiSSPreambleLink packageFolder
                  laTeXPreamble <- readPreamble view preambleLink

                  packageIdStr1 <- getFullName view packageFolder
                  let
                     packageId = PackageId {packageIdStr = packageIdStr1}

                  return (laTeXPreamble,packageId)
               )
            packageFolders

      let
         emacsContentWE = makeMMiSSLatexContent element True laTeXPreambles
      return (mapWithError
         (\ emacsContent -> mkLaTeXString emacsContent)
         emacsContentWE
         )

      
