-- | We define functions which convert an object into a text form in a format
-- specified by MMiSSFormat.hs
module MMiSSEditFormatConverter(
   EditFormatConverter(..), -- The functions provided
   toEditFormatConverter, -- :: Format -> EditFormatConverter

   exportElement,
      -- :: View -> Format -> [MMiSSPackageFolder] -> Element
      -- -> IO (WithError String)

   -- for specification see comment by definition.
   exportElement1,
      -- :: View -> Format
      -- -> Bool -- ^ if set, include a \documentclass header.
      -- -> [MMiSSPackageFolder] -> Element  -> IO (WithError String)
   ) where

import Computation
import Messages
import AtomString(toString)

import ViewType
import LinkManager

import EmacsContent

import Text.XML.HaXml.Types

import LaTeXParser
import LaTeXPreamble(MMiSSLatexPreamble)

import MMiSSEditXml(TypedName)
import MMiSSFormat
import MMiSSEditXml
import MMiSSPreamble
import MMiSSFileSystemExamples
import MMiSSPackageFolder

-- ----------------------------------------------------------------------
-- The types
-- ----------------------------------------------------------------------

-- | For EditFormatConvert, the String\'s are the file name (made available
-- for error messages).
--
data EditFormatConverter = EditFormatConverter {
   toEdit :: String -> Element
      -> WithError (EmacsContent (TypedName,IncludeInfo)),
   fromEdit :: String -> EmacsContent (TypedName,IncludeInfo)
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


               parsedWE <- parseMMiSSLatex fileSystem fileName False

--             debugString ("START|"++str++"|END")

               case fromWithError parsedWE of
                  Left mess -> return (Left mess)
                  Right (element,preambles) ->
                     do
                        case preambles of
                           (preamble,_):_ ->
                              let
                                 maxLen = 70

                                 preambleString1 = toString preamble
                                 preambleString2 =
                                    if length preambleString1 < maxLen
                                       then
                                          preambleString1
                                       else
                                          take maxLen preambleString1
                                             ++ "..."
                              in
                                 errorMess ("Unexpected preamble: \n   " ++
                                    preambleString2 ++ "\n ignored")
                           [] -> done
                        return (Right element)

               return (mapWithError
                  (\ (element,_) -> element)
                  parsedWE
                  )
         )
      }

exportElement :: View -> Format
   -> [MMiSSPackageFolder] -> Element -> IO (WithError String)
exportElement view format = exportElement1 view format True

-- | Convert an Element to an XML or LaTeX String
exportElement1 :: View -> Format
   -> Bool -- ^ if set, include a \documentclass header.
   -> [MMiSSPackageFolder] -> Element  -> IO (WithError String)
exportElement1 _ XML _ _ element = return (hasValue (toExportableXml element))
exportElement1 view LaTeX includeHeader packageFolders element =
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
         emacsContentWE =
            makeMMiSSLatexContent element includeHeader laTeXPreambles
      return (mapWithError
         (\ emacsContent -> mkLaTeXString emacsContent)
         emacsContentWE
         )


