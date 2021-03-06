-- | This module contains the function for converting files or EmacsContent
-- in LaTeX or XML into bundles.
module MMiSS.BundleConvert(
   parseBundle,
      -- :: Format -> FileSystem -> FilePath -> IO (Bundle,PackageId)
      -- Convert a LaTeX/Xml file into a Bundle, also returning the PackageId
      -- of the package actually containing its contents.
   parseBundle1,
      -- :: ElementInfo -> Format -> FileSystem -> FilePath
      -- -> IO (Bundle,PackageId)
      -- As for parseBundle, but also takes ElementInfo, containing location
      -- information, for example inferred from the position of the file
      -- within a Bundle.
   parseBundle2,
      -- :: ElementInfo -> Element -> [(MMiSSLatexPreamble,PackageId)]
      -- -> IO (Bundle,PackageId)
      -- As for parseBundle1, but takes the element directly rather than
      -- reading it from a file system.
   ) where

import IO
import Data.Maybe

import Util.AtomString
import Util.Computation

import Text.XML.HaXml.Types

import MMiSS.LaTeX.Parser
import MMiSS.LaTeX.Preamble

import MMiSS.Format
import MMiSS.Bundle
import MMiSS.DTD
import MMiSS.BundleSimpleUtils
import MMiSS.BundleTypes
import MMiSS.ImportExportErrors
import MMiSS.ElementInstances
import MMiSS.ElementInfo

-- -------------------------------------------------------------------------
-- Parsing
-- -------------------------------------------------------------------------

parseBundle :: Format -> FileSystem -> FilePath -> IO (Bundle,PackageId)
parseBundle = parseBundle1 emptyElementInfo

parseBundle1 :: ElementInfo -> Format -> FileSystem -> FilePath
   -> IO (Bundle,PackageId)
parseBundle1 elInfo0 format fileSystem filePath =
   do
      let
         coerce = coerceImportExportIOPrefix (filePath ++ ": ")

      (element0 :: Element,preambleList :: [(MMiSSLatexPreamble,PackageId)])
            <- case format of
         LaTeX ->
            do
               parseResult <- parseMMiSSLatex fileSystem filePath True
               coerce parseResult
         XML ->
            do
               xmlStringWE <- readString fileSystem filePath
               xmlString <- coerceImportExportIO xmlStringWE
               elementWE <- xmlParseCheck filePath xmlString
               element <- coerce elementWE
               return (element,[])
      parseBundle2 elInfo0 element0 preambleList

parseBundle2 :: ElementInfo -> Element -> [(MMiSSLatexPreamble,PackageId)]
   -> IO (Bundle,PackageId)
parseBundle2 elInfo0 element0 preambleList =
   do
      let
         elInfoPlusWE = getElementInfo element0
      (elInfo1,element1) <- coerceImportExportIO elInfoPlusWE

      let
         elInfo2WE = mergeElementInfoStrict elInfo0 elInfo1
      elInfo2 <- coerceImportExportIO elInfo2WE

      packageId <- return (case packageIdOpt elInfo2 of
         Just packageId -> packageId
         Nothing -> defaultPackageId
         )

      packagePath <- case packagePathOpt elInfo2 of
         Just packagePath -> return packagePath
         Nothing -> importExportError (
            "Imported object has no identifying label or packagePath so "
            ++ "I don't know where to put it")

      let
         preambleNodes :: [(PackageId,BundleNode)]
         preambleNodes =
            map
               (\ (preamble,packageId1) ->
                  let
                     text1 = mkBundleText preamble
                     fileLoc1 = FileLoc {
                        name = Nothing,
                        objectType = mmissPreambleType
                        }
                     node1 = BundleNode {
                        fileLoc = fileLoc1,
                        bundleNodeData = Object [(Nothing,text1)]
                        }

                     node2 = mkOneMMiSSPackage node1

                     packageId2 =
                        if packageId1 == defaultPackageId
                           then
                              packageId
                           else
                              packageId1
                  in
                     (packageId2,node2)
                  )
               preambleList

         preambleBundle :: Bundle
         preambleBundle = Bundle preambleNodes


         elementText = mkBundleText element1
         elementNodeData = Object [(Just (variants elInfo2),elementText)]
         elementPackageWE = wrapContainingMMiSSPackage
            (packageNameOpt elInfo2) packagePath mmissObjectType
            elementNodeData

      elementPackage <- coerceImportExportIO elementPackageWE

      let
         elementBundle :: Bundle
         elementBundle = Bundle [(packageId,elementPackage)]

         bothBundlesWE = mergeBundles [elementBundle,preambleBundle]

      bothBundles <- coerceImportExportIO bothBundlesWE

      return (bothBundles,packageId)

-- This value is also that returned by LaTeXParser, so should not be changed
-- unless that, too, is updated.
defaultPackageId :: PackageId
defaultPackageId = fromString ""


