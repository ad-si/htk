{- This module contains the function for converting files or EmacsContent
   in LaTeX or XML into bundles. -}
module MMiSSBundleConvert(
   parseBundle, 
      -- :: Format -> FileSystem -> FilePath -> IO Bundle
      -- Convert a LaTeX/Xml file into a Bundle
   parseBundle1,
      -- :: ElementInfo -> Format -> FileSystem -> FilePath -> IO Bundle
      -- As for parseBundle, but also takes ElementInfo, containing location
      -- information, for example inferred from the position of the file 
      -- within a Bundle.
   parseBundle2,
      -- :: ElementInfo -> Element -> [(MMiSSLatexPreamble,PackageId)] 
      -- -> IO Bundle
      -- As for parseBundle1, but takes the element directly rather than
      -- reading it from a file system.
   ) where

import IO
import Maybe
import AtomString

import Computation

import Text.XML.HaXml.Types

import EmacsContent

import LaTeXParser

import MMiSSFormat
import MMiSSBundle
import MMiSSDTD
import MMiSSDTDAssumptions
import MMiSSBundleSimpleUtils
import MMiSSBundleTypes
import MMiSSImportExportErrors
import MMiSSElementInstances
import MMiSSElementInfo

-- -------------------------------------------------------------------------
-- Parsing
-- -------------------------------------------------------------------------

parseBundle :: Format -> FileSystem -> FilePath -> IO Bundle
parseBundle = parseBundle1 (
   emptyElementInfo {packageIdOpt = Just (fromString "")})

parseBundle1 :: ElementInfo -> Format -> FileSystem -> FilePath -> IO Bundle
parseBundle1 elInfo0 format fileSystem filePath =
   do
      (element0 :: Element,preambleList :: [(MMiSSLatexPreamble,PackageId)]) 
            <- case format of
         LaTeX -> 
            do
               parseResult <- parseMMiSSLatex fileSystem filePath
               coerceImportExportIO parseResult
         XML ->
            do
               xmlStringWE <- readString fileSystem filePath
               xmlString <- coerceImportExportIO xmlStringWE
               elementWE <- xmlParseCheck filePath xmlString
               element <- coerceImportExportIO elementWE 
               return (element,[])
      parseBundle2 elInfo0 element0 preambleList

parseBundle2 :: ElementInfo -> Element -> [(MMiSSLatexPreamble,PackageId)] 
   -> IO Bundle
parseBundle2 elInfo0 element0 preambleList =
   do
      let 
         elInfoPlusWE = getElementInfo element0
      (elInfo1,element1) <- coerceImportExportIO elInfoPlusWE

      let
         elInfo2WE = mergeElementInfoStrict elInfo0 elInfo1
      elInfo2 <- coerceImportExportIO elInfo2WE

      packageId <- case packageIdOpt elInfo2 of
         Just packageId -> return packageId
         Nothing -> importExportError "Unable to determine package id"
            -- in fact I think this will never happen, since imported files
            -- will be parsed by parseBundle, and files in bundles should
            -- come to this function with their packageId.

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
                  in
                     (packageId1,node2)
                  )
               preambleList

         preambleBundle :: Bundle
         preambleBundle = Bundle preambleNodes


         elementText = mkBundleText element1
         elementNodeData = Object [(Nothing,elementText)]
         elementPackageWE = wrapContainingMMiSSPackage packagePath 
            mmissObjectType elementNodeData

      elementPackage <- coerceImportExportIO elementPackageWE
                         
      let
         elementBundle :: Bundle
         elementBundle = Bundle [(packageId,elementPackage)]

         bothBundlesWE = mergeBundles [elementBundle,preambleBundle]

      bothBundles <- coerceImportExportIO bothBundlesWE

      return bothBundles    
            
               
