-- | This module attempts to read the files referenced in a bundle off the
-- file-system.  (So it is not used when bundles are read from the API.)
--
-- It assumes the bundle has already been dissected (by MMiSSBundleDissect).
module MMiSS.BundleReadFiles(
   readFiles, -- :: FilePath -> Bundle -> IO Bundle
   listUnfoundFiles,
      -- :: Bundle -> [(MMiSSPackageId,EntityFullName,MMiSSVariantSpec)]
      -- return all the files referenced in a bundle but not found.
      -- NB.  The name does not include possible extensions, which should
      -- be found by MMiSSFileType.findMMiSSFilesInRepository or
      -- MMiSSFileType.findMMiSSFilesInDirectory.
   ) where

import IO
import Maybe

import Imports.EntityNames

import Util.Computation
import Util.ExtendedPrelude
import Util.AtomString
import Util.FileNames
import Util.Messages

import Text.XML.HaXml.Types
import MMiSS.XmlExtras

import Posixutil.CopyFile

import MMiSS.Bundle
import MMiSS.BundleTypes
import MMiSS.BundleSimpleUtils
import MMiSS.ElementInstances
import MMiSS.FileType
import MMiSS.Variant
import MMiSS.ImportExportErrors
import MMiSS.ElementInfo

-- -------------------------------------------------------------------------
-- listUnfoundFiles
-- -------------------------------------------------------------------------

listUnfoundFiles :: Bundle -> [(PackageId,EntityFullName,MMiSSVariantSpec)]
listUnfoundFiles bundle =
   uniqOrd (
      do
         (locInfo,bundleNode) <- getAllNodes1 bundle
         bundleText <-
            if (base . objectType . fileLoc $ bundleNode) == MMiSSObjectEnum
               then
                  case bundleNodeData bundleNode of
                     Object variants -> map snd variants
                     _ -> []
               else
                  []
         (element :: Element) <-
            case fromWithError (fromBundleTextWE bundleText) of
               Left mess -> []
               Right element -> [element]
         subElement <- getAllElements1 element
         fileStr <- getAllFiles subElement
         fileFullName <- case fromWithError (fromStringWE fileStr) of
            Left _ -> []
            Right fullName -> [fullName]
         case lookupNode bundle (packageId locInfo) fileFullName of
            Just _ -> []
               -- file already found in bundle.
            Nothing ->
               return (packageId locInfo,fileFullName,variants0 locInfo)
      )

-- -------------------------------------------------------------------------
-- readFiles
-- -------------------------------------------------------------------------

readFiles :: FilePath -> Bundle -> IO Bundle
readFiles filePath bundle =
   do
      let
         unfoundFiles = listUnfoundFiles bundle

      (existingFiles0
            :: [[(PackageId,EntityFullName,String,MMiSSVariantSpec)]])
         <- mapM
            (\ (packageId,fullName0,variantSpec) ->
               do
                  found <- findMMiSSFilesInDirectory filePath fullName0
                  return (map
                     (\ (fullName1,ext)
                        -> (packageId,fullName1,ext,variantSpec))
                     found
                     )
               )
            unfoundFiles
      let
         existingFiles1 = concat existingFiles0

      (bundles1 :: [Maybe Bundle]) <-
         mapM
            (\ (packageId,fullName,ext,variantSpec) ->
               do
                  let
                     completeNameWE = unsplitFullName fullName ext
                  completeName <- coerceImportExportIO completeNameWE
                     -- I don't think this should go wrong unless someone
                     -- has put a tag containing invalid characters in
                     -- Files.xml.

                  let
                     fullPath = filePath `combineNames`
                        (toString completeName)
                  icslWE <- copyFileToICStringLenCheck fullPath
                  case fromWithError icslWE of
                     Left mess ->
                        do
                           -- Perhaps the user has moved the file away behind
                           -- our back?
                           warningMess mess
                           return Nothing
                     Right icsl ->
                        let
                           bundleText = BundleString {
                              contents = icsl,
                              charType = Byte
                              }

                           bundleNodeData
                              = Object [(Just variantSpec,bundleText)]

                           bundleNodeWE = wrapContainingMMiSSPackage Nothing
                              fullName (mmissFileType ext) bundleNodeData

                           bundleNode = coerceWithErrorOrBreak
                              (error "MMiSSBundleReadFiles bug 1")
                              bundleNodeWE
                        in
                           return (Just (Bundle [(packageId,bundleNode)]))
               )
            existingFiles1

      let
         bundleWE = mergeBundles (bundle : catMaybes bundles1)

      coerceImportExportIO (bundleWE)
