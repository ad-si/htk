{- This module contains various utilities for manipulating MMiSSBundles. -}
module MMiSSBundleUtils(
   bundleToICSL, -- :: BundleText -> Maybe (ICStringLen,CharType)

   mkPackageId, -- :: View -> LinkedObject -> IO PackageId

   getFileLoc, -- :: View -> LinkedObject -> I0 FileLoc
   getFileLocForExport, -- :: View -> LinkedObject -> ExportOpts -> IO FileLoc
      -- getFileLocForExport is different in that for MMiSS objects it
      -- returns a file type with appropriate extension.
   getFileLocAndParent, -- :: View -> LinkedObject -> IO (FileLoc,Maybe LinkedObject)
   getUnknownBundleNode, -- :: LinkedObject -> IO BundleNode
   preambleFileLoc, -- :: FileLoc

   ) where

import Maybe

import Text.XML.HaXml.Types

import ICStringLen
import AtomString
import UTF8
import Computation
import FileNames
import IntPlus

import EntityNames

import ObjectTypes
import View
import Link
import LinkManager
import Folders
import GlobalRegistry

import LaTeXParser(MMiSSLatexPreamble)

import MMiSSVariant
import MMiSSDTD
import MMiSSFormat
import MMiSSSplitLink
import MMiSSBundle
import MMiSSBundleTypes
import MMiSSImportExportErrors

-- --------------------------------------------------------------------------
-- BundleText functions
-- --------------------------------------------------------------------------

bundleToICSL :: BundleText -> Maybe (ICStringLen,CharType)
bundleToICSL (BundleString {contents = contents,charType = charType}) =
   Just (contents,charType)
bundleToICSL (BundleDyn {}) 
   = importExportError "API bug: Untranslated BundleDyn in exported bundle"
bundleToICSL NoText = Nothing

-- --------------------------------------------------------------------------
-- Constructing PackageId's
-- --------------------------------------------------------------------------

mkPackageId :: View -> LinkedObject -> IO PackageId
mkPackageId view linkedObject =
   do
      packageIdStr <- describeLinkedObject view linkedObject
      return (PackageId packageIdStr)

-- -------------------------------------------------------------------------
-- Constructing the FileLoc part of a LinkedObject
-- -------------------------------------------------------------------------

getFileLoc :: View -> LinkedObject -> IO FileLoc
getFileLoc view linkedObject =
   do
      (fileLoc,parentOpt) <- getFileLocAndParent view linkedObject
      return fileLoc

getFileLocForExport :: View -> LinkedObject -> ExportOpts -> IO FileLoc
getFileLocForExport view linkedObject exportOpts =
   do
      fileLoc0 <- getFileLoc view linkedObject
      let
         objectType0 = objectType fileLoc0
         objectType1 = case base objectType0 of
            MMiSSObjectEnum -> mmissObjectAsFileType (format exportOpts)
            _ -> objectType0

         fileLoc1 = fileLoc0 {objectType = objectType1}
      return fileLoc1

getFileLocAndParent :: View -> LinkedObject -> IO (FileLoc,Maybe LinkedObject)
getFileLocAndParent view linkedObject =
   do
      nameAndParentOpt <- getNameAndParentOpt linkedObject
      extra1 <- getExtra view linkedObject
      let
         getExtraY :: IO (Maybe String)
            -- used for those types where we supply an extra.
         getExtraY =
            do
               extra1 <- getExtra view linkedObject
               return (Just extra1)

         getExtraN :: IO (Maybe String)
            -- used for those where we don't.
         getExtraN = return Nothing

         (name1,ext1,parentOpt) = case nameAndParentOpt of
            Nothing -> (Nothing,Nothing,Nothing)
            Just (name0,parent) 
               | Just (name1,ext0) <- splitExtension name0
               -> (Just name1,Just ext0,Just parent)
               | True -> (Just name0,Nothing,Just parent)
 
         (base1,getExt) = case splitLinkedObject linkedObject of
            FileC _ -> (FileEnum,getExtraY)
            FolderC _ -> (FolderEnum,getExtraY)
            UnknownLinkC -> (UnknownType,getExtraY)
            MMiSSPreambleC _ -> (MMiSSPreambleEnum,getExtraN)
            MMiSSPackageFolderC _ -> (MMiSSFolderEnum,getExtraN)
            MMiSSObjectC _ -> (MMiSSObjectEnum,getExtraN) 
            MMiSSFileC _ -> (MMiSSFileEnum,getExtraN)

      extra1 <- getExt

      let
         bundleType = BundleType {base = base1,ext = ext1,extra = extra1} 

         fileLoc = FileLoc {
            name = name1,
            objectType = bundleType
            }

      return (fileLoc,parentOpt)

preambleFileLoc :: FileLoc
preambleFileLoc =
   FileLoc {
      name = Nothing,
      objectType = mmissPreambleType
      }
               
 
getExtra :: View -> LinkedObject -> IO String
getExtra view linkedObject =
   do
      let 
         wrappedLink = toWrappedLink linkedObject
      wrappedObject <- wrapReadLink view wrappedLink
      let
         wrappedObjectType = getObjectType wrappedObject
         key = objectTypeId wrappedObjectType
      return (toString key)

-- --------------------------------------------------------------------------
-- Miscellaneous functions
-- --------------------------------------------------------------------------

-- BundleNode extraction in the case when we can't do the job properly and
-- need a soft fall-out.
getUnknownBundleNode :: View -> LinkedObject -> IO BundleNode
getUnknownBundleNode view linkedObject =
   do
      fileLoc1 <- getFileLoc view linkedObject
      name1 <- getNameOpt linkedObject
      let
         bundleNode = BundleNode {
            fileLoc = fileLoc1,
            bundleNodeData = NoData
            }
      return bundleNode

getNameOpt :: LinkedObject -> IO (Maybe String)
getNameOpt linkedObject =
   do
      nameAndParentOpt <- getNameAndParentOpt linkedObject
      return (fmap fst nameAndParentOpt)

getNameAndParentOpt :: LinkedObject -> IO (Maybe (String,LinkedObject))
getNameAndParentOpt linkedObject =
   do
      insertionOpt <- getCurrentInsertion linkedObject
      let
         name1 = fmap
            (\ insertion ->
               let
                  (parent,name) = unmkInsertion insertion
               in
                  (toString name,parent)
               )
            insertionOpt

      return name1
