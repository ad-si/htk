{- This module contains various utilities for manipulating MMiSSBundles. -}
module MMiSSBundleUtils(
   bundleToICSL, -- :: BundleText -> Maybe (ICStringLen,CharType)
   bundleToElement, -- :: BundleText -> WithError Element
   nameFileLoc, -- :: FileLoc -> WithError EntityName
   describeFileLoc, -- :: FileLoc -> String

   mkPackageId, -- :: View -> LinkedObject -> IO PackageId

   getFileLoc, -- :: View -> LinkedObject -> I0 FileLoc
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

-- --------------------------------------------------------------------------
-- BundleText functions
-- --------------------------------------------------------------------------

bundleToICSL :: BundleText -> Maybe (ICStringLen,CharType)
bundleToICSL (BundleString {contents = contents,charType = charType}) =
   Just (contents,charType)
bundleToICSL (BundleElement element) = 
   Just (fromString (toExportableXml element),Byte)
bundleToICSL NoText = Nothing
 

bundleToElement :: BundleText -> WithError Element
bundleToElement (BundleElement element) = return element
bundleToElement (BundleString {contents = contents,charType = charType}) =
   do
      contentsStr <- case charType of
         Byte -> return (toString contents)
         Unicode -> fromUTF8WE (toString contents)
      xmlParseWE "MMiSS Bundle" contentsStr
bundleToElement NoText = fail "No text supplied for file"

-- --------------------------------------------------------------------------
-- Constructing PackageId's
-- --------------------------------------------------------------------------

mkPackageId :: View -> LinkedObject -> IO PackageId
mkPackageId view linkedObject =
   do
      packageIdStr <- describeLinkedObject view linkedObject
      return (PackageId packageIdStr)

-- --------------------------------------------------------------------------
-- FileLoc functions
-- --------------------------------------------------------------------------

strFileLoc :: FileLoc -> Maybe String
strFileLoc fileLoc =
   case name fileLoc of
      Nothing -> Nothing
      Just name0 -> case ext (objectType fileLoc) of
         Nothing -> Just name0
         Just ext0 -> Just (name0 ++ [specialChar] ++ ext0)

nameFileLoc :: FileLoc -> WithError EntityName
nameFileLoc fileLoc = 
   do
      str <- case strFileLoc fileLoc of
         Nothing -> fail "Attempt to write file where no name is specified"
         Just str -> return str
      fromStringWE str

describeFileLoc :: FileLoc -> String
describeFileLoc fileLoc =
   fromMaybe
      (fallBack fileLoc)
      (strFileLoc fileLoc)
   where
      fallBack :: FileLoc -> String
      fallBack fileLoc =
         case ext (objectType fileLoc) of
            Just ext1 -> "Unnamed object with extension " ++ ext1
          

describeBundleTypeEnum :: BundleTypeEnum -> String
describeBundleTypeEnum bte = case bte of
   FolderEnum -> "simple folder"
   FileEnum -> "simple file"
   MMiSSFolderEnum -> "MMiSS package folder"
   MMiSSObjectEnum -> "MMiSS object"
   MMiSSFileEnum -> "MMiSS file"
   MMiSSPreambleEnum -> "MMiSS preamble"
   UnknownType -> "Object of unknown type"

-- -------------------------------------------------------------------------
-- Constructing the FileLoc part of a LinkedObject
-- -------------------------------------------------------------------------

getFileLoc :: View -> LinkedObject -> IO FileLoc
getFileLoc view linkedObject =
   do
      name0Opt <- getNameOpt linkedObject
      extra1 <- getExtra view linkedObject
      let
         (name1,ext1) = case name0Opt of
            Nothing -> (Nothing,Nothing)
            Just name0 
               | Just (name1,ext0) <- splitExtension name0
               -> (Just name1,Just ext0)
               | True -> (Just name0,Nothing)
 
         mkBundleType base1 =
            BundleType {base = base1,ext = ext1,extra = Just extra1} 

         base1 = case splitLinkedObject linkedObject of
            FileC _ -> FileEnum
            FolderC _ -> FolderEnum
            MMiSSPreambleC _ -> MMiSSPreambleEnum
            MMiSSPackageFolderC _ -> MMiSSFolderEnum
            MMiSSObjectC _ -> MMiSSObjectEnum 
            MMiSSFileC _ -> MMiSSFileEnum
            UnknownLinkC -> UnknownType

         fileLoc = FileLoc {
            name = name1,
            objectType = mkBundleType base1
            }

      return fileLoc

preambleFileLoc :: FileLoc
preambleFileLoc =
   FileLoc {
      name = Nothing,
      objectType = BundleType {
         base = MMiSSPreambleEnum,
         ext = Nothing,
         extra = Nothing
         }
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
      insertionOpt <- getCurrentInsertion linkedObject
      let
         name1 = fmap
            (toString . snd . unmkInsertion)
            insertionOpt

      return name1
