-- | This module contains code for turning a LinkedObject or a WrappedLink
-- into one of the types known to us.
--
-- It also contains code for constructing an empty link corresponding
-- to a base type in a bundle.  (This is how all links get started.)
module MMiSS.SplitLink(
   SplitLink(..),
   splitWrappedLink,
   splitLinkedObject,
   newEmptySplitLink,
   readSplitLink, -- :: View -> SplitLink -> IO LinkedObject
   wrapSplitLink, -- :: SplitLink -> WrappedLink

   ) where

import Types.ObjectTypes

import Types.Folders
import Types.Files
import Types.LinkManager
import Types.Link
import Types.View

import MMiSS.Preamble
import MMiSS.ObjectType
import MMiSS.FileType
import MMiSS.Bundle
import MMiSS.ImportExportErrors
import {-# SOURCE #-} MMiSS.ObjectTypeInstance
import {-# SOURCE #-} MMiSS.PackageFolder


-- ------------------------------------------------------------------------
-- The datatype
-- ------------------------------------------------------------------------

data SplitLink =
      FileC (Link File)
   |  FolderC (Link Folder)
   |  MMiSSPreambleC (Link MMiSSPreamble)
   |  MMiSSPackageFolderC (Link MMiSSPackageFolder)
   |  MMiSSObjectC (Link MMiSSObject)
   |  MMiSSFileC (Link MMiSSFile)
   |  UnknownLinkC

-- ------------------------------------------------------------------------
-- The functions
-- ------------------------------------------------------------------------

splitWrappedLink :: WrappedLink -> SplitLink
splitWrappedLink w =
   case (unpackWrappedLink w,unpackWrappedLink w,unpackWrappedLink w,
         unpackWrappedLinkToMMiSSPackageFolder w,
         unpackWrappedLinkToMMiSSObject w,
         unpackWrappedLink w) of
      (Just l,_,_,_,_,_) -> FileC l
      (_,Just l,_,_,_,_) -> FolderC l
      (_,_,Just l,_,_,_) -> MMiSSPreambleC l
      (_,_,_,Just l,_,_) -> MMiSSPackageFolderC l
      (_,_,_,_,Just l,_) -> MMiSSObjectC l
      (_,_,_,_,_,Just l) -> MMiSSFileC l
      (_,_,_,_,_,_) -> UnknownLinkC

splitLinkedObject :: LinkedObject -> SplitLink
splitLinkedObject = splitWrappedLink . toWrappedLink


wrapSplitLink :: SplitLink -> WrappedLink
wrapSplitLink link = case link of
   FileC l -> WrappedLink l
   FolderC l -> WrappedLink l
   MMiSSPreambleC l -> WrappedLink l
   MMiSSPackageFolderC l -> wrapMMiSSPackageFolderLink l
   MMiSSObjectC l -> wrapMMiSSObjectLink l
   MMiSSFileC l -> WrappedLink l
   UnknownLinkC -> error "wrapSplitLink: attempt to wrap unknown link"

-- ------------------------------------------------------------------------
-- Creating empty links
-- ------------------------------------------------------------------------

newEmptySplitLink :: View -> WrappedLink -> BundleTypeEnum -> IO SplitLink
newEmptySplitLink view parentLink enum =
   case enum of
      FolderEnum ->
         do
            link <- wrapNewEmptyLink view parentLink
            return (FolderC link)
      FileEnum ->
         do
            link <- wrapNewEmptyLink view parentLink
            return (FileC link)
      MMiSSFolderEnum ->
         do
            link <- newEmptyLinkMMiSSPackageFolder view parentLink
            return (MMiSSPackageFolderC link)
      MMiSSObjectEnum ->
         do
            link <- newEmptyLinkMMiSSObject view parentLink
            return (MMiSSObjectC link)
      MMiSSFileEnum ->
         do
            link <- wrapNewEmptyLink view parentLink
            return (MMiSSFileC link)
      MMiSSPreambleEnum ->
         do
            link <- wrapNewEmptyLink view parentLink
            return (MMiSSPreambleC link)
      UnknownType ->
         importExportError "Unable to create UnknownType in bundle"

-- ------------------------------------------------------------------------
-- Reading a link
-- ------------------------------------------------------------------------

readSplitLink :: View -> SplitLink -> IO LinkedObject
readSplitLink view splitLink = case splitLink of
   FolderC link ->
      do
         object <- readLink view link
         return (toLinkedObject object)
   FileC link ->
      do
         object <- readLink view link
         return (toLinkedObject object)
   MMiSSFileC link ->
      do
         object <- readLink view link
         return (toLinkedObject object)
   MMiSSObjectC link -> linkToLinkedObjectMMiSSObject view link
   MMiSSPackageFolderC link -> linkToLinkedObjectMMiSSPackageFolder view link
   MMiSSPreambleC link -> importExportError "readSplitLink: used on preamble"
   UnknownLinkC -> importExportError "readSplitLink: used on UnknownLinkC"
