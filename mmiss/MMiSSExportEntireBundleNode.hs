{- This module handles the Entire export of a BundleNode, that is, the sort
   you get when you select a folder or a file, so that it includes every
   variant of a file. -}
module MMiSSExportEntireBundleNode(
   exportEntireLinkedObject,
      -- :: View -> LinkedObject -> ExportOpts -> IO BundleNode
   ) where

import Messages
import AtomString

import View
import Link

import Folders
import Files
import LinkManager

import MMiSSPackageFolder
import MMiSSObjectType
import MMiSSFileType
import MMiSSSplitLink

import MMiSSBundle


-- --------------------------------------------------------------------------
-- Splitting
-- --------------------------------------------------------------------------

exportEntireLinkedObject :: View -> LinkedObject -> ExportOpts -> IO BundleNode
exportEntireLinkedObject view linkedObject exportOpts =
   case splitLinkedObject linkedObject of
      FileC fileLink -> exportEntireFile view fileLink exportOpts
      FolderC folderLink -> exportEntireFolder view folderLink exportOpts
      MMiSSPackageFolderC packageFolderLink 
         -> exportEntirePackageFolder view packageFolderLink exportOpts
      MMiSSObjectC objectLink
         -> exportEntireMMiSSObject view objectLink exportOpts
      MMiSSFileC mmissFileLink
         -> exportEntireMMiSSFile view mmissFileLink exportOpts
      _ ->
         do
            linkedObjectStr <- describeLinkedObject view linkedObject
            errorMess ("Unable to extract " ++ linkedObjectStr)
            bundleNode <- getUnknownBundleNode linkedObject
            return bundleNode

-- --------------------------------------------------------------------------
-- MMiSSPackageFolders
-- --------------------------------------------------------------------------

exportEntirePackageFolder 
   :: View -> Link MMiSSPackageFolder -> ExportOpts -> IO BundleNode
exportEntirePackageFolder = error "TBD"

-- --------------------------------------------------------------------------
-- Files
-- --------------------------------------------------------------------------

exportEntireFile :: View -> Link File -> ExportOpts -> IO BundleNode
exportEntireFile = error "TBD"

-- --------------------------------------------------------------------------
-- Folders
-- --------------------------------------------------------------------------

exportEntireFolder :: View -> Link Folder -> ExportOpts -> IO BundleNode
exportEntireFolder = error "TBD"

-- --------------------------------------------------------------------------
-- MMiSSObjects
-- --------------------------------------------------------------------------

exportEntireMMiSSObject :: View -> Link MMiSSObject -> ExportOpts -> IO BundleNode
exportEntireMMiSSObject = error "TBD"

-- --------------------------------------------------------------------------
-- MMiSSFiles
-- --------------------------------------------------------------------------

exportEntireMMiSSFile :: View -> Link MMiSSFile -> ExportOpts -> IO BundleNode
exportEntireMMiSSFile = error "TBD"
