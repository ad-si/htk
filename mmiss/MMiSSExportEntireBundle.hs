{- This module handles the Entire export of a BundleNode, that is, the sort
   you get when you select a folder or a file, so that it includes every
   variant of a file. -}
module MMiSSExportEntireBundle(
   exportEntireLinkedObject,
      -- :: View -> LinkedObject -> ExportOpts -> IO Bundle
   exportMMiSSPreamble,
      -- :: View -> Link MMiSSPreamble -> Bool 
      -- -> IO BundleNode
   ) where

import Messages
import AtomString
import Sources
import ICStringLen

import View
import Link

import Folders
import Files
import LinkManager
import BasicObjects

import MMiSSObjectType
import MMiSSFileType
import MMiSSSplitLink
import MMiSSPreamble

import MMiSSBundle
import MMiSSBundleUtils
import MMiSSObjectTypeInstance
import MMiSSPackageFolder



-- --------------------------------------------------------------------------
-- exportEntireLinkedObject
-- --------------------------------------------------------------------------


exportEntireLinkedObject :: View -> LinkedObject -> ExportOpts -> IO Bundle
exportEntireLinkedObject view linkedObject exportOpts =
   do
      bundleNode <- exportEntireLinkedObject1 view linkedObject exportOpts
      packageId <- mkPackageId view linkedObject
      return (Bundle [(packageId,bundleNode)])

-- --------------------------------------------------------------------------
-- Splitting
-- --------------------------------------------------------------------------

exportEntireLinkedObject1 :: View -> LinkedObject -> ExportOpts 
   -> IO BundleNode
exportEntireLinkedObject1 view linkedObject exportOpts =
   case splitLinkedObject linkedObject of
      FileC fileLink -> exportEntireFile view fileLink exportOpts
      FolderC folderLink -> exportEntireFolder view folderLink exportOpts
      MMiSSPackageFolderC packageFolderLink 
         -> exportEntirePackageFolder view packageFolderLink exportOpts
      MMiSSObjectC objectLink
         -> exportEntireMMiSSObject view objectLink exportOpts
      MMiSSFileC mmissFileLink
         -> exportEntireMMiSSFile view mmissFileLink exportOpts
      -- don't put preambles here since they shouldn't happen
      _ ->
         do
            linkedObjectStr <- describeLinkedObject view linkedObject
            errorMess ("Unable to extract " ++ linkedObjectStr)
            bundleNode <- getUnknownBundleNode view linkedObject
            return bundleNode

-- --------------------------------------------------------------------------
-- MMiSSPackageFolders
-- --------------------------------------------------------------------------

exportEntirePackageFolder 
   :: View -> Link MMiSSPackageFolder -> ExportOpts -> IO BundleNode
exportEntirePackageFolder view packageFolderLink exportOpts =
   do
      packageFolder <- readLink view packageFolderLink

      -- add the preamble
      extraNodes <-
         if recurseDepth exportOpts > 0
            then
               do
                  preambleNode <- exportMMiSSPreamble view
                     (toMMiSSPreambleLink packageFolder) exportOpts
                  return [preambleNode]
                  
            else
               return []

      exportDirPlus view (toLinkedObject packageFolder) exportOpts extraNodes

-- --------------------------------------------------------------------------
-- Preambles
-- --------------------------------------------------------------------------

exportMMiSSPreamble :: View -> Link MMiSSPreamble -> ExportOpts 
   -> IO BundleNode
exportMMiSSPreamble view link exportOpts = 
   do
      bundleText1 <- if (getText exportOpts)
         then
            do
               mmissLaTeXPreamble <- readPreamble view link
               let
                  preambleStr :: String
                  preambleStr = toString mmissLaTeXPreamble

                  preambleICSL :: ICStringLen 
                  preambleICSL = fromString preambleStr

                  bundleText1 = BundleString {
                     contents = preambleICSL,
                     charType = Byte
                     }
               return bundleText1
         else
            return NoText

      let
         bundleNodeData1 = Object [(Nothing,bundleText1)]

      return (BundleNode {
         fileLoc = preambleFileLoc,
         bundleNodeData = bundleNodeData1
         })

-- --------------------------------------------------------------------------
-- Files
-- --------------------------------------------------------------------------

exportEntireFile :: View -> Link File -> ExportOpts -> IO BundleNode
exportEntireFile view fileLink exportOpts =
   do
      file <- readLink view fileLink
      fileLoc1 <- getFileLoc view (toLinkedObject file)
      bundleText1 <- if getText exportOpts
         then
            do
               icsl <- getAsICSL view file
               let
                  bundleText = BundleString {
                     contents = icsl,
                     charType = Byte
                     }
               return bundleText
         else
            return NoText

      let
         bundleNodeData1 = Object [(Nothing,bundleText1)]

      return (BundleNode {
         fileLoc = fileLoc1,
         bundleNodeData = bundleNodeData1
         })

      
-- --------------------------------------------------------------------------
-- Folders
-- --------------------------------------------------------------------------

exportEntireFolder :: View -> Link Folder -> ExportOpts -> IO BundleNode
exportEntireFolder view link exportOpts = 
   do
      folder <- readLink view link
      exportDir view (toLinkedObject folder) exportOpts

-- --------------------------------------------------------------------------
-- MMiSSObjects
-- --------------------------------------------------------------------------

exportEntireMMiSSObject :: View -> Link MMiSSObject -> ExportOpts 
   -> IO BundleNode
exportEntireMMiSSObject view link exportOpts =
   do
      mmissObject <- readLink view link
      fileLoc1 
         <- getFileLocForExport view (toLinkedObject mmissObject) exportOpts
      bundleNodeData1 <- getBundleNodeData1 view mmissObject exportOpts

      return (BundleNode {
         fileLoc = fileLoc1,
         bundleNodeData = bundleNodeData1
         })

-- --------------------------------------------------------------------------
-- MMiSSFiles
-- --------------------------------------------------------------------------

exportEntireMMiSSFile :: View -> Link MMiSSFile -> ExportOpts -> IO BundleNode
exportEntireMMiSSFile view link exportOpts =
   do
      mmissFile <- readLink view link
      fileLoc1 <- getFileLoc view (toLinkedObject mmissFile)
      bundleNodeData1 <- getBundleNodeData1 view mmissFile exportOpts
      
      return (BundleNode {
         fileLoc = fileLoc1,
         bundleNodeData = bundleNodeData1
         })

-- --------------------------------------------------------------------------
-- Modified version of getBundleNodeData1 which checks for recursion depth.
-- --------------------------------------------------------------------------

getBundleNodeData1 :: HasBundleNodeData object 
   => View -> object -> ExportOpts -> IO BundleNodeData
getBundleNodeData1 view object exportOpts =
   if recurseDepth exportOpts >= 1
      then
         getBundleNodeData view object exportOpts
      else
         return NoData

-- --------------------------------------------------------------------------
-- General Folder-Like objects
-- --------------------------------------------------------------------------

-- Export the objects within the contents of a linked object and construct
-- a BundleNode containing them 
exportDir :: View -> LinkedObject -> ExportOpts -> IO BundleNode
exportDir view linkedObject exportOpts =
   exportDirPlus view linkedObject exportOpts []

-- Generalisation of exportDir where we also supply a list of
-- nodes to be prepended to the contents.
exportDirPlus :: View -> LinkedObject -> ExportOpts -> [BundleNode] 
   -> IO BundleNode
exportDirPlus view linkedObject0 exportOpts0 extraNodes =
   do
      fileLoc1 <- getFileLoc view linkedObject0

      let
         recurseDepth0 = recurseDepth exportOpts0

         recurseDepth1 = recurseDepth0 - 1

         exportOpts1 = exportOpts0 {
            recurseDepth = recurseDepth0
            }

      linkedObjects <- if recurseDepth1 >= 0
         then
            do
               objectContents 
                  <- readContents (listObjectContents linkedObject0)
               return (map snd objectContents)
         else
            return []

      bundleNodes0 <- mapM
         (\ linkedObject1 
            -> exportEntireLinkedObject1 view linkedObject1 exportOpts1
            )
         linkedObjects
      let
         bundleNodes1 = extraNodes ++ bundleNodes0

      return (BundleNode {
         fileLoc = fileLoc1,
         bundleNodeData = Dir bundleNodes1
         })

