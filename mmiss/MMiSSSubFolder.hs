{- An MMiSSSubFolder is a folder contained within an MMiSS package.
   Fortunately we don't need to define these as a new instance of ObjectTypes;
   instead we can use the bog-standard Folders.
   -}
module MMiSSSubFolder(
   mmissSubFolderType, 
      -- :: FolderType
      -- feed this to Folders.newEmptyFolder

   registerMMiSSSubFolder,
      -- MUST be done on initialisation.
   ) where

import GraphConfigure

import Folders
import GlobalRegistry
import DisplayParms

registerMMiSSSubFolder = registerExtraFolderType mmissSubFolderType
   

mmissSubFolderType :: FolderType
mmissSubFolderType =
   let
      key = oneOffKey "MMiSSSubFolder" ""
      displayParms =  readDisplay "green rhombus"
   in
      mkFolderType0 key displayParms
   

