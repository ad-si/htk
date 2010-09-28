-- | This file contains the doMMiSSRegistrations function, which should
-- execute all the registrations required in the mmiss directory.
module MMiSS.Registrations(doMMiSSRegistrations) where

import Types.ObjectTypes
import Types.Folders

import MMiSS.ObjectTypeType

import MMiSS.ObjectTypeType
import MMiSS.ObjectTypeInstance
import MMiSS.Display
import MMiSS.Preamble
import MMiSS.PackageFolder
import MMiSS.FileType
import MMiSS.SubFolder

doMMiSSRegistrations :: IO ()
doMMiSSRegistrations =
   do
      registerMMiSSObjects
      registerMMiSSDisplay
      registerMMiSSSubFolder
      registerAsMoveable (error "Unknown PackageFolder" :: MMiSSPackageFolder)

registerMMiSSObjects :: IO ()
registerMMiSSObjects =
   do
      registerObjectType (error "Unknown MMiSSObjectType" :: MMiSSObjectType)
      registerObjectType (error "Unknown MMiSSPreambleType"
         :: MMiSSPreambleType)
      registerObjectType (error "Unknown MMiSSPackageFolderType"
         :: MMiSSPackageFolderType)
      registerObjectType (error "Unknown MMiSSFileType" :: MMiSSFileType)

