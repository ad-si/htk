-- | This file contains the doMMiSSRegistrations function, which should
-- execute all the registrations required in the mmiss directory.
module MMiSSRegistrations(doMMiSSRegistrations) where

import ObjectTypes
import Folders

import MMiSSObjectTypeType

import MMiSSObjectTypeType
import MMiSSObjectTypeInstance
import MMiSSDisplay
import MMiSSPreamble
import MMiSSPackageFolder
import MMiSSFileType
import MMiSSSubFolder

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

