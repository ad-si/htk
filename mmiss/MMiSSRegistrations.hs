{- This file contains the doMMiSSRegistrations function, which should
   execute all the registrations required in the mmiss directory. 
   -}
module MMiSSRegistrations(doMMiSSRegistrations) where

import ObjectTypes

import MMiSSObjectTypeType

import MMiSSObjectTypeType
import MMiSSObjectTypeInstance
import MMiSSDisplay
import MMiSSPreamble
import MMiSSPackageFolder

doMMiSSRegistrations :: IO ()
doMMiSSRegistrations =
   do
      registerMMiSSObjects
      registerMMiSSDisplay

registerMMiSSObjects :: IO ()
registerMMiSSObjects =
   do
      registerObjectType (error "Unknown MMiSSObjectType" :: MMiSSObjectType)
      registerObjectType (error "Unknown MMiSSPreambleType" 
         :: MMiSSPreambleType)
      registerObjectType (error "Unknown MMiSSPackageFolderType" 
         :: MMiSSPackageFolderType)