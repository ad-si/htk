-- | This file contains the doRegistrations function, which should
-- execute the registration functions of all object and display types.
module Registrations(doRegistrations) where

import AttributesType
import Folders
import Files
import NoAccessObject


doRegistrations :: IO ()
doRegistrations =
   do
      registerFolders
      registerFiles
      registerAttributes
      registerNoAccessObjectType