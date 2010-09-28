-- | This file contains the doRegistrations function, which should
-- execute the registration functions of all object and display types.
module Types.Registrations(doRegistrations) where

import Types.AttributesType
import Types.Folders
import Types.Files
import Types.NoAccessObject


doRegistrations :: IO ()
doRegistrations =
   do
      registerFolders
      registerFiles
      registerAttributes
      registerNoAccessObjectType
