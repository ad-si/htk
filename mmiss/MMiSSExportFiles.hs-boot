module MMiSSExportFiles where

import MMiSSPackageFolder
import EntityNames
import MMiSSVariant
import ViewType
import GHC.Base
import GHC.IOBase

type ExportFiles = 
   [(MMiSSPackageFolder.MMiSSPackageFolder,EntityNames.EntityFullName,
      MMiSSVariant.MMiSSVariantSearch)]

exportFiles :: 
   ViewType.View 
   -> GHC.Base.String 
   -> ExportFiles 
   -> GHC.IOBase.IO () 

