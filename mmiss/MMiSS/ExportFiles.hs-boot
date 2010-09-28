module MMiSS.ExportFiles where

import MMiSS.PackageFolder as MMiSSPackageFolder
import Imports.EntityNames as EntityNames
import MMiSS.Variant as MMiSSVariant
import Types.ViewType as ViewType

type ExportFiles = 
   [(MMiSSPackageFolder.MMiSSPackageFolder,EntityNames.EntityFullName,
      MMiSSVariant.MMiSSVariantSearch)]

exportFiles :: 
   ViewType.View 
   -> FilePath
   -> ExportFiles 
   -> IO () 

