module MMiSSExportFiles where

import MMiSSPackageFolder
import EntityNames
import MMiSSVariant
import ViewType

type ExportFiles = 
   [(MMiSSPackageFolder.MMiSSPackageFolder,EntityNames.EntityFullName,
      MMiSSVariant.MMiSSVariantSearch)]

exportFiles :: 
   ViewType.View 
   -> FilePath
   -> ExportFiles 
   -> IO () 

