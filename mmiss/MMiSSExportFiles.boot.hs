module MMiSSExportFiles where

type ExportFiles = 
   [(MMiSSPackageFolder.MMiSSPackageFolder,EntityNames.EntityFullName,
      MMiSSVariant.MMiSSVariantSearch)]

exportFiles :: 
   ViewType.View 
   -> GHC.Base.String 
   -> ExportFiles 
   -> GHC.IOBase.IO () 

