module MMiSSExportFiles where

type ExportFiles = 
   [(MMiSSPackageFolder.MMiSSPackageFolder,GHC.Base.String,
      MMiSSVariant.MMiSSVariantSearch)]

exportFiles :: 
   ViewType.View 
   -> GHC.Base.String 
   -> ExportFiles 
   -> GHC.IOBase.IO () 

