module MMiSSImportLaTeX where

importMMiSSLaTeX :: 
   Link.Link MMiSSPreamble.MMiSSPreamble 
   -> MMiSSObjectTypeType.MMiSSObjectType 
   -> ViewType.View
   -> (EntityNames.EntityName 
      -> GHC.IOBase.IO (
         Computation.WithError MMiSSPackageFolder.MMiSSPackageFolder)) 
   -> GHC.IOBase.IO (Data.Maybe.Maybe (Link.Link MMiSSObjectType.MMiSSObject))
