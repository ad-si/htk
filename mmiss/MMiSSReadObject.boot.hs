module MMiSSReadObject where

readMMiSSObject :: 
   ViewType.View 
   -> Link.Link MMiSSObjectType.MMiSSObject 
   -> Data.Maybe.Maybe MMiSSVariant.MMiSSVariantSearch
   -> IntPlus.IntPlus -> GHC.Base.Bool
   -> GHC.IOBase.IO (
         Computation.WithError (
            Text.XML.HaXml.Types.Element,
            [MMiSSPackageFolder.MMiSSPackageFolder],
            MMiSSExportFiles.ExportFiles
            )
         )

