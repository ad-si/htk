module MMiSSBundleWrite where

writeBundle ::
   MMiSSBundle.Bundle
   -> Data.Maybe.Maybe LaTeXParser.PackageId
   -> Data.Maybe.Maybe GHC.IOBase.FilePath
   -> ViewType.View
   -> MMiSSInsertionPoint.InsertionPoint
   -> GHC.IOBase.IO ()