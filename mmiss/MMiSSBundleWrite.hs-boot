module MMiSSBundleWrite where

import MMiSSBundle
import MMiSSInsertionPoint
import LaTeXParser
import ViewType
import Data.Maybe
import GHC.IOBase

writeBundle ::
   MMiSSBundle.Bundle
   -> Data.Maybe.Maybe LaTeXParser.PackageId
   -> Data.Maybe.Maybe GHC.IOBase.FilePath
   -> ViewType.View
   -> MMiSSInsertionPoint.InsertionPoint
   -> GHC.IOBase.IO ()