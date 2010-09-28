module MMiSS.BundleWrite where

import MMiSS.Bundle as MMiSSBundle
import MMiSS.InsertionPoint as MMiSSInsertionPoint
import MMiSS.LaTeX.Parser as LaTeXParser
import Types.ViewType as ViewType
import Data.Maybe
import GHC.IOBase

writeBundle ::
   MMiSSBundle.Bundle
   -> Data.Maybe.Maybe LaTeXParser.PackageId
   -> Data.Maybe.Maybe GHC.IOBase.FilePath
   -> ViewType.View
   -> MMiSSInsertionPoint.InsertionPoint
   -> GHC.IOBase.IO ()
