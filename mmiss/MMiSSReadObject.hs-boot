module MMiSSReadObject where

import ViewType
import Link
import MMiSSObjectType
import MMiSSVariant
import MMiSSPackageFolder
import MMiSSExportFiles
import Computation
import IntPlus
import Text.XML.HaXml.Types
import Data.Maybe
import GHC.Base
import GHC.IOBase

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

