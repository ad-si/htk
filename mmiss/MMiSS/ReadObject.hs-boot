module MMiSS.ReadObject where

import Types.ViewType as ViewType
import Types.Link as Link
import MMiSS.ObjectType as MMiSSObjectType
import MMiSS.Variant as MMiSSVariant
import MMiSS.PackageFolder as MMiSSPackageFolder
import MMiSS.ExportFiles as MMiSSExportFiles
import Util.Computation as Computation
import Util.IntPlus as IntPlus
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

