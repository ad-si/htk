module MMiSSExportLaTeX where

import ViewType
import Link
import MMiSSObjectType
import ReferenceVariables
import GHC.Base
import GHC.IOBase


exportMMiSSObjectLaTeX :: 
   ViewType.View 
   -> Link.Link MMiSSObjectType.MMiSSObject 
   -> GHC.IOBase.IO ()

exportMMiSSObjectXML :: 
   ViewType.View 
   -> Link.Link MMiSSObjectType.MMiSSObject 
   -> GHC.IOBase.IO ()

pathRef :: ReferenceVariables.Ref GHC.Base.String