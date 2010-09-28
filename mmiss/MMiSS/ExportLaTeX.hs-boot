module MMiSS.ExportLaTeX where

import Types.ViewType as ViewType
import Types.Link as Link
import MMiSS.ObjectType as MMiSSObjectType
import Reactor.ReferenceVariables as ReferenceVariables
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
