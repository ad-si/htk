module MMiSS.EmacsEdit where

import Types.ViewType as ViewType
import Types.Link as Link
import MMiSS.ObjectType as MMiSSObjectType
import GHC.IOBase

editMMiSSObjectXml :: 
   ViewType.View
   -> Link.Link MMiSSObjectType.MMiSSObject 
   -> GHC.IOBase.IO ()
editMMiSSObjectLaTeX :: 
   ViewType.View 
   -> Link.Link MMiSSObjectType.MMiSSObject 
   -> GHC.IOBase.IO ()
