module MMiSS.EditAttributes where

import Types.ViewType as ViewType
import Types.Link as Link
import MMiSS.ObjectType as MMiSSObjectType
import GHC.IOBase

editObjectAttributes :: 
   ViewType.View 
   -> Link.Link MMiSSObjectType.MMiSSObject 
   -> GHC.IOBase.IO ()
