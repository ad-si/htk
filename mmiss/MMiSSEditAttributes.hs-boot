module MMiSSEditAttributes where

import ViewType
import Link
import MMiSSObjectType
import GHC.IOBase

editObjectAttributes :: 
   ViewType.View 
   -> Link.Link MMiSSObjectType.MMiSSObject 
   -> GHC.IOBase.IO ()
