module MMiSSEmacsEdit where

import ViewType
import Link
import MMiSSObjectType
import GHC.IOBase

editMMiSSObjectXml :: 
   ViewType.View
   -> Link.Link MMiSSObjectType.MMiSSObject 
   -> GHC.IOBase.IO ()
editMMiSSObjectLaTeX :: 
   ViewType.View 
   -> Link.Link MMiSSObjectType.MMiSSObject 
   -> GHC.IOBase.IO ()
