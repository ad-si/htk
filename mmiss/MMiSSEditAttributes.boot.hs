module MMiSSEditAttributes where


editObjectAttributes :: 
   ViewType.View 
   -> Link.Link MMiSSObjectType.MMiSSObject 
   -> GHC.IOBase.IO ()
