module MMiSSExportLaTeX where


exportMMiSSObjectLaTeX :: 
   ViewType.View 
   -> Link.Link MMiSSObjectType.MMiSSObject 
   -> GHC.IOBase.IO ()

exportMMiSSObjectXML :: 
   ViewType.View 
   -> Link.Link MMiSSObjectType.MMiSSObject 
   -> GHC.IOBase.IO ()

pathRef :: ReferenceVariables.Ref GHC.Base.String