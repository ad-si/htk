module MMiSSWriteObject where

#include "config.h"

writeToMMiSSObject :: 
   Link.Link MMiSSPreamble.MMiSSPreamble 
   -> MMiSSObjectTypeType.MMiSSObjectType 
   -> ViewType.View 
   -> LinkManager.LinkedObject
   -> Data.Maybe.Maybe EntityNames.EntityFullName 
#if HAXMLINT
   -> Text.XML.HaXml.Types.Element
#else
   -> XmlTypes.Element
#endif
   -> GHC.Base.Bool 
   -> GHC.IOBase.IO (Computation.WithError (
      Link.Link MMiSSObjectType.MMiSSObject))

