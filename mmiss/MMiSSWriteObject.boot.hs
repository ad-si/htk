module MMiSSWriteObject where

#include "config.h"

#if HAXMLINT
#define ELEMENT Text.XML.HaXml.Types.Element
#else
#define ELEMENT XmlTypes.Element
#endif


writeToMMiSSObject :: 
   Link.Link MMiSSPreamble.MMiSSPreamble 
   -> MMiSSObjectTypeType.MMiSSObjectType 
   -> ViewType.View 
   -> LinkManager.LinkedObject
   -> Data.Maybe.Maybe EntityNames.EntityFullName 
   -> ELEMENT
   -> GHC.Base.Bool 
   -> GHC.IOBase.IO (Computation.WithError (
      Link.Link MMiSSObjectType.MMiSSObject,
      Data.Maybe.Maybe ELEMENT))

