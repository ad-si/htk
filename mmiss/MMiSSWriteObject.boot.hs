module MMiSSWriteObject where

#include "config.h"

#if HAXMLINT
#define ELEMENT Text.XML.HaXml.Types.Element
#else
#define ELEMENT XmlTypes.Element
#endif

writeToMMiSSObject :: 
   MMiSSObjectTypeType.MMiSSObjectType 
   -> ViewType.View 
   -> MMiSSPackageFolder.MMiSSPackageFolder   
   -> Data.Maybe.Maybe EntityNames.EntitySearchName 
   -> ELEMENT
   -> GHC.Base.Bool 
   -> GHC.IOBase.IO (Computation.WithError (
      Link.Link MMiSSObjectType.MMiSSObject,
      Data.Maybe.Maybe ELEMENT))

