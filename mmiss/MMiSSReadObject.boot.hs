module MMiSSReadObject where

#include "config.h"

simpleReadFromMMiSSObject :: 
   ViewType.View 
   -> Link.Link MMiSSObjectType.MMiSSObject 
   -> MMiSSVariant.MMiSSVariantSearch
   -> GHC.IOBase.IO (Computation.WithError (
       MMiSSObjectType.Variable,MMiSSObjectType.MMiSSObject))

readMMiSSObject :: 
   ViewType.View 
   -> Link.Link MMiSSObjectType.MMiSSObject 
   -> Data.Maybe.Maybe MMiSSVariant.MMiSSVariantSearch
   -> IntPlus.IntPlus -> GHC.Base.Bool
   -> GHC.IOBase.IO (
         Computation.WithError (
#if HAXMLINT
            Text.XML.HaXml.Types.Element,
#else
            XmlTypes.Element,
#endif
            [(Link.Link MMiSSPreamble.MMiSSPreamble,LaTeXParser.MMiSSExtraPreambleData)]))

