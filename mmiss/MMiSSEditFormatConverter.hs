{- We define functions which convert an object into a text form in a format
   specified by MMiSSFormat.hs -}
module MMiSSEditFormatConverter(
   EditFormatConverter(..), -- The functions provided
   toEditFormatConverter, -- :: Format -> EditFormatConverter

   exportElement,
      -- :: View -> Format -> [Link MMiSSPreamble] -> Element 
      -- -> IO (WithError String)
   ) where

#include "config.h"

import Maybe

import Data.Set

import Computation 
import Debug(debugString)

import ViewType
import Link

import EmacsContent

#if HAXMLINT
import Text.XML.HaXml.Types
#else
import XmlTypes
#endif

import LaTeXParser

import MMiSSEditXml(TypedName)
import MMiSSFormat
import MMiSSEditXml
import MMiSSLaTeXAssumptions
import MMiSSPreamble

-- ----------------------------------------------------------------------
-- The types
-- ----------------------------------------------------------------------

---
-- For EditFormatConvert, the String's are the file name (made available
-- for error messages).
--
data EditFormatConverter = EditFormatConverter {
   toEdit :: String -> Element 
      -> WithError (EmacsContent (TypedName,[Attribute])),
   fromEdit :: String -> EmacsContent (TypedName,[Attribute]) 
      -> IO (WithError Element)
   }

-- ----------------------------------------------------------------------
-- The functions
-- ----------------------------------------------------------------------

toEditFormatConverter :: Format -> EditFormatConverter
toEditFormatConverter XML = 
   EditFormatConverter {
      toEdit = (\ str elem -> hasValue (toEditableXml str elem)),
      fromEdit = fromEditableXml
      }
toEditFormatConverter LaTeX =
   EditFormatConverter {
      toEdit = (\ preambleOpt element -> makeMMiSSLatex (element,False,[])),
      fromEdit = (\ string content 
         ->
            do
               let 
                  str = mkLaTeXString content

--             debugString ("START|"++str++"|END")

               return (mapWithError 
                  (\ (element,_,_) -> element) 
                  (parseMMiSSLatex str)
                  )
            )
      }


exportElement :: View -> Format -> [Link MMiSSPreamble] -> Element 
   -> IO (WithError String)
exportElement _ XML _ element = return (hasValue (toExportableXml element))
exportElement view LaTeX preambleLinks0 element =
   do
      -- Remove duplicate preamble links
      let 
         preambleLinks1 = setToList (mkSet preambleLinks0)

      laTeXPreambles <- mapM
         (\ preambleLink -> readPreamble view preambleLink)
         preambleLinks1

      return (
         mapWithError 
            (\ content -> mkLaTeXString content)
            (makeMMiSSLatex (element,True,laTeXPreambles))
         )

      
