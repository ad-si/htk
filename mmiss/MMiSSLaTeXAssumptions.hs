{- This module contains those assumptions which files in this directory need to
   make about the format of MMiSSLaTeX. -}
module MMiSSLaTeXAssumptions(
   mkLaTeXString, 
      -- :: EmacsContent (TypedName,[Attribute]) -> String
      -- NB.  This assumes each value of [Attribute] is of the form
      -- AttValue [Left String], in other words is a single String.
   ) where

#include "config.h"

import ExtendedPrelude

import EmacsContent

#if HAXMLINT
import Text.XML.HaXml.Types
#else
import XmlTypes
#endif

import MMiSSDTDAssumptions

mkLaTeXString :: EmacsContent ((String,Char),[Attribute]) -> String
mkLaTeXString (EmacsContent dataItems) =
   concatMap
      (\ dataItem -> case dataItem of
         EditableText str -> str
         EmacsLink ((included,ch),attributes) -> 
            "\\Include"
            ++ toIncludeStr ch
            ++ toLaTeXAttributes attributes
            ++ "{" ++ included ++ "}{status=present}"
         )     
      dataItems

toLaTeXAttributes :: [Attribute] -> String
toLaTeXAttributes [] = ""
toLaTeXAttributes attributes =
   "["
   ++ unsplitByChar ',' (map
      (\ (name,attValue) ->
         case attValue of
            AttValue [Left value] -> value
         )
      attributes   
      )
   ++
   "]"