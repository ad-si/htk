{- This module contains those assumptions which files in this directory need to
   make about the format of MMiSSLaTeX. -}
module MMiSSLaTeXAssumptions(
   mkLaTeXString, 
      -- :: EmacsContent (TypedName,[Attribute]) -> String
      -- NB.  This assumes each value of [Attribute] is of the form
      -- AttValue [Left String], in other words is a single String.
   ) where

import ExtendedPrelude

import EmacsContent

import Text.XML.HaXml.Types

import MMiSSDTDAssumptions

mkLaTeXString :: EmacsContent ((String,Char),[Attribute]) -> String
mkLaTeXString (EmacsContent dataItems) =
   concatMap
      (\ dataItem -> case dataItem of
         EditableText str -> str
         EmacsLink ((included,ch),attributes) -> 
            "\\Include"
            ++ toIncludeStr ch
            ++ "{" ++ included ++ "}"
            ++ toLaTeXAttributes (attributes ++ [statusAttribute])
         )     
      dataItems

toLaTeXAttributes :: [Attribute] -> String
toLaTeXAttributes [] = ""
toLaTeXAttributes attributes =
   "{"
   ++ unsplitByChar ',' (map
      (\ (name,attValue) ->
         case attValue of
            AttValue [Left value] -> name ++ "=" ++ value
         )
      attributes   
      )
   ++
   "}"

statusAttribute :: Attribute
statusAttribute = ("status",AttValue [Left "present"])