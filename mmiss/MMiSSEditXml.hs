{- This module contains functions for editing Xml -}
module MMiSSEditXml(
   -- The first argument of each function is the file name.
   toEditableXml, -- :: String -> Element -> EmacsContent TypedName
   fromEditableXml, -- :: String -> EmacsContent TypedName 
     -- -> IO (WithError Element)
   parseXmlString, -- exported for debugging purposes

   toExportableXml, -- :: Element -> String

   ) where

#include "config.h"

import Computation
import ExtendedPrelude
import RegularExpression
import Debug(debug)

import EmacsContent
import EmacsEdit (TypedName)

#if HAXMLINT
import Text.XML.HaXml.Types
import Text.XML.HaXml.Pretty
import Text.PrettyPrint.HughesPJ hiding (char)
#else
import XmlTypes
import XmlPP
import Pretty
#endif

import MMiSSDTD
import MMiSSDTDAssumptions

fromEditableXml :: String -> EmacsContent TypedName -> IO (WithError Element)
fromEditableXml fName (EmacsContent dataItems) =
   do
      let
         xmlString = concat (
            map
               (\ dataItem -> case dataItem of
                  EditableText text -> text
                  EmacsLink (link,miniType) ->
                        "<include"++toIncludeStr miniType
                           ++" included=\"" ++ link
                           ++"\" status=\"present\"/>"
                  )
               dataItems
            )
      xmlParseCheck fName xmlString 

toEditableXml :: String -> Element -> EmacsContent TypedName
toEditableXml fName elem =
   let
      xmlString = toExportableXml elem
   in
      parseXmlString xmlString

toExportableXml :: Element -> String
toExportableXml elem = render (element elem)

---
-- This function is split off partly for debugging purposes.
-- It has to parse Xml Strings produced by HaXml's pretty-printing
-- and extract present includes.
parseXmlString :: String -> EmacsContent TypedName
parseXmlString str =
   let
      -- Regular Expression for includes as defined by 
      -- util/RegularExpression.  NB that double slashes must be
      -- double-escaped
      includeIndex = 0 -- position of include attribute name ("TextFragment"
         -- or whatever) in this.

      include1 = compile
         "\\`<include(\\w*)( |\t|\n)+included=\"(.*)\"( |\t|\n)+status=\"present\"( |\t|\n)*/>"
      include1index = 2 -- position of included attribute in this.
         -- (\\w*) counts as 0 and ( |\n)+ as 1.
      
      include2 = compile
         "\\`<include(\\w*)( |\t|\n)+status=\"present\"( |\t|\n)+included=\"(.*)\"( |\t|\n)*/>"
      include2index = 3

      --
      -- Argument 2 is the index of the included attribute
      doMatched :: MatchResult -> Int -> EmacsDataItem TypedName
      doMatched matched includedAtt =
         EmacsLink (
            getSubString matched includedAtt,
            fromIncludeStr (getSubString matched includeIndex)
            )

      recurse :: String -> [EmacsDataItem TypedName]
      recurse [] = []
      recurse (str @ ('<':cs)) = 
         case matchString include1 str of
            Just matched -> doMatched matched include1index :
               recurse (getAfter matched)
            Nothing -> case matchString include2 str of
               Just matched -> doMatched matched include2index :
                  recurse (getAfter matched)
               Nothing -> prepend '<' (recurse cs)
      recurse (c:cs) = prepend c (recurse cs)

      prepend :: Char -> [EmacsDataItem TypedName] -> [EmacsDataItem TypedName]
      prepend ch (EditableText t : rest) = EditableText (ch:t) : rest
      prepend ch other = EditableText [ch] : other
   in
      EmacsContent (recurse str)

