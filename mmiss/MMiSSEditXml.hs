{- This module contains functions for editing Xml -}
module MMiSSEditXml(
   -- The first argument of each function is the file name.
   toEditableXml, -- :: String -> Element -> EmacsContent String
   fromEditableXml, -- :: String -> EmacsContent String 
     -- -> IO (WithError Element)
   ) where

import XmlPP
import XmlTypes
import Pretty

import Computation
import ExtendedPrelude
import RegularExpression

import EmacsContent

import MMiSSDTD

fromEditableXml :: String -> EmacsContent String -> IO (WithError Element)
fromEditableXml fName (EmacsContent dataItems) =
   do
      let
         xmlString = concat (
            map
               (\ dataItem -> case dataItem of
                  EditableText text -> text
                  EmacsLink link ->
                     "<include included=\"" ++ link ++ "\" status=present/>"
                  )
               dataItems
            )
      xmlParseCheck fName xmlString 

toEditableXml :: String -> Element -> EmacsContent String
toEditableXml fName elem =
   let
      xmlString = render (element elem)
   in
      parseXmlString xmlString


---
-- This function is split off partly for debugging purposes.
-- It has to parse Xml Strings produced by HaXml's pretty-printing
-- and extract present includes.
parseXmlString :: String -> EmacsContent String
parseXmlString str =
   let
      -- Regular Expression for includes as defined by 
      -- util/RegularExpression.  NB that double slashes must be
      -- double-escaped
      include1 = compile
         "\\`<include( |\n)+included=\"(.*)\"( |\n)+status=\"present\"/>"
      include1index = 1 -- position of included attribute in this.
         -- ( |\n)+ counts as 0.
      
      include2 = compile
         "\\`<include( |\n)+status=\"present\"( |\n)+included=\"(.*)\"/>"
      include2index = 2

      recurse :: String -> [EmacsDataItem String]
      recurse [] = []
      recurse (str @ ('<':cs)) = 
         case matchString include1 str of
            Just matched -> (EmacsLink (getSubString matched include1index)) :
               recurse (getAfter matched)
            Nothing -> case matchString include2 str of
               Just matched -> (EmacsLink (getSubString matched include2index))
                  : recurse (getAfter matched)
               Nothing -> prepend '<' (recurse cs)
      recurse (c:cs) = prepend c (recurse cs)

      prepend :: Char -> [EmacsDataItem String] -> [EmacsDataItem String]
      prepend ch (EditableText t : rest) = EditableText (ch:t) : rest
      prepend ch other = EditableText [ch] : other
   in
      EmacsContent (recurse str)