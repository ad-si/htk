{- This module contains functions for editing Xml -}
module MMiSSEditXml(
   -- The first argument of each function is the file name.
   toEditableXml, -- :: String -> Element -> EmacsContent String
   fromEditableXml, -- :: String -> EmacsContent String 
     -- -> IO (WithError Element)
   parseXmlString, -- exported for debugging purposes
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
                  EmacsLink encodedLink ->
                     let
                        (detail,link) = decodeLink encodedLink
                     in
                        "<include"++detail++" included=\"" ++ link ++ 
                           "\" status=present/>"
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
      includeIndex = 0 -- position of include attribute name ("TextFragment"
         -- or whatever) in this.

      include1 = compile
         "\\`<include(\\w*)( |\t|\n)+included=\"(.*)\"( |\t|\n)+status=\"present\"/>"
      include1index = 2 -- position of included attribute in this.
         -- (\\w*) counts as 0 and ( |\n)+ as 1.
      
      include2 = compile
         "\\`<include(\\w*)( |\t|\n)+status=\"present\"( |\t|\n)+included=\"(.*)\"/>"
      include2index = 3

      --
      -- Argument 2 is the index of the included attribute
      doMatched :: MatchResult -> Int -> EmacsDataItem String
      doMatched matched includedAtt =
         EmacsLink (encodeLink 
            (getSubString matched includeIndex)
            (getSubString matched includedAtt)
            )

      recurse :: String -> [EmacsDataItem String]
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

      prepend :: Char -> [EmacsDataItem String] -> [EmacsDataItem String]
      prepend ch (EditableText t : rest) = EditableText (ch:t) : rest
      prepend ch other = EditableText [ch] : other
   in
      EmacsContent (recurse str)


---
-- Encoding details.  We need to encode the four sorts of
-- include, namely includeTextFragment/Group/Atom/Unit in the link name.
-- We do this by prefixing a letter T/G/A/U.
-- encodeLink and decodeLink do this.
encodeLink :: String -> String -> String
encodeLink includeType str =
   (case includeType of
      "TextFragment" -> 'T'
      "Group" -> 'G'
      "Atom" -> 'A'
      "Unit" -> 'U'
      _ -> error ("MMiSSEditXml.encodeLink: bad includeType "++includeType)
      ) : str

decodeLink :: String -> (String,String)
decodeLink "" = error ("MMiSSEditXml.decodeLink: empty link name")
decodeLink (i:str) =
   ((case i of
      'T' -> "TextFragment"
      'G' -> "Group"
      'A' -> "Atom"
      'U' -> "Unit"
      _ -> error ("MMiSSEditXml.decodeLink: bad type "++[i])
      ),str)
