-- | This module contains functions for editing Xml 
module MMiSSEditXml(
   -- The first argument of each function is the file name.
   toEditableXml, 
       -- :: String -> Element -> EmacsContent (TypedName,IncludeInfo) 
   fromEditableXml, -- :: String -> EmacsContent (TypedName,IncludeInfo) 
     -- -> IO (WithError Element)

   toExportableXml, -- :: Element -> String
      -- actually defined in MMiSSDTD.

   TypedName,

   ) where

import Maybe

import Computation

import EmacsContent

import Text.XML.HaXml.Types
import Text.XML.HaXml.Lex

import XmlExtras

import LaTeXParser(IncludeInfo(..),fromIncludeStrOpt)

import MMiSSDTD
import MMiSSDTDAssumptions
import MMiSSXmlBasics

type TypedName = (String,Char)

-- -------------------------------------------------------------------
-- toEditableXml
-- -------------------------------------------------------------------

toEditableXml :: String -> Element -> EmacsContent (TypedName,IncludeInfo) 
toEditableXml fName elem0 =
   -- This function functions using the following awful hacks.
   -- (1) We scan the element to find all included elements 
   --     and remember them, replacing them by special tag elements
   --     which have a "name" consisting of just an "X" character (which
   --     would be illegal in the DTD).
   -- (2) We print the result out to a string.
   -- (3) We tokenise the string again.
   -- (4) We scan for our dummy X characters in the tokens, and replace them
   --     by appropriate (TypedName,IncludeInfo) items.
   let
      includes :: [(TypedName,IncludeInfo)]
      includes = getIncludes elem0

      elem1 :: Element
      elem1 = stripIncludes elem0

      exportedString1 :: String
      exportedString1 = toExportableXml elem1

      tokens :: [Token]
      tokens = xmlLex "MMiSSEditXml.zoEditableXml" exportedString1 

      dummyElements :: [DummyElementLoc]
      dummyElements = extractDummyElements tokens

      mkContents :: Posn -> String -> [(TypedName,IncludeInfo)] 
         -> [DummyElementLoc] -> [EmacsDataItem (TypedName,IncludeInfo)]
      mkContents stringStart0 rest0 [] [] = addText rest0 []
      mkContents stringStart0 rest0 (include:includes1) (dummyLoc:dummyLocs1) =
         let
            start1 = start dummyLoc
            end1 = end dummyLoc

            (text,rest1) 
               = extractFromString stringStart0 (start dummyLoc) rest0

            (_,'/':'>':rest2) = extractFromString start1 end1 rest1
            stringStart1 = stepString end1 "/>"

            dataItems0 = mkContents stringStart1 rest2 includes1 dummyLocs1
            dataItems1 = (EmacsLink include):dataItems0
            dataItems2 = addText text dataItems1
         in
            dataItems2


      startPos :: Posn
      startPos = (Pn (error "MMiSSEditXml.1") 1 1 (error "MMiSSEditXml.2")) 

      dataItems :: [EmacsDataItem (TypedName,IncludeInfo)]
      dataItems = mkContents startPos exportedString1 includes dummyElements

      addText :: String -> [EmacsDataItem (TypedName,IncludeInfo)]
         -> [EmacsDataItem (TypedName,IncludeInfo)]
      addText "" dataItems = dataItems
      addText text dataItems = EditableText text : dataItems
   in
      EmacsContent dataItems


data DummyElementLoc = DummyElementLoc {
   start :: Posn,
   end :: Posn
   }

extractDummyElements :: [Token] -> [DummyElementLoc]
extractDummyElements tokens0 =
   case tokens0 of
      (startPosn,TokAnyOpen):(_,TokName "X"):(endPosn,TokEndClose):tokens1
         -> (DummyElementLoc {start = startPosn,end = endPosn})
            : extractDummyElements tokens1
      token:tokens1 -> extractDummyElements tokens1
      [] -> []

stripIncludes :: Element -> Element
stripIncludes element =
   mapElement
      (\ element -> case mapIncludeElement element of
         Nothing -> Nothing
         Just _ -> Just dummyElement
         )
      element

dummyElement :: Element
dummyElement = Elem "X" [] []

getIncludes :: Element -> [(TypedName,IncludeInfo)]
getIncludes elem = mapMaybe mapIncludeElement (getAllElements elem)

mapIncludeElement :: Element -> Maybe (TypedName,IncludeInfo)
mapIncludeElement elem = coerceWithError (mapIncludeElement1 elem)

mapIncludeElement1 :: Element -> WithError (Maybe (TypedName,IncludeInfo))
mapIncludeElement1 (Elem name attributes contents) =
   case name of 
      'i':'n':'c':'l':'u':'d':'e':name1 -> 
         do
            isPresent <- getAttribute attributes "status"
            case isPresent of
               Just "present" ->
                  do
                     variantOpt <- case contents of
                        [] -> return Nothing
                        [CElem variant] -> return (Just variant)
                        _ -> fail ("Include element does not contain a "
                           ++ "single variant-attributes element")

                     labelOpt <- getAttribute attributes "included"
                     label <- case labelOpt of
                        Nothing -> fail "Include does not have a label"
                        Just label -> return label

                     miniType <- case fromIncludeStrOpt name1 of
                        Just c -> return c
                        Nothing -> fail ("Unrecognised include tag" ++ name)
                      
                     let
                        -- Construct attributes in which "included" and
                        -- "status" are deleted (since they are used elsewhere)
                        otherAttributes = 
                           delAttribute (
                              delAttribute attributes "included"
                              ) "status"

                        includeInfo = IncludeInfo {
                           variantOpt = variantOpt,
                           otherAttributes = otherAttributes
                           }

                     return (Just ((label,miniType),includeInfo))
               _ -> return Nothing
      _ -> return Nothing

-- -------------------------------------------------------------------
-- fromEditableXml
-- -------------------------------------------------------------------

fromEditableXml :: String -> EmacsContent (TypedName,IncludeInfo) 
   -> IO (WithError Element)
fromEditableXml fName (EmacsContent dataItems) =
   do
      let
         xmlString = concat (
            map
               (\ dataItem -> case dataItem of
                  EditableText text -> text
                  EmacsLink include -> fromInclude include
                  )
               dataItems
            )
      xmlParseCheck fName xmlString 

fromInclude :: (TypedName,IncludeInfo) -> String
fromInclude ((link,miniType),includeInfo) =
   "<"
   ++ includeTagName
   ++ " included=\"" ++ link++"\""
   ++ " status=\"present\""
   ++ printAttributes (otherAttributes includeInfo)
   ++ ">"
   ++ variantString
   ++ "</"
   ++ includeTagName
   ++ ">"
   where
      includeTagName = "include" ++ toIncludeStr miniType

      variantString = case variantOpt includeInfo of
         Nothing -> ""
         Just variantAttributesElement 
            -> toUglyExportableXml variantAttributesElement
  
-- -------------------------------------------------------------------
-- Miscellaneous utilities.
-- -------------------------------------------------------------------

-- | extractFromString extractStart extractEnd string
-- assumes the string begins at position extractStart, extracts
-- the portion of it up until extractEnd-1, and
-- returns the string starting at extractEnd as well. 
extractFromString :: Posn -> Posn -> String -> (String,String)
extractFromString extractStart extractEnd string =
   case (comp extractStart extractEnd,string) of
      (EQ,_) -> ([],string)
      (LT,c:cs) ->
         let
            next = step extractStart c
            (extracted,rest) = extractFromString next extractEnd cs
         in
            (c:extracted,rest)
   where
      comp :: Posn -> Posn -> Ordering
      comp (Pn _ l1 c1 _) (Pn _ l2 c2 _) =
         case compare l1 l2 of
            LT -> LT
            GT -> GT
            EQ -> compare c1 c2

-- | Given that the start of s is at posn, return the position after s.
stepString :: Posn -> String -> Posn
stepString posn s = case s of
   [] -> posn
   (c:rest) -> stepString (step posn c) rest

-- | Given that the position at char is at posn, this returns
-- the position of the following character.  Each line is deemed to
-- end with its newline character.
step :: Posn -> Char -> Posn
step (Pn x l c y) ch =
   case ch of
      '\n' -> Pn x (l+1) 1 y
      _ -> Pn x l (c+1) y

