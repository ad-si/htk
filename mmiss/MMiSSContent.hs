{- This module contains code for manipulating HaXml's Element and Content
   type. -}
module MMiSSContent(
   structureContents,
   StructuredContent(..),
   AccContents(..),
   ) where

import List

import XmlTypes

import Computation
import Dynamics

import CodedValue
import MMiSSAttributes

-- ------------------------------------------------------------------------
--
-- HasCodedValue instances
--
-- ------------------------------------------------------------------------

-- (1) Attributes

newtype XmlAttributes = XmlAttributes [Attribute]
attributes_tyRep = mkTyRep "MMiSSContent" "XmlAttributes"
instance HasTyRep XmlAttributes where
   tyRep _ = attributes_tyRep

instance HasCodedValue XmlAttributes where
   encodeIO (XmlAttributes xmlAttributes) codedValue view =
      do         
         attributesWE <- fromXmlAttributes view xmlAttributes
         encodeIO (coerceWithError attributesWE) codedValue view
   decodeIO codedValue0 view =
      do
         (attributes,codedValue1) <- decodeIO codedValue0 view
         xmlAttributes <- toXmlAttributes attributes
         return (XmlAttributes xmlAttributes,codedValue1)

-- (2) Misc
misc_tyRep = mkTyRep "MMiSSContent" "Misc"
instance HasTyRep Misc where
   tyRep _ = misc_tyRep

instance HasCodedValue Misc where
   encodeIO = mapEncodeIO 
      (\ misc -> case misc of
         Comment comment -> Left comment
         PI pi -> Right pi
         )
   decodeIO = mapDecodeIO 
      (\ either -> case either of
         Left comment -> Comment comment
         Right pi -> PI pi
         )

-- (4) Reference
reference_tyRef = mkTyRep "MMiSSContent" "Reference"
instance HasTyRep Reference where
   tyRep _ = reference_tyRef

instance HasCodedValue Reference where
   encodeIO = mapEncodeIO (\ ref -> case ref of
      RefEntity n1 -> Left n1
      RefChar n2 -> Right n2
      )
   decodeIO = mapDecodeIO (\ either -> case either of
      Left n1 -> RefEntity n1
      Right n2 -> RefChar n2
      )

-- (4) Content
content_tyRep = mkTyRep "MMiSSContent" "Content"
instance HasTyRep Content where
   tyRep _ = content_tyRep


type PackedContent = Choice5 Element (Bool,CharData) Reference Misc ()
toPackedContent :: Content -> PackedContent
toPackedContent content = case content of
   CElem elem -> Choice1 elem
   CString b cd -> Choice2 (b,cd)
   CRef r -> Choice3 r
   CMisc m -> Choice4 m

fromPackedContent :: PackedContent -> Content
fromPackedContent choice = case choice of
   Choice1 elem -> CElem elem
   Choice2 (b,cd) -> CString b cd
   Choice3 r -> CRef r
   Choice4 m -> CMisc m

instance HasCodedValue Content where
   encodeIO = mapEncodeIO toPackedContent
   decodeIO = mapDecodeIO fromPackedContent

-- (5) Element

element_tyRep = mkTyRep "MMiSSContent" "Element"
instance HasTyRep Element where
   tyRep _ = element_tyRep

instance HasCodedValue Element where
   encodeIO = mapEncodeIO 
      (\ (Elem name xmlAttributes contents) 
         -> (name,XmlAttributes xmlAttributes,contents))
   decodeIO = mapDecodeIO
      (\ (name,XmlAttributes xmlAttributes,contents) 
         -> Elem name xmlAttributes contents) 

-- ------------------------------------------------------------------------
-- Structuring Content data in input.
-- ------------------------------------------------------------------------

data StructuredContent = StructuredContent {
   tag :: String, -- The Xml Tag of the head of this object.
   label :: String, -- The object's label
   attributes :: [Attribute], -- The attributes, in Xml format
   accContents :: AccContents -- The data that comes from the children
   }

data AccContents = AccContents {
   contents :: [Content], -- what's inside the object
   includes :: [String], -- included entities (not in this XML document)
   children :: [StructuredContent], 
      -- included entities (in this XML document)
   references :: [String] -- referenced entities
   }

nullAccContents :: AccContents
nullAccContents = 
   AccContents {contents = [],includes = [],children = [],references = []}

addAccContents :: AccContents -> AccContents -> AccContents
addAccContents 
   (AccContents {
      contents = contents1,
      includes = includes1,
      children = children1,
      references = references1})
   (AccContents {
      contents = contents2,
      includes = includes2,
      children = children2,
      references = references2}) =
   AccContents {
      contents = contents1 ++ contents2,
      includes = includes1 ++ includes2,
      children = children1 ++ children2,
      references = references1 ++ references2
      }

getAttributes :: String -> [Attribute] -> Maybe String
getAttributes key attributes =
   case find (\ (name,attVal) -> (name == key)) attributes of
      Nothing -> Nothing
      Just (_,AttValue [Left s]) -> Just s
      Just _ -> error "MMiSSContent.getAttributes - irregular attribute"

-- ----------------------------------------------------------------------
-- Structuring Contents
-- ----------------------------------------------------------------------

structureContents :: Element -> StructuredContent
structureContents elem = case structureElement elem of
   Left structuredContent -> structuredContent
   Right _ -> error "Top-level in Xml does not have a label attribute"

---
-- If the "label" element is set we create a new StructuredContent item
-- otherwise we return an AccContents.
structureElement :: Element -> Either StructuredContent AccContents
structureElement (Elem tag attributes contents0) =
   let
      accContentsl = map structureContent contents0
      accContents = foldr addAccContents nullAccContents accContentsl
      labelOpt = getAttributes "label" attributes
   in
      case labelOpt of
         Just label ->
            Left (
               StructuredContent {
                  tag = tag,
                  label = label,
                  attributes = attributes,
                  accContents = accContents
                  }
               )
         Nothing ->
            let
               newElem = Elem tag attributes (contents accContents)
               (includes1,references1) =
                  case tag of
                     "include" -> 
                        case (
                           getAttributes "included" attributes,
                           getAttributes "status" attributes) of
                              (Just s,Just "present") -> ([s],[])
                              (Just s,_) -> ([],[])
                              (Nothing,_) -> error "included not specified"
                     "reference" -> 
                        case (
                           getAttributes "referenced" attributes,
                           getAttributes "status" attributes) of
                              (Just s,Just "present") -> ([s],[])
                              (Just s,_) -> ([],[])
                              (Nothing,_) -> error "referenced not specified"
                     _ -> ([],[])

               toAdd = AccContents {
                  contents = [CElem newElem],
                  includes = includes1,
                  references = [],
                  children = []
                  }

               newAccContents = addAccContents toAdd 
                  (accContents {contents = []})
            in
               Right newAccContents
           
structureContent ::  Content -> AccContents
structureContent content =
   case content of
      CElem element -> case structureElement element of
         Left structuredContent -> 
            let
               lab = label structuredContent
            in
               nullAccContents {
                  contents = [mkIncludeLink lab],
                  children = [structuredContent],
                  includes = [lab],
                  references = []
                  }
         Right contents -> contents
      other -> nullAccContents {contents = [other]}

---
-- Make an include reference which already exists
mkIncludeLink :: String -> Content
mkIncludeLink label 
   = CElem (Elem "include" [
      ("included",AttValue [Left label]),
      ("status",AttValue [Left "present"])
      ] [])
 
