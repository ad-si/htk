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
import MMiSSDTDAssumptions

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
   includes :: [String], -- included entities (including ones in this document)
   children :: [StructuredContent], 
      -- included entities (in this XML document)
   links :: [String], -- linked entities
   references :: [String] -- referenced entities
   }

nullAccContents :: AccContents
nullAccContents = 
   AccContents {contents = [],includes = [],children = [],
      links = [],references = []}

addAccContents :: AccContents -> AccContents -> AccContents
addAccContents 
   (AccContents {
      contents = contents1,
      includes = includes1,
      children = children1,
      links = links1,
      references = references1})
   (AccContents {
      contents = contents2,
      includes = includes2,
      children = children2,
      links = links2,
      references = references2}) =
   AccContents {
      contents = contents1 ++ contents2,
      includes = includes1 ++ includes2,
      children = children1 ++ children2,
      links = links1 ++ links2,
      references = references1 ++ references2
      }

-- ----------------------------------------------------------------------
-- Structuring Contents
-- ----------------------------------------------------------------------

structureContents :: Element -> WithError StructuredContent
structureContents elem = case structureElement elem of
   Left (_,_,structuredContent) -> hasValue structuredContent
   Right _ -> hasError "Top-level in Xml does not have a label attribute"

---
-- For DirectInclude items, structureElement returns (a) the associated
-- label; (b) an Element referencing the item; (c) a StructuredContent
-- corresponding to the item.
--
-- For other items, we return AccContents corresponding to the content of
-- the item.
structureElement :: Element 
   -> Either (String,Element,StructuredContent) AccContents
structureElement (element @ (Elem tag attributes contents0)) =
   let
      accContentsl = map structureContent contents0
      accContents = foldr addAccContents nullAccContents accContentsl

      -- code for handling include/link/reference's.
      newElem = Elem tag attributes (contents accContents)

      wrapContents = Right (nullAccContents {contents = [CElem newElem]})

      addPointer newPtr =
         Right (addAccContents 
            newPtr (accContents {contents = [CElem newElem]})
            )
   in
      case classifyElement element of
         DirectInclude label element ->
            Left (
               label,
               element,
               StructuredContent {
                  tag = tag,
                  label = label,
                  attributes = attributes,
                  accContents = accContents
                  }
               )
         Include label -> addPointer (nullAccContents {includes = [label]})
         Reference label -> addPointer (nullAccContents {references = [label]})
         Link label -> addPointer (nullAccContents {links = [label]})
         Other -> wrapContents
           
structureContent ::  Content -> AccContents
structureContent content =
   case content of
      CElem element -> case structureElement element of
         Left (lab,element,structuredContent) -> 
               nullAccContents {
                  contents = [CElem element],
                  children = [structuredContent],
                  includes = [lab],
                  references = []
                  }
         Right contents -> contents
      other -> nullAccContents {contents = [other]}
 
