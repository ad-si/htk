{- This module contains code for manipulating HaXml's Element and Content
   type. -}
module MMiSSContent(
   structureContents,
   StructuredContent(..),
   AccContents(..),
   LinkType(..),
   ) where

#include "config.h"

import List

import Computation
import Dynamics

import CodedValue
import EntityNames

#if HAXMLINT
import Text.XML.HaXml.Types
#else
import XmlTypes
#endif

import MMiSSAttributes
import MMiSSDTDAssumptions
import MMiSSVariant

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

-- (6) LinkType
linkType_tyRep = mkTyRep "MMiSSContent" "LinkType"
instance HasTyRep LinkType where
   tyRep _ = linkType_tyRep

instance HasCodedValue LinkType where
   encodeIO = mapEncodeIO (\ linkType -> case linkType of
      IncludeLink -> 'I'
      ReferenceLink -> 'R'
      LinkLink -> 'L'
      )
   decodeIO = mapDecodeIO (\ char -> case char of
      'I' -> IncludeLink
      'R' -> ReferenceLink
      'L' -> LinkLink
      _ -> error ("MMiSSContent: unexpected char "++show char)
      )

-- ------------------------------------------------------------------------
-- Structuring Content data in input.
-- ------------------------------------------------------------------------

data StructuredContent = StructuredContent {
   tag :: String, -- The Xml Tag of the head of this object.
   label :: EntityFullName, -- The object's label
   path :: EntityPath, -- The path, if specified.
   variantSpec :: MMiSSVariantSpec,
      -- The variants of this object.
   attributes :: [Attribute],
      -- The objects attributes, in HaXml format.
   accContents :: AccContents -- The data that comes from the children
   }

---
-- Type of a link/reference/include from a document.
data LinkType = IncludeLink | LinkLink | ReferenceLink

data AccContents = AccContents {
   contents :: [Content], -- what's inside the object
   children :: [StructuredContent],
      -- other nodes split off from this one.
   links :: [(EntityFullName,MMiSSVariantSearch,LinkType)] 
      -- all included, linked or referenced entities in this object in
      -- order.
      --
      -- For documents which are split into more than one StructuredContent
      -- links contains the links into this particular part, including links
      -- to the (split off) children.
   }

nullAccContents :: AccContents
nullAccContents = AccContents {contents = [],children = [],links = []}

addAccContents :: AccContents -> AccContents -> AccContents
addAccContents 
   (AccContents {
      contents = contents1,
      children = children1,
      links = links1})
   (AccContents {
      contents = contents2,
      children = children2,
      links = links2}) =
   AccContents {
      contents = contents1 ++ contents2,
      children = children1 ++ children2,
      links = links1 ++ links2}

-- ----------------------------------------------------------------------
-- Structuring Contents
-- ----------------------------------------------------------------------

structureContents :: Element -> WithError StructuredContent
structureContents element = 
   mapWithError'
      (\ structured -> case structured of
         Left (_,_,structuredContent) -> hasValue structuredContent
         Right _ -> hasError "Top-level in Xml does not have a label attribute"
         )
      (structureElement element)

---
-- For DirectInclude items, structureElement returns (a) the associated
-- label; (b) an Element referencing the item; (c) a StructuredContent
-- corresponding to the item.
--
-- For other items, we return AccContents corresponding to the content of
-- the item.
structureElement :: Element 
   -> WithError (Either (EntityFullName,Element,StructuredContent) AccContents)
structureElement (element @ (Elem tag attributes contents0)) =
   (flip mapWithError')
      (concatWithError (map structureContent contents0))
      (\ accContentsl ->
         let
            accContents = foldr addAccContents nullAccContents accContentsl

            -- code for handling include/link/reference's.
            newElem = Elem tag attributes (contents accContents)

            wrapContents = 
               hasValue (Right (accContents {contents = [CElem newElem]}))

            addPointer label linkType =
               hasValue (Right (AccContents {
                     contents = [CElem newElem],
                     children = [],
                     links = [(label,emptyMMiSSVariantSearch,linkType)]
                     }))

            variantSpec = toMMiSSVariantSpecFromXml attributes
         in
            (flip mapWithError')
               (pairWithError (classifyElement element) (getPath element))
               (\ (classifiedElement,path) ->
                  case classifiedElement of
                     DirectInclude label element ->
                        hasValue (Left (
                           label,
                           element,
                           StructuredContent {
                              tag = tag,
                              label = label,
                              path = path,
                              variantSpec = variantSpec,
                              attributes = attributes,
                              accContents = accContents
                              }
                           ))
                     Include label -> addPointer label IncludeLink
                     Reference label -> addPointer label ReferenceLink
                     Link label -> addPointer label LinkLink
                     Other -> wrapContents
                  )
         )
           
structureContent ::  Content -> WithError AccContents
structureContent content =
   case content of
      CElem element -> 
         (flip mapWithError)
            (structureElement element)
            (\ structuredElement -> case structuredElement of
               Left (lab,element,structuredContent) -> 
                     AccContents {
                        contents = [CElem element],
                        children = [structuredContent],
                        links = [(lab,emptyMMiSSVariantSearch,IncludeLink)]
                        }
               Right contents -> contents
               )
      other -> hasValue (nullAccContents {contents = [other]})
 
