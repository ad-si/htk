{- In this module we collect the various assumptions the Haskell code makes
   about the MMiSS DTD, which are not explicitly read from it. 

   See also MMiSSContent.hs.  But hopefully that is less dependent on the DTD
   itself. 
   -}
module MMiSSDTDAssumptions(
   findLabelledElements, -- :: DocTypeDecl -> [String]

   ClassifiedElement(..),
   classifyElement, -- :: Element -> WithError ClassifiedElement
   unclassifyElement, -- :: Element -> Maybe (String,Element -> WithError ())

   getMiniType, -- :: String -> Char
   toIncludeStr, -- :: Char -> String
   fromIncludeStr, -- :: String -> Char

   getLabel, -- :: Element -> String
      -- Get an Element's label.
   setLabel, -- :: Element -> String -> Element
      -- Set an Element's label. 
     
   getPath, -- :: Element -> WithError EntityPath
      -- Get an Element's path.  This must be a single String and defaults to 
      -- "."

   variantAttributes, -- :: [String]
   variantAttributesType, -- :: AttributesType

   printAttributes, -- :: [Attribute] -> String

   isPackageTag, -- :: String -> Bool
      -- Returns True if an element with this tag is to be treated as a 
      -- package.
   ) where

#include "config.h"

import Maybe
import Char

import ExtendedPrelude
import Computation
import AtomString

#if HAXMLINT
import Text.XML.HaXml.Types
#else
import XmlTypes
#endif

import AttributesType

import EntityNames

import LaTeXParser

---
-- How to distinguish those elements in the DTD which can be displayed
-- separately in the development graph.
-- Rule: those which have an attribute "label". 
findLabelledElements :: DocTypeDecl -> [String]
findLabelledElements (DTD _ _ markups) =
   catMaybes
      (map
         (\ markup -> case markup of
            AttList (AttListDecl name attDefs) -> 
               findJust
                  (\  (AttDef key _ _) ->
                     if key == "label" then Just name else Nothing
                     )
                  attDefs
            _ -> Nothing
            )
         markups
         )

---
-- This contains the output of classifyElement, which describes how the 
-- supplied Element should be structured and, if necessary, replaced.
--
-- Absent links get classified as "Other"; this is because the user is
-- supposed to explicitly change them to "present" before we attempt to
-- resolve them.
data ClassifiedElement =
      Link EntityFullName --  present link to entity with this label
   |  Reference EntityFullName -- present reference to entity with this label
   |  Include EntityFullName 
         -- present include of an entity with this label.  The 
         -- element itself was the import
   |  DirectInclude EntityFullName Element
         -- The Element was an entity with this label.  The Element returned
         -- is an include pointer to it.
   |  Other

classifyElement :: Element -> WithError ClassifiedElement
classifyElement (Elem name attributes content) =
-- The various assumptions made by this function about attributes being 
-- present should be enforced by the DTD.
   let
      -- abbreviations
      getAtt key = getAttribute attributes key
      

      cErr str = error ("MMiSSDTDAssumptions.classifyElement: "++str)
      -- cErr should only be used for "This-can't-happen" errors, so things
      -- that should not happen if XML matched the DTD.

      -- General code for handling includes/links/references.  It assumes
      -- the attribute for the referenced object (the first argument) exists.
      generalRef :: String -> (EntityFullName -> ClassifiedElement) 
         -> WithError ClassifiedElement
      generalRef refName constructor =
         case getAtt refName of
            Just linkedString -> 
               -- We check that the link is an EntityFullName 
               -- even if the link is absent
               mapWithError
                  (\ linked -> case getAtt "status" of
                     Just "present" -> constructor linked
                     Just "absent" -> Other
                     Nothing -> Other
                     )
                  (fromStringWE linkedString)
            Nothing -> cErr (refName++" attribute is not defined")

      -- Code for links.  We assume the attribute "linked" refers to the
      -- linked object
      link = generalRef "linked" Link

      -- Code for references.  We assume the attribute "referenced" refers to
      -- the referenced object.
      reference = generalRef "referenced" Reference

      -- Includes.  We assume the "included" and "status" attributes exist
      include = generalRef "included" Include

      -- Direct Includes.  These are most complicated as they involve
      -- converting the included element into a reference to it.
      --
      -- Such converted elements should be valid, wherever the original 
      -- element is, except that they do not need to be valid at the top-level
      -- of a document.
     
      -- How to convert.
      --    If there is no "label" attribute defined, Other is returned.
      --
      --
      -- Rules for mapping entity names: encoded by mapLabelledTag.
      --
      --    The returned element has an Xml tag taken from the corresponding
      --    RHS, with empty content and two attributes.  First attribute is 
      --    "included" pointing to the value of the supplied label attribute.  
      --    Second attribute is "status=present".
      mkInclude includeChar = case getAtt "label" of
         Nothing -> hasValue Other
         Just labelString ->
            mapWithError (\ label ->
               DirectInclude label 
                  (Elem ("include" ++ toIncludeStr includeChar) [
                  ("included",AttValue [Left labelString]),
                  ("status",AttValue [Left "present"])
                  ] [])
               )
               (fromStringWE labelString)

   in
      case name of
         -- Links
         "link" -> link

         -- References
         "reference" -> reference

         -- Includes
         'i':'n':'c':'l':'u':'d':'e':_ -> include

         _ -> case classifyLabelledTag name of
            Just c -> mkInclude c
            Nothing -> hasValue Other

---
-- We also use classifyLabelledTag to get the mini-type-letter, used by
-- EmacsEdit.hs to colour magic buttons appropriately.
getMiniType :: String -> Char
getMiniType str = case classifyLabelledTag str of
   Just c -> c
   Nothing -> error ("Attempt to edit object with unclassifiable tag "++str)


---
-- Returns True if an element with this tag is to be treated as a package.
isPackageTag :: String -> Bool
isPackageTag s = (s == "package")


-- ----------------------------------------------------------------------
-- Turning a (possible) link back into an element
-- ----------------------------------------------------------------------

---
-- Given an Element which is an include, returns the contained label,
-- the specific attributes for the include, and a 
-- function which verifies that the given Element matches.  For other elements,
-- returns Nothing.
unclassifyElement :: Element -> Maybe (String,[Attribute],
   Element -> WithError ())
unclassifyElement (Elem name attributes _) =
   let
      includeSort :: Maybe Char
      includeSort =
         case name of
            'i':'n':'c':'l':'u':'d':'e': rest -> Just (fromIncludeStr rest)
            _ -> Nothing
   in
      fmap
         (\ tag ->
            (
               fromMaybe 
                  (error ("MMiSSDTDAssumptions: "++name
                     ++" element has no included attribute"))
                  (getAttribute attributes "included"),
               attributes,
               (\ (Elem newName _ _) ->
                  if includeSort == classifyLabelledTag newName
                     then
                        hasValue ()
                     else
                        hasError ("The element "++newName
                           ++" does not match the reference "++name)
                  )
               )
            )
         includeSort

-- ----------------------------------------------------------------------
-- Functions for operating on attributes
-- ----------------------------------------------------------------------

---
-- Extract an attribute, which had better be a single String
getAttribute :: [Attribute] -> String -> Maybe String
getAttribute attributes key =
   findJust
      (\ (key2,value) ->
            if key == key2 
               then 
                  case value of
                     AttValue [Left name] -> Just name
                     _ -> error ("MMiSSDTDAssumptions: attribute "++key++
                        " has inappropriate type")
               else
                  Nothing
         )
      attributes

---
-- Get an Element's label, assuming it was of DirectInclude type.
getLabel :: Element -> WithError EntityFullName
getLabel element = 
   mapWithError' 
      (\ classified -> case classified of
         (DirectInclude label _) -> hasValue label
         _ -> hasError "Element has no label!"
         )
      (classifyElement element)

---
-- Set an Element's label, assuming it was of DirectInclude type.
setLabel :: Element -> EntityFullName -> Element
setLabel (Elem name attributes0 content) entityFullName =
   let
      attributes1 = deleteFirst (\ (key,_) -> key == "label") attributes0
      attributes2 = ("label",AttValue [Left (toString entityFullName)])
         : attributes1
   in
      Elem name attributes2 content
    
---
-- Get an Element's path.  This must be a single String and defaults to "."
getPath :: Element -> WithError EntityPath
getPath (Elem xmlTag attributes _) = 
   let
      pathString = fromMaybe "." (getAttribute attributes "path")
   in
      fromStringWE pathString 

-- ----------------------------------------------------------------------
-- Variant Attributes
-- These should be defined in the DTD for each labelled element.
-- NB.  At the moment variantAttributesType just assumes that each
-- variant attribute has type String; this needs to be improved at some
-- point.
-- ----------------------------------------------------------------------

-- All the attributes we provide are the same.
variantAttributes :: [String]
variantAttributes = 
   ["xml:lang","levelOfDetail","interactionLevel"]

variantAttributesType :: AttributesType
variantAttributesType =
   foldl
      (\ attType name -> needs (mkAttributeKey name) "" attType)
      emptyAttributesType
      variantAttributes



-- ----------------------------------------------------------------------
-- Display/Parse attributes in Xml format
-- If the list is non-empty, we also prepend a space.
-- We assume the attribute values are just single strings.
-- ----------------------------------------------------------------------

printAttributes :: [Attribute] -> String
printAttributes attributes =
   concatMap
      (\ (name,attValue) -> case attValue of
         AttValue [Left value] -> 
            " "++name++"="++"\""++value++"\""
            )
      attributes


   