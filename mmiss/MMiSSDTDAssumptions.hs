{- In this module we collect the various assumptions the Haskell code makes
   about the MMiSS DTD, which are not explicitly read from it. 

   See also MMiSSContent.hs.  But hopefully that is less dependent on the DTD
   itself. 
   -}
module MMiSSDTDAssumptions(
   findLabelledElements, -- :: DocTypeDecl -> [String]

   ClassifiedElement(..),
   classifyElement, -- :: Element -> ClassifiedElement

   getMiniType, -- :: String -> Char
   toIncludeStr, -- :: Char -> String
   fromIncludeStr, -- :: String -> Char

   getLabel, -- :: Element -> Maybe String

   variantAttributes, -- :: [String]
   variantAttributesType, -- :: AttributesType
   ) where

import Maybe

import XmlTypes

import ExtendedPrelude

import AttributesType

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
data ClassifiedElement =
      Link String --  present link to entity with this label
   |  Reference String -- present reference to entity with this label
   |  Include String -- present include of an entity with this label.  The 
         -- element itself was the import
   |  DirectInclude String Element
         -- The Element was an entity with this label.  The Element returned
         -- is an include pointer to it.
   |  Other

classifyElement :: Element -> ClassifiedElement
classifyElement (Elem name attributes content) =
-- The various assumptions made by this function about attributes being 
-- present should be enforced by the DTD.
   let
      -- abbreviations
      getAtt key = getAttribute attributes key

      cErr str = error ("MMiSSDTDAssumptions.classifyElement: "++str)

      -- General code for handling includes/links/references.  It assumes
      -- the attribute for the referenced object (the first argument) exists.
      generalRef :: String -> (String -> ClassifiedElement) 
         -> ClassifiedElement
      generalRef refName constructor =
         case (getAtt refName,getAtt "status") of
            (Just linked,Just "present") -> constructor linked
            (Just linked,_) -> Other
            (Nothing,_) -> cErr (refName++" attribute is not defined")

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
      -- Rules for mapping entity names:
      --    elements "package"/"section"/"paragraph"/"view" -> "includeGroup"
      --             "example"/"exercise"/definition"/"program"/"theory"/
      --                "theorem"/"development"/"proof"/"script"/"list" 
      --                                                    -> "includeUnit"
      --             "textFragment"                 -> "includeTextFragment"
      --             "table"/"figure"/"glossaryEntry"/
      --                 "bibEntry"/"programFragment"/"clause"/"step" 
      --                                                    -> "includeAtom"
      --
      --    The returned element has an Xml tag taken from the corresponding
      --    RHS, with empty content and two attributes.  First attribute is 
      --    "included" pointing to the value of the supplied label attribute.  
      --    Second attribute is "status=present".
      mkInclude includeTag = case getAtt "label" of
         Nothing -> Other
         Just label ->
            DirectInclude label (Elem includeTag [
               ("included",AttValue [Left label]),
               ("status",AttValue [Left "present"])
               ] [])

   in
      case name of
         -- Links
         "link" -> link

         -- References
         "reference" -> reference

         -- Includes
         "includeGroup" -> include
         "includeUnit" -> include
         "includeAtom" -> include
         "includeTextFragment" -> include

         _ -> case classifyLabelledTag name of
            Just GroupTag -> mkInclude "includeGroup"
            Just UnitTag -> mkInclude "includeUnit"
            Just AtomTag -> mkInclude "includeAtom"
            Just TextFragmentTag -> mkInclude "includeTextFragment"
            Nothing -> Other

---
-- The tags which take labels are divided according to which sort of "include"
-- they correspond to.
data LabelledTag =
      GroupTag
   |  UnitTag
   |  AtomTag
   |  TextFragmentTag

classifyLabelledTag :: String -> Maybe LabelledTag
classifyLabelledTag str = case str of
   "package" -> Just GroupTag
   "section" -> Just GroupTag
   "paragraph" -> Just GroupTag
   "view" -> Just GroupTag

   "example" -> Just UnitTag
   "exercise" -> Just UnitTag
   "definition" -> Just UnitTag
   "program" -> Just UnitTag
   "theory" -> Just UnitTag
   "theorem" -> Just UnitTag
   "development" -> Just UnitTag
   "proof" -> Just UnitTag
   "script" -> Just UnitTag
   "list" -> Just UnitTag

   "textFragment" -> Just TextFragmentTag

   "table" -> Just AtomTag
   "figure" -> Just AtomTag
   "glossaryEntry" -> Just AtomTag
   "bibEntry" -> Just AtomTag
   "programFragment" -> Just AtomTag
   "clause" -> Just AtomTag
   "step" -> Just AtomTag
   _ -> Nothing

---
-- We also use classifyLabelledTag to get the mini-type-letter, used by
-- EmacsEdit.hs to colour magic buttons appropriately.
getMiniType :: String -> Char
getMiniType str = case classifyLabelledTag str of
   Just GroupTag -> 'G'
   Just UnitTag -> 'U'
   Just AtomTag -> 'A'
   Just TextFragmentTag -> 'T'
   Nothing -> error ("Attempt to edit object with unclassifiable tag "++str)

---
-- toIncludeStr and fromIncludeStr convert the mini-type to and from XXX in
-- the corresponding includeXXX command.
toIncludeStr :: Char -> String
toIncludeStr 'G' = "Group"
toIncludeStr 'U' = "Unit"
toIncludeStr 'A' = "Atom"
toIncludeStr 'T' = "TextFragment"
toIncludeStr _ = error "MMiSSDTDAssumptions.toIncludeStr - bad mini-type"

fromIncludeStr :: String -> Char
fromIncludeStr "Group" = 'G'
fromIncludeStr "Unit" = 'U'
fromIncludeStr "Atom" = 'A'
fromIncludeStr "TextFragment" = 'T'
fromIncludeStr _ = error 
   "MMiSSDTDAssumptions.fromIncludeStr - bad include string"

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
-- Get an Element's label.  This must be a single String.
getLabel :: Element -> Maybe String
getLabel (Elem xmlTag attributes _) = getAttribute attributes "label"

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
   ["xml:lang","notationId","levelOfDetailId","interactionLevelId"]

variantAttributesType :: AttributesType
variantAttributesType =
   foldl
      (\ attType name -> needs (mkAttributeKey name) "" attType)
      emptyAttributesType
      variantAttributes



