{- In this module we collect the various assumptions the Haskell code makes
   about the MMiSS DTD, which are not explicitly read from it. 

   See also MMiSSContent.hs.  But hopefully that is less dependent on the DTD
   itself. 
   -}
module MMiSSDTDAssumptions(
   findLabelledElements, -- :: DocTypeDecl -> [String]

   ClassifiedElement(..),
   classifyElement, -- :: Element -> ClassifiedElement
   unclassifyElement, -- :: Element -> Maybe (String,Element -> WithError ())

   getMiniType, -- :: String -> Char
   toIncludeStr, -- :: Char -> String
   fromIncludeStr, -- :: String -> Char

   getLabel, -- :: Element -> Maybe String

   variantAttributes, -- :: [String]
   variantAttributesType, -- :: AttributesType
   ) where

import Maybe
import Char

import XmlTypes

import ExtendedPrelude
import Computation

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
      -- Rules for mapping entity names: encoded by mapLabelledTag.
      --
      --    The returned element has an Xml tag taken from the corresponding
      --    RHS, with empty content and two attributes.  First attribute is 
      --    "included" pointing to the value of the supplied label attribute.  
      --    Second attribute is "status=present".
      mkInclude includeChar = case getAtt "label" of
         Nothing -> Other
         Just label ->
            DirectInclude label (Elem ("include" ++ toIncludeStr includeChar) [
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
         'i':'n':'c':'l':'u':'d':'e':_ -> include

         _ -> case classifyLabelledTag name of
            Just c -> mkInclude c
            Nothing -> Other

---
-- Map tags to the name of their corresponding include element (minus
-- "include")
mapLabelledTag :: String -> String
mapLabelledTag s = 
   case s of
      "package" -> "Group"
      "paragraph" -> "Group"
      "view" -> "Group"
      "example" -> "Unit"
      "exercise" -> "Unit"
      "definition" -> "Unit"
      "theorem" -> "Unit"
      "list" -> "Unit"
      "table" -> "Atom"
      "figure" -> "Atom"
      "glossaryEntry" -> "Atom"
      "bibEntry" -> "Atom"
      _ -> mapUpper s
   where
      mapUpper [] = []
      mapUpper (c : cs) = toUpper c : cs      


classifyLabelledTag :: String -> Maybe Char
classifyLabelledTag str = fromIncludeStrOpt (mapLabelledTag str)

---
-- We also use classifyLabelledTag to get the mini-type-letter, used by
-- EmacsEdit.hs to colour magic buttons appropriately.
getMiniType :: String -> Char
getMiniType str = case classifyLabelledTag str of
   Just c -> c
   Nothing -> error ("Attempt to edit object with unclassifiable tag "++str)

---
-- toIncludeStr and fromIncludeStr convert the mini-type to and from XXX in
-- the corresponding includeXXX command.
toIncludeStr :: Char -> String
toIncludeStr 'G' = "Group"
toIncludeStr 'U' = "Unit"
toIncludeStr 'A' = "Atom"
toIncludeStr 'T' = "TextFragment"
toIncludeStr 'S' = "Section"
toIncludeStr 'a' = "Abstract"
toIncludeStr 'I' = "Introduction"
toIncludeStr 's' = "Summary"
toIncludeStr 'F' = "FormalUnit"
toIncludeStr 'C' = "ConceptualAtom"
toIncludeStr 'p' = "ProgramFragment"
toIncludeStr 'P' = "Program"
toIncludeStr 'c' = "Clause"
toIncludeStr 't' = "Theory"
toIncludeStr 'x' = "Step"
toIncludeStr 'y' = "Proof"
toIncludeStr 'z' = "Script"
toIncludeStr 'D' = "Development"            
toIncludeStr _ = error "MMiSSDTDAssumptions.toIncludeStr - bad mini-type"


---
-- fromIncludeStrOpt
-- and also handles the case where the first letter is lower-cased.
fromIncludeStrOpt :: String -> Maybe Char
fromIncludeStrOpt "Group" = Just 'G'
fromIncludeStrOpt "Unit" = Just 'U'
fromIncludeStrOpt "Atom" = Just 'A'
fromIncludeStrOpt "TextFragment" = Just 'T'
fromIncludeStrOpt "Section"         = Just 'S'                
fromIncludeStrOpt "Abstract"        = Just 'a'        
fromIncludeStrOpt "Introduction"    = Just 'I'        
fromIncludeStrOpt "Summary"         = Just 's'        
fromIncludeStrOpt "FormalUnit"      = Just 'F'        
fromIncludeStrOpt "ConceptualAtom"  = Just 'C'        
fromIncludeStrOpt "ProgramFragment" = Just 'p'         
fromIncludeStrOpt "Program"         = Just 'P'        
fromIncludeStrOpt "Clause"          = Just 'c'        
fromIncludeStrOpt "Theory"          = Just 't'        
fromIncludeStrOpt "Step"            = Just 'x'        
fromIncludeStrOpt "Proof"           = Just 'y'        
fromIncludeStrOpt "Script"          = Just 'z'        
fromIncludeStrOpt "Development"     = Just 'D' 
fromIncludeStrOpt (c : cs) | Char.isLower c 
   = fromIncludeStrOpt (toUpper c : cs)
fromIncludeStrOpt _ = Nothing

fromIncludeStr :: String -> Char
fromIncludeStr str = case fromIncludeStrOpt str of
   Just c -> c
   Nothing -> error 
    ("MMiSSDTDAssumptions.fromIncludeStr - bad include string"++str)

-- ----------------------------------------------------------------------
-- Turning a (possible) link back into an element
-- ----------------------------------------------------------------------

---
-- Given an Element which is an include, returns the contained label and a 
-- function which verifies that the given Element matches.  For other elements,
-- returns Nothing.
unclassifyElement :: Element -> Maybe (String,Element -> WithError ())
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
   ["xml:lang","levelOfDetail","interactionLevel"]

variantAttributesType :: AttributesType
variantAttributesType =
   foldl
      (\ attType name -> needs (mkAttributeKey name) "" attType)
      emptyAttributesType
      variantAttributes




