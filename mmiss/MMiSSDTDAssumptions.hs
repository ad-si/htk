-- | In this module we collect the various assumptions the Haskell code makes
-- about the MMiSS DTD, which are not explicitly read from it. 
-- 
-- MMiSSElementInfo.hs contains code specific to processing and changing
--    labels, paths, and so on.
-- 
-- See also MMiSSBundleDissect.hs.  But hopefully that is less dependent on 
-- the DTD itself. 
module MMiSSDTDAssumptions(
   findLabelledElements, -- :: DocTypeDecl -> [String]

   mkIncludeElement, -- :: Element -> Maybe Element


   LinkType(..),
   classifyLink, -- :: Element -> Maybe (LinkType,String)



   unclassifyElement, 
      -- :: Element -> Maybe (String,[Attribute],Element -> WithError ())

   getMiniType, -- :: String -> Char
   toIncludeStr, -- :: Char -> String
   fromIncludeStr, -- :: String -> Char

   getAttribute, -- :: [Attribute] -> String -> Maybe String
   setAttribute, -- :: [Attribute] -> String -> String -> [Attribute]

   setAttribute0, -- :: [Attribute] -> String -> String -> [Attribute]
      -- Only to be used when it is known that the attribute being added
      -- does not occur in the input attributes.

   getLabel, -- :: Element -> WithError EntitySearchName
      -- Get an Element's label.

   getPackageId, -- :: Element -> Maybe PackageId
      -- Get an Element's packageId, if any.
   setPackageId, -- :: Element -> PackageId -> Element
      -- Set an Element's packageId. 
   delPackageId, -- :: Element -> Element
      -- Delete an Element's packageId.

   getPriorityAttributes, -- :: [Attribute] -> String
   getPriority, -- :: Element -> String
   setPriority, -- :: Element -> String -> Element
      -- get/set an element's priority.

   setPriorityAttributes', -- :: [Attribute] -> String -> [Attribute]


   variantAttributes, -- :: [String]
   variantAttributesType, -- :: AttributesType

   printAttributes, -- :: [Attribute] -> String

   isPackageTag, -- :: String -> Bool
      -- Returns True if an element with this tag is to be treated as a 
      -- package.

   getFiles, -- :: Element -> [String]
      -- Return all the external files in an element
   ) where

import Maybe
import Char

import ExtendedPrelude
import Computation
import AtomString
import Dynamics(Typeable)
import BinaryAll

import Text.XML.HaXml.Types
import XmlExtras

import AttributesType

import EntityNames

import LaTeXParser(classifyLabelledTag,fromIncludeStr,toIncludeStr,
   PackageId(..))

-- ----------------------------------------------------------------------
-- LinkType
-- ----------------------------------------------------------------------

-- | Type of a link/reference/include from a document, output by
data LinkType = IncludeLink | LinkLink | ReferenceLink deriving (Typeable)

-- ----------------------------------------------------------------------
-- Instances for LinkType
-- ----------------------------------------------------------------------

instance Monad m => HasBinary LinkType m where
   writeBin = mapWrite (\ linkType -> case linkType of
      IncludeLink -> 'I'
      ReferenceLink -> 'R'
      LinkLink -> 'L'
      )
   readBin = mapRead (\ char -> case char of
      'I' -> IncludeLink
      'R' -> ReferenceLink
      'L' -> LinkLink
      _ -> error ("MMiSSDTDAssumptions: unexpected char "++show char)
      )

-- ----------------------------------------------------------------------
-- Determining if an Element can be referenced by an includeXXX element,
-- and if so, by what.
-- ----------------------------------------------------------------------

mkIncludeElement :: Element -> Maybe Element
mkIncludeElement (Elem name atts contents) =
   case (getAttribute atts "label",classifyLabelledTag name) of
      (Just labelString,Just includeChar) ->
         Just (
            Elem 
              ("include" ++ toIncludeStr includeChar) 
              [
                  ("included",AttValue [Left labelString]),
                  ("status",AttValue [Left "present"])
                  ]
              []
            )
      _ -> Nothing 


-- ----------------------------------------------------------------------
-- Determine whether an element is link, reference or include and
-- if so and status is "present", extract the corresponding LinkType and label.
-- ----------------------------------------------------------------------

classifyLink :: Element -> Maybe (LinkType,String)
classifyLink (Elem name attributes _)  =
   let
      generalRef 
         :: String -> LinkType -> Maybe (LinkType,String)
      generalRef refAttName linkType =
         case getAttribute attributes refAttName of
            Just labelString -> case getAttribute attributes "status" of
               Just "present" -> Just (linkType,labelString)
               _ -> Nothing
            _ -> Nothing
  in
     case name of
         -- Links
         "link" -> generalRef "linked" LinkLink

         -- References
         "reference" -> generalRef "referenced" ReferenceLink

         -- Includes
         'i':'n':'c':'l':'u':'d':'e':_ -> generalRef "included" IncludeLink

         -- everything else
         _ -> Nothing


-- ----------------------------------------------------------------------
-- ----------------------------------------------------------------------

-- | How to distinguish those elements in the DTD which can be displayed
-- separately in the development graph.
-- Rule: those which have an attribute \"label\". 
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

-- | We also use classifyLabelledTag to get the mini-type-letter, used by
-- EmacsEdit.hs to colour magic buttons appropriately.
getMiniType :: String -> Char
getMiniType str = case classifyLabelledTag str of
   Just c -> c
   Nothing -> error ("Attempt to edit object with unclassifiable tag "++str)


-- | Returns True if an element with this tag is to be treated as a package.
isPackageTag :: String -> Bool
isPackageTag s = (s == "package")


-- ----------------------------------------------------------------------
-- Turning a (possible) link back into an element
-- ----------------------------------------------------------------------

-- | Given an Element which is an include, returns the contained label,
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
-- Primitive functions for operating on attributes
-- ----------------------------------------------------------------------

-- | | Extract an attribute, which had better be a single String
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

-- | Delete an attribute with a given key, if one is present
delAttribute :: [Attribute] -> String -> [Attribute]
delAttribute attributes0 key =
   deleteFirstOpt (\ (key1,_) -> key1 == key) attributes0

-- | Set an attribute
setAttribute :: [Attribute] -> String -> String -> [Attribute]
setAttribute attributes0 key value =
   let
      attributes1 = delAttribute attributes0 key
      attributes2 = setAttribute0 attributes1 key value
   in
      attributes2


-- | Only to be used when it is known that the attribute being added
-- does not occur in the input attributes.
setAttribute0 :: [Attribute] -> String -> String -> [Attribute]
setAttribute0 attributes0 key value =
   (key,AttValue [Left value]) : attributes0
   


-- | Get an Element's attribute
getAtt :: String -> Element -> Maybe String
getAtt key (Elem _ atts _) = getAttribute atts key

-- | Delete an Element's attribute (or nothing, if it isn't set)
delAtt :: String -> Element -> Element
delAtt key (Elem name atts0 content) =
   let
      atts1 = delAttribute atts0 key
   in
      Elem name atts1 content
      
-- | Set an Element's attribute
setAtt :: String -> Element -> String -> Element
setAtt key (Elem name atts0 content) value =
   let
      atts1 = setAttribute atts0 key value
   in
      (Elem name atts1 content)

-- ----------------------------------------------------------------------
-- Functions for accessing particular attributes
-- ----------------------------------------------------------------------

-- | Get an Element's label.
getLabel :: Element -> WithError EntitySearchName
getLabel (elem @ (Elem name _ _)) =
   let
      labelOpt = getAtt "label" elem
   in
      case labelOpt of
         Nothing -> hasError ("Element " ++ name ++ " has no label!")
         Just labelStr -> fromStringWE labelStr

{-
-- | Set an Element's label.
-- NB.  Now obsolete and dangerous.  Use MMiSSElementInfo.changeLabel
-- to change the label properly, also changing the packagePath.
setLabel :: Element -> EntitySearchName -> Element
setLabel elem entitySearchName =
   setAtt "label" elem (toString entitySearchName)

-- | Delete an Element's label.
delLabel :: Element -> Element
delLabel = delAtt "label"
-}

getPackageId :: Element -> Maybe PackageId
getPackageId elem =
   fmap
      PackageId 
      (getAtt "packageId" elem)

{-
getPackagePath :: Element -> WithError EntityFullName
getPackagePath elem =
   case (getAtt "packagePath" elem,getLabel elem) of
         (Just packagePathStr,_) -> fromStringWE packagePathStr
         (Nothing,entitySearchNameWE) ->
            do
               entitySearchName <- entitySearchNameWE
               case entitySearchName of
                  FromHere fullName -> return fullName
                  _ -> fail ("No package path or valid label found for " ++
                    toString entitySearchName)

-}

setPackageId :: Element -> PackageId -> Element
setPackageId elem (PackageId packageIdStr) =
   setAtt "packageId" elem packageIdStr

delPackageId :: Element -> Element
delPackageId = delAtt "packageId"

getFiles :: Element -> [String]
getFiles element =
   uniqOrd (
      concat
         (map
            getFiles1
            (getAllElements1 element)
            )
      )

getFiles1 :: Element -> [String]
getFiles1 elem
   = case getAtt "files" elem of
      Nothing -> []
      Just files -> splitByChar ',' files

getPriority :: Element -> String
getPriority (Elem _ attributes _) 
   = getPriorityAttributes attributes

getPriorityAttributes :: [Attribute] -> String
getPriorityAttributes attributes
   = fromMaybe "1" (getAttribute attributes "priority")

setPriorityAttributes' :: [Attribute] -> String -> [Attribute]
setPriorityAttributes' attributes priority =
   case priority of
      "1" -> delAttribute attributes "priority"
      _ -> setAttribute attributes "priority" priority

setPriority :: Element -> String -> Element
setPriority elem priority =
   case priority of 
      "1"  -> delAtt "priority" elem
      _ -> setAtt "priority" elem priority


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
   ["xml:lang","levelOfDetail","interactionLevel","formalism","format"]

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


   
