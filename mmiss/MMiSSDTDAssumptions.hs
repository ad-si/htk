-- | In this module we collect the various assumptions the Haskell code makes
-- about the MMiSS DTD, which are not explicitly read from it. 
-- 
-- MMiSSElementInfo.hs contains code specific to processing and changing
--    labels, paths, and so on.
-- 
-- See also MMiSSBundleDissect.hs.  But hopefully that is less dependent on 
-- the DTD itself. 
module MMiSSDTDAssumptions(
   mkIncludeElement, -- :: Element -> Maybe Element


   LinkType(..),
   classifyLink, -- :: Element -> Maybe (LinkType,String)



   unclassifyElement, 
      -- :: Element -> Maybe (String,[Attribute],Element -> WithError ())

   getMiniType, -- :: String -> Char
   toIncludeStr, -- :: Char -> String
   fromIncludeStr, -- :: String -> Char

   printAttributes, -- :: [Attribute] -> String

   isPackageTag, -- :: String -> Bool
      -- Returns True if an element with this tag is to be treated as a 
      -- package.
   ) where

import Maybe
import Char

import Computation
import AtomString
import Dynamics(Typeable)
import BinaryAll

import Text.XML.HaXml.Types

import LaTeXParser(classifyLabelledTag,fromIncludeStr,toIncludeStr,
   IncludeInfo(..))

import MMiSSXmlBasics
import MMiSSElementInfo

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
mkIncludeElement (elem@(Elem name atts contents)) =
   let
      labelOpt = fromWithError1 Nothing (getLabelOpt elem)
   in
      case (labelOpt,classifyLabelledTag name,
            fromWithError (getObjectClass elem)) of
         (Just label,Just includeChar,Right (Just objectClass)) ->
            Just (
               Elem 
                 ("include" ++ toIncludeStr includeChar) 
                 [
                     ("included",AttValue [Left (toString label)]),
                     ("status",AttValue [Left "present"]),
                     ("object-class",AttValue [Left objectClass])
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
         case fromWithError (getAttribute attributes refAttName) of
            Right (Just labelString)  -> 
               case fromWithError (getAttribute attributes "status") of
                  Right (Just "present") -> Just (linkType,labelString)
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
unclassifyElement :: Element 
   -> Maybe (String,IncludeInfo,Element -> WithError ())
unclassifyElement (elem@(Elem name attributes contents)) =
   do
      (tag :: Char) <- case name of
         'i':'n':'c':'l':'u':'d':'e': rest -> Just (fromIncludeStr rest)
         _ -> Nothing

      let
         includedLabel = 
            case fromWithError (getAttribute attributes "included") of
               Right (Just included) -> included
               _ -> error ("MMiSSDTDAssumptions: "++name
                  ++" element has no included attribute")

      variantOpt <- case contents of
         [] -> return Nothing
         [CElem variant] -> return (Just variant)
         _ -> fail ("Include element does not contain a "
            ++ "single variant-attributes element")

      let
         includeInfo = IncludeInfo {
            variantOpt = variantOpt,
            otherAttributes = attributes
            }


         checkFn (Elem newName _ _) =
            if Just tag == classifyLabelledTag newName
               then
                  return ()
               else
                  fail ("The element "++newName
                     ++" does not match the reference "++name)

      return (includedLabel,includeInfo,checkFn)

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


   
