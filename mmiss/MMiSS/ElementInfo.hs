-- | This information controls getting locational info (label, packageId,
-- path and variants) out of an Element (for when we read it in), and
-- putting it back in again (for when we write it out).
module MMiSS.ElementInfo(
   ElementInfo(..),
   emptyElementInfo, -- :: ElementInfo
   changeLabel, -- :: Element -> EntitySearchName -> WithError Element
      -- Change an Element's label.
   getElementInfo, -- :: Element -> WithError (ElementInfo,Element)
      -- extract an Element's ElementInfo, and return the Element without
      -- that ElementInfo.
   packagePathOpt,
      -- :: ElementInfo -> Maybe EntityFullName
      -- deduce a PackagePath from this ElementInfo
   setElementInfoStrict,
      -- :: Element -> ElementInfo -> WithError Element
      -- Set an Element's ElementInfo (or complain if there is a clash).
   mergeElementInfoStrict,
      -- :: ElementInfo -> ElementInfo -> WithError ElementInfo
      -- Merge two ElementInfo's (or complain if there is a clash).

   getPackageId, -- :: Element -> WithError (Maybe PackageId)
   setPackageId, -- :: Element -> PackageId -> Element


   getFiles, -- :: Element -> WithError [String]
   getAllFiles, -- :: Element -> [String]
   getPriority, -- :: Element -> WithError String
   getPriorityAtt, -- :: [Attribute] -> WithError String
   setPriority, -- :: Element -> String -> Element
   setPriorityAtt, -- :: [Attribute] -> String -> [Attribute]
   getLabelOpt, -- :: Element -> WithError (Maybe EntitySearchName)
   getObjectClass, -- :: Element -> WithError (Maybe String)
   ) where

import Data.List
import Data.Maybe

import Util.AtomString
import Util.Computation
import Util.ExtendedPrelude

import Text.XML.HaXml.Types

import MMiSS.XmlExtras

import Imports.EntityNames

import MMiSS.LaTeX.Parser(PackageId(..))

import MMiSS.Variant
import MMiSS.DTD
import MMiSS.XmlBasics

-- ----------------------------------------------------------------------
-- Datatypes
-- ----------------------------------------------------------------------

data ElementInfo = ElementInfo {
   packageIdOpt :: Maybe PackageId,
   packagePathOpt1 :: Maybe EntityFullName,
      -- ^ the packagePathOpt function will also try to get a packagePath
      -- function from the labelOpt, if packagePathOpt1 is unset.
   packageNameOpt :: Maybe EntityName,
      -- ^ the name of the containing package object.
      -- NB.  This module doesn't attempt to set this to anything but Nothing;
      -- that is done by, for example, MMiSSBundleFillIn.fillInBundle
   labelOpt :: Maybe EntitySearchName,
   variants :: MMiSSVariantSpec
   } deriving (Show) -- for debugging.

-- ----------------------------------------------------------------------
-- Main Functions
-- ----------------------------------------------------------------------

emptyElementInfo :: ElementInfo
emptyElementInfo = ElementInfo {
   packageIdOpt = Nothing,
   packagePathOpt1 = Nothing,
   packageNameOpt = Nothing,
   labelOpt = Nothing,
   variants = emptyMMiSSVariantSpec
   }

getElementInfo :: Element -> WithError (ElementInfo,Element)
getElementInfo (Elem name0 _ content0) =
   do
      (attributesElement,content1) <- case content0 of
         CElem (attributesElement @ (Elem "attributes" _ _)) : content1
            -> return (attributesElement,content1)
         _ -> fail (name0 ++
            " element does not begin with an attributes element")

      let
         Elem _ attributeElementAttributes0 attributesElementContent
            = attributesElement

      case validateElement0 attributesElement of
         [] -> done
         errorMessages -> fail (unlines (
            ("Unable to validate attributes element in " ++ name0
               ++ " element:")
               : errorMessages) ++ toUglyExportableXml attributesElement)
         -- From now on we can assume attributesElement matches the DTD,
         -- for example when we call toMMiSSVariantSpecFromXml.

      packageIdOpt0 <- getPackageIdAtt attributeElementAttributes0
      packagePathOpt0 <- getPackagePathAtt attributeElementAttributes0
      labelOpt0 <- getLabelAtt attributeElementAttributes0

      let
         (variants0,restAttributes) = case attributesElementContent of
            CElem (variantAttributesElement@(Elem "variant-attributes" _ _))
               : restAttributes ->
                  (toMMiSSVariantSpecFromXml variantAttributesElement,
                     restAttributes)
            _ -> (emptyMMiSSVariantSpec,attributesElementContent)

         elementInfo = ElementInfo {
            packageIdOpt = packageIdOpt0,
            packagePathOpt1 = packagePathOpt0,
            packageNameOpt = Nothing,
            labelOpt = labelOpt0,
            variants = variants0
            }

         attributeElementAttributes1 =
            filter
               (\ (name,_) -> case name of
                  "label" -> False
                  "packageId" -> False
                  "packagePath" -> False
                  _ -> True
                  )
               attributeElementAttributes0

         attributesElem1
            = Elem "attributes" attributeElementAttributes1 restAttributes

         remainingElem = Elem name0 [] (CElem attributesElem1 : content1)

      return (elementInfo,remainingElem)

packagePathOpt :: ElementInfo -> Maybe EntityFullName
packagePathOpt elInfo =
   case packagePathOpt1 elInfo of
      Just fullName -> Just fullName
      Nothing -> guessPackagePathFromLabel (labelOpt elInfo)

setElementInfoStrict :: Element -> ElementInfo -> WithError Element
setElementInfoStrict element0 elInfo0 =
   do
      (elInfo1,element1) <- getElementInfo element0
      elInfo2 <- mergeElementInfoStrict elInfo0 elInfo1
      let
         elInfo3 = stripUnnecessaryPackagePath elInfo2
         element4 = prependElementInfo elInfo3 element1

      return element4

changeLabel :: Element -> EntitySearchName -> WithError Element
changeLabel element0 name =
   do
      (elInfo1,element1) <- getElementInfo element0

      let
         elInfo2 = elInfo1 {
            packagePathOpt1 = packagePathOpt elInfo1,
            labelOpt = Just name
            }

         elInfo3 = stripUnnecessaryPackagePath elInfo2

         element4 = prependElementInfo elInfo3 element1

      return element4


mergeElementInfoStrict :: ElementInfo -> ElementInfo -> WithError ElementInfo
mergeElementInfoStrict elementInfo1 elementInfo2 =
   do
      packageIdOpt0 <- mm "packageId" packageIdOpt
      labelOpt0 <- mm "label" labelOpt
      packagePathOpt0 <- mm "packagePath" packagePathOpt1
      packageNameOpt0 <- mm "package name" packageNameOpt
      variants0 <- mergeMMiSSVariantSpecStrict
         (variants elementInfo1) (variants elementInfo2)
      return (ElementInfo {
         packageIdOpt = packageIdOpt0,
         labelOpt = labelOpt0,
         packageNameOpt = packageNameOpt0,
         packagePathOpt1 = packagePathOpt0,
         variants = variants0
         })
   where
      mm :: (Show a,Eq a)
         => String -> (ElementInfo -> Maybe a) -> WithError (Maybe a)
      mm fieldName fieldFn
         = mergeMaybe fieldName (fieldFn elementInfo1) (fieldFn elementInfo2)

-- -----------------------------------------------------------------
-- Functions for accessing the system attributes
-- In general, functions ending "Att" work with the attributes list
-- of the <attribute> element.  Functions which don't work with the
-- actual <group>, <unit>, <atom> or <embedded> involved.
--
-- The latter functions are also a lot more careless about error conditions.
-- -----------------------------------------------------------------


getLabelOpt :: Element -> WithError (Maybe EntitySearchName)
getLabelOpt elem =
   do
      attributes <- getAttributes elem
      getLabelAtt attributes

getObjectClass :: Element -> WithError (Maybe String)
getObjectClass elem =
   do
      attributes <- getAttributes elem
      getObjectClassAtt attributes

getPackageId :: Element -> WithError (Maybe PackageId)
getPackageId elem =
   do
      attributes <- getAttributes elem
      getPackageIdAtt attributes

-- | Get the files imported by a particular element (but not by included
-- elements).
getFiles :: Element -> WithError [String]
getFiles elem =
   do
      attributes <- getAttributes elem
      getFilesAtt attributes

getPriority :: Element -> WithError String
getPriority elem =
   do
      attributes <- getAttributes elem
      getPriorityAtt attributes

-- | Get all the files imported by an element or its included elements.
getAllFiles :: Element -> [String]
getAllFiles elem =
   let
      elems :: [Element]
      elems = getAllElements1 elem

      files1 :: [WithError [String]]
      files1 = map getFiles elems

      files2 :: [[String]]
      files2 = map
         (\ ssWE -> case fromWithError ssWE of
            Left _ -> []
            Right ss -> ss
            )
         files1
   in
      uniqOrd (concat files2)

getAttributes :: Element -> WithError [Attribute]
getAttributes (Elem name0 _ content) =
   return (
      case content of
         CElem (Elem "attributes" attributes _) : _ -> attributes
         _ -> []
         )

getLabelAtt :: [Attribute] -> WithError (Maybe EntitySearchName)
getLabelAtt attributes =
   do
      labelStrOpt <- getAttribute attributes "label"
      case labelStrOpt of
         Nothing -> return Nothing
         Just labelStr ->
            do
               label <- fromStringWE labelStr
               return (Just label)


getObjectClassAtt :: [Attribute] -> WithError (Maybe String)
getObjectClassAtt attributes = getAttribute attributes "object-class"

getPackageIdAtt :: [Attribute] -> WithError (Maybe PackageId)
getPackageIdAtt attributes =
   do
      packageIdStrOpt <- getAttribute attributes "packageId"
      return (fmap PackageId packageIdStrOpt)

getPackagePathAtt :: [Attribute] -> WithError (Maybe EntityFullName)
getPackagePathAtt attributes =
   do
      packagePathStrOpt <- getAttribute attributes "packagePath"
      case packagePathStrOpt of
         Nothing -> return Nothing
         Just packagePathStr ->
            do
               packagePath <- fromStringWE packagePathStr
               return (Just packagePath)

getPriorityAtt :: [Attribute] -> WithError String
getPriorityAtt attributes =
   do
      pOpt <- getAttribute attributes "priority"
      return (fromMaybe "1" pOpt)

getFilesAtt :: [Attribute] -> WithError [String]
getFilesAtt attributes =
   do
      filesOpt <- getAttribute attributes "files"
      return (case filesOpt of
         Nothing -> []
         Just files -> splitByChar ',' files
         )

-- -----------------------------------------------------------------
-- Functions for setting the system attributes
-- -----------------------------------------------------------------

setPackageId :: Element -> PackageId -> Element
setPackageId element packageId =
   setAttributes (setPackageIdAtt packageId) element

setPriority :: Element -> String -> Element
setPriority element priority =
   setAttributes (\ atts -> setPriorityAtt atts priority) element

setAttributes :: ([Attribute] -> [Attribute]) -> Element -> Element
setAttributes newAttributesFn (Elem name0 atts0 content0) =
   let
      (attributesElem0,content1) = case content0 of
         (CElem attributesElem) : content1 ->
            (attributesElem,content1)
         _ -> (Elem "attributes" [] [],content0)
            -- I'm not sure if this ever happens.

      (Elem attributesElemName attributesElemAttributes0
         attributesElemContent) = attributesElem0
      attributesElemAttributes1 = newAttributesFn attributesElemAttributes0
      attributesElem1 = Elem attributesElemName attributesElemAttributes1
         attributesElemContent

      content2 = (CElem attributesElem1) : content1
   in
      Elem name0 atts0 content2

setPackageIdAtt :: PackageId -> [Attribute] -> [Attribute]
setPackageIdAtt (PackageId id) attributes =
   setAttribute attributes "packageId" id

setPriorityAtt :: [Attribute] -> String -> [Attribute]
setPriorityAtt attributes priority =
   case priority of
      "1" -> delAttribute attributes "priority"
      _ -> setAttribute attributes "priority" priority


-- -----------------------------------------------------------------
-- Internal functions
-- -----------------------------------------------------------------

-- | Add ElementInfo back to an Element.  It is assumed that the
-- Element supplied is the result of getElementInfo, so it contains
-- no system attributes apart from objectClass and no variant attributes.
prependElementInfo :: ElementInfo -> Element -> Element
prependElementInfo elInfo (Elem name0 [] content0) =
   let
      (attributesElement0,content1) = case content0 of
         CElem (attributesElement0 @ (Elem "attributes" _ _)) : content1
            -> (attributesElement0,content1)
         _ -> error ("MMiSSElementInfo.prependElementInfo: " ++ name0 ++
            " element does not begin with an attributes element")
            -- this should not happen, I hope, since this function is supposed
            -- to be called too internally.

      Elem _ attributes0 attributesContent = attributesElement0

      -- (1) compute the new system attributes
      attributes1 :: [Attribute]
      attributes1 =
         (addAtt "label" labelOpt) .
         (addAtt "packageId" packageIdOpt) .
         (addAtt "packagePath" packagePathOpt1) $
         attributes0

      addAtt :: StringClass object
         => String -> (ElementInfo -> Maybe object)
         -> [Attribute] -> [Attribute]
      addAtt attName toAtt atts1 =
         case toAtt elInfo of
           Nothing -> atts1
           Just att -> setAttribute0 atts1 attName (toString att)

      -- (2) compute the variants, if any
      variantAttributesElements =
         if variants elInfo == emptyMMiSSVariantSpec
            then
               []
            else
               [CElem (fromMMiSSVariantSpecToXml (variants elInfo))]

      attributesElement1 = Elem "attributes" attributes1
         (variantAttributesElements ++ attributesContent)
   in
      Elem name0 [] ((CElem attributesElement1) : content1)

stripUnnecessaryPackagePath :: ElementInfo -> ElementInfo
stripUnnecessaryPackagePath elInfo0 =
   if guessPackagePathFromLabel (labelOpt elInfo0) == packagePathOpt1 elInfo0
      then
         elInfo0 {packagePathOpt1 = Nothing}
      else
         elInfo0


guessPackagePathFromLabel :: Maybe EntitySearchName -> Maybe EntityFullName
guessPackagePathFromLabel labelOpt0 = case labelOpt0 of
   Just (FromHere fullName)-> Just fullName
   Just (FromCurrent fullName) -> Just fullName
   _ -> Nothing

mergeMaybe :: (Show a,Eq a)
   => String -> Maybe a -> Maybe a -> WithError (Maybe a)
mergeMaybe keyName aOpt1 aOpt2 =
   case (aOpt1,aOpt2) of
      (Just a1,Just a2) | a1 /= a2
         -> fail ("Conflicting values for " ++ keyName ++
               ": " ++ show a1 ++ " and " ++ show a2)
      (Just a1,_) -> return aOpt1
      _ -> return aOpt2




