-- | This information controls getting locational info (label, packageId,
-- path and variants) out of an Element (for when we read it in), and
-- putting it back in again (for when we write it out).
module MMiSSElementInfo(
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
   ) where

import List

import AtomString
import Computation

import Text.XML.HaXml.Types

import EntityNames

import LaTeXParser(PackageId)

import MMiSSVariant
import MMiSSDTDAssumptions

-- ----------------------------------------------------------------------
-- Datatypes
-- ----------------------------------------------------------------------

data ElementInfo = ElementInfo {
   packageIdOpt :: Maybe PackageId,
   packagePathOpt1 :: Maybe EntityFullName,
      -- the packagePathOpt function will also try to get a packagePath
      -- function from the labelOpt, if packagePathOpt1 is unset.
   packageNameOpt :: Maybe EntityName,
      -- the name of the containing package object.
   labelOpt :: Maybe EntitySearchName,
   variants :: MMiSSVariantSpec
   }

-- ----------------------------------------------------------------------
-- Functions
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
getElementInfo (Elem name atts0 content) =
   let
      attsToExtract = "label" : "packageId" : "packagePath" 
         : variantAttributes2
      (infoAtts,atts1) = 
         partition 
            (\ (name,_) -> elem name attsToExtract)
            atts0
      elem1 = Elem name atts1 content

      packageIdStrOpt = getAttribute infoAtts "packageId"
      packageIdOpt =
         fmap
            fromString
            packageIdStrOpt
 
      labelStrOpt = getAttribute infoAtts "label"
     
      packagePathStrOpt = getAttribute infoAtts "packagePath"

      variantSpec = toMMiSSVariantSpecFromXml infoAtts
   in
      do
         labelOpt <- case labelStrOpt of 
            Just labelStr -> 
               do 
                  label <- fromStringWE labelStr
                  return (Just label)
            Nothing -> return Nothing
         packagePathOpt <- case packagePathStrOpt of
            Just packagePathStr -> 
               do
                  packagePath <- fromStringWE packagePathStr
                  return (Just packagePath)
            Nothing -> return Nothing
         return (ElementInfo {
            packageIdOpt = packageIdOpt,
            packagePathOpt1 = packagePathOpt,
            packageNameOpt = Nothing,
            labelOpt = labelOpt,
            variants = variantSpec
            },elem1) 

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
-- Internal functions
-- -----------------------------------------------------------------

prependElementInfo :: ElementInfo -> Element -> Element
prependElementInfo elInfo (Elem name atts0 content) =
   let
      atts1 = 
         (addAtt "label" labelOpt) .
         (addAtt "packageId" packageIdOpt) . 
         (addAtt "packagePath" packagePathOpt1) .
         ( (fromMMiSSVariantSpecToXml . variants $ elInfo) ++ ) $
            atts0

      addAtt :: StringClass object 
         => String -> (ElementInfo -> Maybe object) 
         -> [Attribute] -> [Attribute]
      addAtt attName toAtt atts1 =
         case toAtt elInfo of
           Nothing -> atts1
           Just att -> setAttribute0 atts1 attName (toString att)
   in
      Elem name atts1 content

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