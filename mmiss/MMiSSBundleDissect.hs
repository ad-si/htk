{- This module is used for splitting up the elements within a bundle. 
   It is assumed that the bundle has already been validated. -}
module MMiSSBundleDissect(
   dissectBundle, -- :: Bundle -> WithError Bundle
   reduceElement, -- :: Element -> Maybe Element
   ) where

import Monad
import Maybe

import Computation

import Text.XML.HaXml.Types

import MMiSSBundle
import MMiSSBundleTypes
import MMiSSBundleSimpleUtils
import MMiSSDTDAssumptions
import MMiSSElementInfo
import MMiSSElementInstances
import MMiSSVariant


dissectBundle :: Bundle -> WithError Bundle
dissectBundle bundle0 =
   do
      (bundle1,bundles) <- splitBundle bundle0
      mergeBundles (bundle1 : bundles)      

-- ----------------------------------------------------------------------
-- Splitting things apart from elements
-- ----------------------------------------------------------------------

splitBundle :: Bundle -> WithError (Bundle,[Bundle])
splitBundle (Bundle bundleNodePackages0) =
   do
      (bundleNodePackages1bundles :: [((PackageId,BundleNode),[Bundle])]) <-
         mapM
            (\ (packageId,bundleNode0) ->
               do
                  (bundleNode1,bundles) 
                     <- splitBundleNode packageId bundleNode0
                  return ((packageId,bundleNode1),bundles)
               )
            bundleNodePackages0
      let
         bundleNodePackages1 = map fst bundleNodePackages1bundles
         bundles = concat (map snd bundleNodePackages1bundles)
      return (Bundle bundleNodePackages1,bundles) 


splitBundleNode :: PackageId -> BundleNode -> WithError (BundleNode,[Bundle])
splitBundleNode packageId bundleNode =
   let
      fileLoc1 = fileLoc bundleNode
      objectType1 = objectType fileLoc1

      bundleNodeData1 = bundleNodeData bundleNode

      unchanged = return (bundleNode,[])
      newBundleNodeData bundleNodeData1 bundles =
         return (bundleNode {bundleNodeData = bundleNodeData1},bundles)

      isFolder = case bundleNodeData1 of
         Dir bundleNodes0 ->
            do
               (bundleNodes1,bundles) 
                  <- splitBundleNodes packageId bundleNodes0
               newBundleNodeData (Dir bundleNodes1) bundles 
   in
      case base objectType1 of
         FileEnum -> unchanged
         MMiSSFileEnum -> unchanged
         MMiSSPreambleEnum -> unchanged
         FolderEnum -> isFolder
         MMiSSFolderEnum -> isFolder
         MMiSSObjectEnum ->
            do
               (bundleNodeData2,bundles) 
                  <- splitElementBundleNodeData packageId bundleNodeData1
               newBundleNodeData bundleNodeData2 bundles
           

splitBundleNodes :: PackageId -> [BundleNode] 
   -> WithError ([BundleNode],[Bundle])   
splitBundleNodes packageId bundleNodes0 =
   do
      (bundleNodes1bundles :: [(BundleNode,[Bundle])]) <- mapM
         (splitBundleNode packageId)
         bundleNodes0
      let
         bundleNodes1 = map fst bundleNodes1bundles
         bundles = concat (map snd bundleNodes1bundles)
      return (bundleNodes1,bundles)

splitElementBundleNodeData :: PackageId -> BundleNodeData 
   -> WithError (BundleNodeData,[Bundle])
splitElementBundleNodeData packageId (Object variants0) =
   do
      (variants1bundles :: [((Maybe MMiSSVariantSpec,BundleText),[Bundle])]) 
            <- mapM
         (\ (variantSpecOpt,text0) ->
            do
               element0 <- fromBundleTextWE text0
               (element1,bundles) <- splitElement packageId element0
               let
                  text1 = mkBundleText element1 
               return ((variantSpecOpt,text1),bundles)
            )     
         variants0
      let
         variants1 = map fst variants1bundles
         bundles = concat (map snd variants1bundles)
      return (Object variants1,bundles)

-- ----------------------------------------------------------------------
-- Determining if an Element contains components that would be split out
-- ----------------------------------------------------------------------

-- | If (Just element) means that this element contains imported portions
-- which are split off by dissectElement, and returns the element obtained
-- when these portions are replaced by includes.
-- 
-- NB.  Assumes that dissectElement has in fact already succeeded on 
-- the element.
reduceElement :: Element -> Maybe Element
reduceElement element = 
   case fromWithError (splitElement (PackageId "") element) of
      Right (element,[]) -> Nothing
      Right (element,_) -> Just element
      Left mess -> error ("Unexpected error " ++ mess ++ " in reduceElement")

-- ----------------------------------------------------------------------
-- splitting individual elements
-- ----------------------------------------------------------------------

splitElement :: PackageId -> Element -> WithError (Element,[Bundle])
splitElement packageId (Elem name atts (contents0 :: [Content])) =
   do
      (contents1bundles :: [(Content,[Bundle])])  <- mapM
         (\ content0 -> case content0 of
            CElem innerElement0 ->
               do
                  (innerElement1,bundles) 
                     <- splitInnerElement packageId innerElement0
                  return (CElem innerElement1,bundles)
            _ -> return (content0,[])
            )
         contents0

      let
         contents1 = map fst contents1bundles
         bundles = concat (map snd contents1bundles)

      return (Elem name atts contents1,bundles)

splitInnerElement :: PackageId -> Element -> WithError (Element,[Bundle])
splitInnerElement packageId0 element0 =
   case mkIncludeElement element0 of
      Nothing -> splitElement packageId0 element0
      Just includeElement ->
         do
            (elementInfo,element1) <- getElementInfo element0
            let
               packageId1 = fromMaybe packageId0 (packageIdOpt elementInfo)
            (element2,bundles0) <- splitElement packageId1 element1

            packagePath <- case packagePathOpt elementInfo of
               Just packagePath -> return packagePath
               Nothing -> fail (
                  "Can't work out where to put included element with "
                  ++ (case labelOpt elementInfo of
                     Just label -> "label " ++ show label
                     Nothing -> "no label" 
                        -- this can't happen I think since mkIncludeElement
                        -- requires a label.
                     )
                  ++ "."
                  )
            let
               elementText = mkBundleText element2
               elementNodeData 
                  = Object [(Just (variants elementInfo),elementText)]

            elementPackage <- wrapContainingMMiSSPackage Nothing packagePath 
               mmissObjectType elementNodeData
            let
               elementBundle :: Bundle
               elementBundle = Bundle [(packageId1,elementPackage)]

            return (includeElement,elementBundle : bundles0)
