{- This contains the instance of HasBundleNodeWrite for MMiSS objects. -}
module MMiSSBundleNodeWriteObject(
   ) where

import Text.XML.HaXml.Types

import Computation

import BSem

import Link
import ObjectTypes
import LinkManager

import MMiSSImportExportErrors
import MMiSSBundleNodeWriteClass
import MMiSSBundle
import MMiSSBundleSimpleUtils
import MMiSSObjectType
import MMiSSVariant
import MMiSSVariantObject
import MMiSSObjectTypeType

import {-# SOURCE #-} MMiSSObjectTypeInstance
-- ---------------------------------------------------------------------------
-- The instance
-- ---------------------------------------------------------------------------

instance HasBundleNodeWrite MMiSSObject where
   bundleNodeWrite view node objectLink =
      do
         isNew <- isEmptyLink view objectLink
         if isNew
            then
               do
                  let
                     tagWE = getTag node
                  tag <- coerceImportExportIO tagWE
                  let
                     objectType = retrieveObjectType tag

                  linkedObjectWE <- newLinkedObject view 
                     (wrapMMiSSObjectLink objectLink) Nothing
                  linkedObject <- coerceImportExportIO linkedObjectWE

                  variantObject <- newEmptyVariantObject1 
                     (MMiSSObjectType.converter view linkedObject)

                  object <- createMMiSSObject objectType linkedObject 
                     variantObject
                  writeLink view objectLink object
            else
               done

         object <- readLink view objectLink
         let
            newVersions :: [(MMiSSVariantSpec,Element)]
            newVersions = 
               map
                  (\ (Just variantSpec,bundleText) ->
                     (variantSpec,
                        coerceWithError (fromBundleTextWE bundleText))
                     )
                  (toVariants node)
 
            variantObject1 = variantObject object

         mapM_
            (\ (variantSpec,element1) ->
               do
                  (variableOpt :: Maybe Variable)
                     <- lookupVariantObjectExact variantObject1 variantSpec
                  case variableOpt of
                     Nothing ->
                        do
                           elementLink <- createLink view element1
                           editLock <- newBSem
                           let
                              variable = Variable {
                                 element = elementLink,
                                 editLock = editLock
                                 }
                           writeVariantObject variantObject1 variantSpec
                              variable
                     Just variable ->
                        writeLink view (element variable) element1
               )             
            newVersions

         let
            (firstVersionSpec,_) : _ = newVersions

         pointVariantObject variantObject1 firstVersionSpec