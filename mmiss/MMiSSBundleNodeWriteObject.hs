-- | This contains the instance of HasBundleNodeWrite for MMiSS objects. 
module MMiSSBundleNodeWriteObject(
   ) where

import Text.XML.HaXml.Types

import Thread

import BSem

import Link
import LinkManager

import MMiSSImportExportErrors
import MMiSSBundleNodeWriteClass
import MMiSSBundleSimpleUtils
import MMiSSObjectType
import MMiSSVariant
import MMiSSVariantObject
import MMiSSUpdateVariantObject
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

         updateVariantObject view objectLink (variantObject object)
            element 
            (\ elementLink ->
               do
                  editLock <- newBSem
                  return (Variable {
                     element = elementLink,
                     editLock = editLock
                     })
               )
            newVersions
