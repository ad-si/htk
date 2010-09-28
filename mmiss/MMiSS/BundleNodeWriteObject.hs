-- | This contains the instance of HasBundleNodeWrite for MMiSS objects.
module MMiSS.BundleNodeWriteObject(
   ) where

import Text.XML.HaXml.Types

import Util.Thread

import Reactor.BSem

import Types.Link
import Types.LinkManager

import MMiSS.ImportExportErrors
import MMiSS.BundleNodeWriteClass
import MMiSS.BundleSimpleUtils
import MMiSS.ObjectType
import MMiSS.Variant
import MMiSS.VariantObject
import MMiSS.UpdateVariantObject
import MMiSS.ObjectTypeType

import {-# SOURCE #-} MMiSS.ObjectTypeInstance
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
                     (converter view linkedObject)

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
