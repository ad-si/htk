-- | updateVariantObject is used for adding a series of new variants to an
-- object (presumably an MMiSS object or MMiSS files).
-- 
-- This is complicated because
-- (a) we avoid writing a new version when an identical old version exists.
-- (b) we always set the pointer of the variant object to the first new
--     version.  (This is an especially good idea when initialising since it
--     puts something in the cache.)
-- (c) we compute the dirty flags and dirty the parent link if anything is
--     changed.
module MMiSSUpdateVariantObject(
   updateVariantObject,
   ) where

import Thread
import Computation

import ObjectTypes
import Link
import View
import CodedValue

import MMiSSVariantObject
import MMiSSVariant

updateVariantObject 
   :: (HasCodedValue parent,HasCodedValue element,Eq element) 
   => View
   -> Link parent 
      -- ^ the object containing the variant dictionary to be updated.
   -> VariantObject object cache 
      -- ^ the variant dictionary
   -> (object -> Link element)
      -- ^ Finding the corresponding element within an object
   -> (Link element -> IO object)
      -- ^ Constructing a new object given an element 
   -> [(MMiSSVariantSpec,element)]
      -- ^ the new data
   -> IO ()
updateVariantObject view 
      (parentLink :: Link parent) 
      (variantObject :: VariantObject object cache) 
      (toElementLink :: object -> Link element)
      (mkObject :: Link element -> IO object) 
      (newData :: [(MMiSSVariantSpec,element)])
      =
   do
      (dirtyFlags :: [Bool]) <- mapMConcurrentExcep
         (\ (variantSpec,element1) ->
            do
               (objectOpt :: Maybe (object,MMiSSVariantSpec))
                  <- lookupVariantObjectAlmostExactWithSpec variantObject 
                     variantSpec
               let
                  createNewVariant =
                     do
                        elementLink <- createLink view element1
                        object <- mkObject elementLink
                        writeVariantObject variantObject variantSpec object
                        return True

               case objectOpt of
                  Nothing -> createNewVariant
                  Just (object,variantSpec2) ->
                     if variantSpec2 /= variantSpec
                        then
                           -- an older version exists.  Create a new
                           -- variant if the elements are different.
                           do
                              oldElement 
                                 <- readLink view (toElementLink object)
                              if oldElement /= element1
                                 then
                                    createNewVariant
                                 else
                                    return False
                        else
                           do
                              -- we already have something with this 
                              -- version.
                              isChanged <- writeLinkIfNe view 
                                 (toElementLink object) element1
                              return isChanged
            )
         newData

      let
         ((firstVariantSpec,_) : _) = newData

         firstWasChanged : _ = dirtyFlags

         dirtyParent = dirtyLink view parentLink
      
      if firstWasChanged
         then
            do
               pointVariantObjectAlwaysConvert variantObject firstVariantSpec
               dirtyParent
         else
            do
               dirty2 <- pointVariantObject variantObject firstVariantSpec
               let
                  somethingIsDirty = or (dirty2 : dirtyFlags)

               when somethingIsDirty (dirtyLink view parentLink)

    
   