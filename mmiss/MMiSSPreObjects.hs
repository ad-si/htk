{- An MMiSSPreObjects is a collection of objects-to-be produced from a
   single input document.  Since various checks remain to be done, and
   the input operation is supposed to be atomic, we cannot actually make any
   alterations to objects whil creating a pre-object. -}
module MMiSSPreObjects(
   ObjectLoc(..),
   PreObjects,
   emptyPreObjects,
   addObject,
   listPreObjects,
   removeObject,
   ) where

import Data.FiniteMap

import Computation
import ExtendedPrelude
import AtomString

import VersionDB
import CodedValue
import Link
import Folders
import LinkManager(LinkedObject)
import EntityNames

import MMiSSVariant
import MMiSSContent
import MMiSSObjectType
import MMiSSPackageFolder


-- ------------------------------------------------------------------
-- The datatypes
-- ------------------------------------------------------------------

---
-- A collection of new objects.
-- The inner finite map is always non-empty.
newtype PreObjects = PreObjects (
   FiniteMap ObjectLoc (FiniteMap MMiSSVariantSpec StructuredContent)
   )


---
-- This represents some object where changes are to be made.
data ObjectLoc = 
   ObjectLoc {
      package :: MMiSSPackageFolder,
         -- package in which object lies
      name :: EntityName,
         -- name of object
      ifExists :: Maybe (Link MMiSSObject)
         -- if the object already exists, a link to it.
      }

-- ------------------------------------------------------------------
-- The instances
-- ------------------------------------------------------------------

idObjectLoc :: ObjectLoc -> (MMiSSPackageFolder,EntityName)
idObjectLoc objectLoc = (package objectLoc,name objectLoc)

instance Eq ObjectLoc where
   (==) = mapEq idObjectLoc

instance Ord ObjectLoc where
   compare = mapOrd idObjectLoc

-- ------------------------------------------------------------------
-- The client interface
-- ------------------------------------------------------------------

emptyPreObjects :: PreObjects
emptyPreObjects = PreObjects emptyFM

---
-- It is an error for the same object to occur twice with the same
-- variant spec.
addObject :: ObjectLoc -> StructuredContent -> PreObjects 
   -> WithError PreObjects
addObject objectLoc content (PreObjects fm1) =
   let
      variantSpec1 = variantSpec content

      newItemWE = case lookupFM fm1 objectLoc of
         Nothing -> hasValue (unitFM variantSpec1 content)
         Just fm2 -> case lookupFM fm2 variantSpec1 of
            Nothing -> hasValue (addToFM fm2 variantSpec1 content)
            _ -> hasError (
               "Attempt to import object "++toString (label content)++
                  " in two versions with the same variants.")
               -- To Do.  Allow this when the two StructuredContent's are in
               -- fact identical.  Testing this means making StructuredContent
               -- instance Eq, which can be done in MMiSSContent with the
               -- HasCodedValue instances.
   in
      mapWithError
         (\ newItem -> PreObjects (addToFM fm1 objectLoc newItem)) 
         newItemWE

---
-- List all objects-to-be
listPreObjects :: PreObjects -> [(ObjectLoc,[StructuredContent])]
listPreObjects (PreObjects fm1) =
   map
      (\ (objectLoc,fm2) -> (objectLoc,eltsFM fm2))
      (fmToList fm1)

removeObject :: ObjectLoc -> StructuredContent -> PreObjects -> PreObjects
-- Removing an object which isn't there will simply cause error to be 
-- raised or a pattern failure, since we don't expect this to happen.
removeObject objectLoc content (PreObjects fm1) =
   let
      fm2 :: FiniteMap MMiSSVariantSpec StructuredContent
      (Just fm2) = lookupFM fm1 objectLoc
      fm2' = delFromFM fm2 (variantSpec content)
  
      fm1' = 
         if isEmptyFM fm2'
            then
               delFromFM fm1 objectLoc
            else
               addToFM fm1 objectLoc fm2'
   in
      PreObjects fm1'