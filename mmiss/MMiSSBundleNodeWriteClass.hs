-- | This module contains the HasBundleNodeWrite class that
-- objects need to implement to be able to be written from
-- a bundle, and some miscellaneous instances of it. 
module MMiSSBundleNodeWriteClass(
   HasBundleNodeWrite(..),   
   BundleNodeLocations(..),
   BundleNodeExtraData(..),
   ) where

import Data.FiniteMap

import AtomString
import Computation

import EntityNames

import View
import Link
import Files
import Folders
import ObjectTypes

import MMiSSBundle
import MMiSSBundleSimpleUtils

-- ------------------------------------------------------------------------
-- Datatypes
-- ------------------------------------------------------------------------


-- A few types (in particular MMiSSPackageFolder) will need a 
-- BundleNodeLocations value.

-- Given a location of some node within a bundle (corresponding to
-- the packagePath field in MMiSSBundleSimpleUtils), the link we
-- have constructed for that node. 
newtype BundleNodeLocations = BundleNodeLocations {
   fm :: FiniteMap [EntityName] BundleNodeExtraData
   }

newtype BundleNodeExtraData = BundleNodeExtraData {
   location :: WrappedLink  -- ^ where the BundleNode is to be put
   }

-- ------------------------------------------------------------------------
-- The class
-- ------------------------------------------------------------------------

class HasBundleNodeWrite object where
   -- NB. This may make all the assumptions checked in 
   -- MMiSSBundleValidate.validateBundle
   -- and
   -- MMiSSBundleNodeCheckTypes.checkBundleNodeTypes.

   -- As for bundleNodeWrite1, but is not so general.  Instances may choose
   -- to implement either bundleNodeWrite or bundleNodeWrite1.
   bundleNodeWrite :: View -> BundleNode -> Link object -> IO ()

   -- Given a link, which may contain the existing value of the object,
   -- or may be entirely new and empty, create the object
   -- and link in its children.
   bundleNodeWrite1 :: 
     View 
     -> BundleNodeLocations 
        -- ^ all the locations in the bundle (which are preallocated)
     -> [EntityName] 
        -- ^ the location of this node in the bundle
     -> BundleNode
        -- ^ the BundleNode corresponding to this object 
     -> Link object 
        -- ^ the pre-allocated link for this object.
     -> IO (IO ())
     -- The action returned is performed after all the bundleNodeWrite's for
     -- the bundle have been completed.

   bundleNodeWrite1 view _ _ node link =
      do
         bundleNodeWrite view node link
         return done

 
-- ------------------------------------------------------------------------
-- Instance for simple file
-- ------------------------------------------------------------------------

instance HasBundleNodeWrite File where
   bundleNodeWrite view node fileLink =
      do
         let
            Just extra1 = extra . objectType . fileLoc $ node
            globalKey = fromString extra1
            
            Object [(_,text)] = bundleNodeData node

            icsl = bundleTextToAsciiICSL text
         writeToFile view fileLink globalKey icsl

-- ------------------------------------------------------------------------
-- Instance for simple folder
-- ------------------------------------------------------------------------

instance HasBundleNodeWrite Folder where
   bundleNodeWrite view node folderLink =
      do
         let
            Just extra1 = extra . objectType . fileLoc $ node
            globalKey = fromString extra1
         writeEmptyFolder view folderLink globalKey
         