{- This file defines an extremely simple non-path based mechanism for
   searching for objects inside the same folder as a given object.

   Hopefully we will, one day, replace it with something better,
   like MMiSSPaths.hs
   -}
module MMiSSPathsSimple(
   EntityName, -- Instance of HasCodedValue, StringClass
   lookupByObject, -- look up an entity for an object
   checkLookup, -- check a lookup.
   ) where

import Maybe

import Computation
import Dynamics
import VariableSet
import AtomString

import SimpleForm
import DialogWin

import CodedValue
import Folders
import View
import AttributesType
import ObjectTypes

-- ---------------------------------------------------------------------
-- MMiSS entity
-- ---------------------------------------------------------------------

newtype EntityName = EntityName String

-- ---------------------------------------------------------------------
-- Turning these into strings and back.  (We don't use Read/Show because
-- we can't extract these strings as prefixes.)
-- ---------------------------------------------------------------------

---
-- EntityNames are represented with names separated by periods.
instance StringClass EntityName where
   toString (EntityName name) = name
   fromString str = EntityName str

-- ---------------------------------------------------------------------
-- Forms for EntityName's
-- ---------------------------------------------------------------------

instance FormTextField EntityName where
   makeFormString path = toString path
   readFormString str = hasValue (fromString str)

-- ---------------------------------------------------------------------
-- EntityName as instances of Dynamic
-- ---------------------------------------------------------------------

entityName_tyRep = mkTyRep "MMiSSPaths" "EntityName"
instance HasTyRep EntityName where
   tyRep _ = entityName_tyRep

-- ---------------------------------------------------------------------
-- EntityName as instance of HasCodedValue.
-- ---------------------------------------------------------------------

instance HasCodedValue EntityName where
   encodeIO = mapEncodeIO (\ name -> Str name)
   decodeIO = mapDecodeIO (\ (Str name) -> name)

-- ---------------------------------------------------------------------
-- Make EntityName's  suitable attribute types
-- ---------------------------------------------------------------------

entityName_attributeTypeKey = mkAttributeTypeKey "MMiSSPaths" "EntityName"
instance HasAttributeTypeKey EntityName where
   attributeTypeKey _ = entityName_attributeTypeKey

registerMMiSSPaths :: IO ()
registerMMiSSPaths =
   do
      let e = error "registerMMiSSPaths"
      registerAttribute (e :: EntityName)

-- ---------------------------------------------------------------------
-- Look up an EntityName for an object implementing pathNameKey
-- ---------------------------------------------------------------------

lookupByObject :: HasParent object => View -> object -> EntityName 
   -> IO (Maybe WrappedLink)
lookupByObject view object (EntityName name) =
   case toParent object of
      Nothing -> error ("MMiSSPathsSimple: object doesn't have a parent!")
      Just folderLink -> getInFolder view folderLink name
 
-- ---------------------------------------------------------------------
-- Used for providing an error message
-- ---------------------------------------------------------------------

checkLookup :: (EntityName -> IO (Maybe WrappedLink)) 
   -> (EntityName -> IO (Maybe WrappedLink))
checkLookup lookupFn entityName =
   do
      wrappedLinkOpt <- lookupFn entityName
      if isNothing wrappedLinkOpt
         then
            createErrorWin
               ("MMiSS object "++toString entityName++" cannot be found")
               []
         else
            done
      return wrappedLinkOpt  

-- ---------------------------------------------------------------------
-- Make EntityName accessible by HasKey
-- ---------------------------------------------------------------------

instance HasKey EntityName String where
   toKey (EntityName str) = str