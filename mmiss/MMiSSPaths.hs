{- We define a path-based mechanism for finding objects by file name
   in the repository -}
module MMiSSPaths(
   lookupByPath,
   verifyPath,

   EntityName,
   EntityPathComponent,
   EntityPath,

   topPath,

   registerMMiSSPaths,

   pathNameKey,
   ) where

import ExtendedPrelude
import Computation
import AtomString
import Dynamics

import SimpleForm

import View
import CodedValue
import ObjectTypes
import Folders
import Link
import AttributesType

-- ---------------------------------------------------------------------
-- MMiSS entity names and paths
-- Like normal paths and file names, with the highest component coming first.
-- There is no concept of an absolute path.
-- ---------------------------------------------------------------------

newtype EntityName = EntityName [String]

newtype EntityPathComponent = EntityPathComponent [String]

newtype EntityPath = EntityPath [EntityPathComponent]

---
-- topPath corresponds to the path with just one element indicating the
-- top directory.
topPath :: EntityPath
topPath = EntityPath [EntityPathComponent []]

-- ---------------------------------------------------------------------
-- Path operations
-- ---------------------------------------------------------------------

---
-- Search for an entity in the given path
lookupByPath :: View -> EntityPath -> EntityName -> IO (Maybe WrappedLink)
lookupByPath view (EntityPath []) _ = return Nothing
lookupByPath view (EntityPath ((EntityPathComponent parts1):rest)) 
      (entityName @ (EntityName parts2)) =
   do
      lookedUp <- lookupFileName view (parts1 ++ parts2)
      case lookedUp of
         Nothing -> lookupByPath view (EntityPath rest) entityName
         justWrappedLink -> return justWrappedLink

---
-- Verify that all the components in the path are indeed folders.
verifyPath :: View -> EntityPath -> IO (WithError EntityPath)
verifyPath view (entityPath @ (EntityPath pathComponents)) =
   do
      let
         verifyList :: WithError () -> [EntityPathComponent] 
            -> IO (WithError ())
         verifyList acc [] = return acc
         verifyList acc (pathComponent:pathComponents) =
            do
               we <- verifyPathComponent view pathComponent
               verifyList (mapWithError (const ()) (pairWithError acc we)) 
                  pathComponents
      verified <- verifyList (Right ()) pathComponents
      return (mapWithError (const entityPath) verified)

-- Verify that a path component is indeed a folder
verifyPathComponent :: View -> EntityPathComponent 
   -> IO (WithError ())
verifyPathComponent view (component @ (EntityPathComponent names)) =
   do
      lookedUp <- lookupFileName view names
      return (case lookedUp of
         Nothing -> 
            Left ("Path component "++toString component
               ++" does not exist")
         Just wrappedLink ->
            if linkTypeTypeId wrappedLink == objectTypeTypeIdPrim 
                  (error "MMiSSPaths.2" :: FolderType)
               then 
                  Right ()
               else
                  Left ("Path component "++toString component
                     ++" is not a folder")
         )

---
-- Extract the typetypeId of a wrapped link without actually reading the
-- contents.
linkTypeTypeId :: WrappedLink -> String
linkTypeTypeId (WrappedLink link) =
   let
      getPhantomType :: (ObjectType objectType object) => Link object 
         -> objectType 
      getPhantomType _ = error "MMiSSPaths.1"
   in
      objectTypeTypeIdPrim (getPhantomType link)

-- ---------------------------------------------------------------------
-- Turning these into strings and back.  (We don't use Read/Show because
-- we can't extract these strings as prefixes.)
-- ---------------------------------------------------------------------

---
-- EntityNames are represented with names separated by periods.
instance StringClass EntityName where
   toString (EntityName names) = unsplitByChar '.' names
   fromString str = EntityName (splitByChar '.' str)

---
-- EntityPathComponents are represented with names separated by periods.
instance StringClass EntityPathComponent where
   toString (EntityPathComponent names) = unsplitByChar '.' names
   fromString str = EntityPathComponent (splitByChar '.' str)

---
-- EntityPaths are represented as EntityPathComponents separated by colons.
instance StringClass EntityPath where
   toString (EntityPath components) 
      = unsplitByChar ':' (map toString components)
   fromString str =  EntityPath (map fromString (splitByChar ':' str))

-- ---------------------------------------------------------------------
-- Forms for EntityName's and EntityPath's.
-- ---------------------------------------------------------------------

instance FormTextField EntityName where
   makeFormString path = toString path
   readFormString str = Right (fromString str)

instance FormTextField EntityPath where
   makeFormString path = toString path
   readFormString str = Right (fromString str)

-- ---------------------------------------------------------------------
-- EntityName and EntityPath as instances of Dynamic
-- ---------------------------------------------------------------------

entityName_tyRep = mkTyRep "MMiSSPaths" "EntityName"
instance HasTyRep EntityName where
   tyRep _ = entityName_tyRep

entityPath_tyRep = mkTyRep "MMiSSPaths" "EntityPath"
instance HasTyRep EntityPath where
   tyRep _ = entityPath_tyRep

-- ---------------------------------------------------------------------
-- EntityName and EntityPath as instances of HasCodedValue.
-- ---------------------------------------------------------------------

instance HasCodedValue EntityName where
   encodeIO = mapEncodeIO (\ name -> Str name)
   decodeIO = mapDecodeIO (\ (Str name) -> name)


instance HasCodedValue EntityPath where
   encodeIO = mapEncodeIO (\ path -> Str path)
   decodeIO = mapDecodeIO (\ (Str path) -> path)

-- ---------------------------------------------------------------------
-- The key to be used to describe an EntityPath
-- ---------------------------------------------------------------------

pathNameKey :: AttributeKey
pathNameKey = mkAttributeKey "Pathname"

-- ---------------------------------------------------------------------
-- Make EntityName's and EntityPath's suitable attribute types
-- ---------------------------------------------------------------------

entityName_attributeTypeKey = mkAttributeTypeKey "MMiSSPaths" "EntityName"
instance HasAttributeTypeKey EntityName where
   attributeTypeKey _ = entityName_attributeTypeKey

entityPath_attributeTypeKey = mkAttributeTypeKey "MMiSSPaths" "EntityPath"
instance HasAttributeTypeKey EntityPath where
   attributeTypeKey _ = entityPath_attributeTypeKey


registerMMiSSPaths :: IO ()
registerMMiSSPaths =
   do
      let e = error "registerMMiSSPaths"
      registerAttribute (e :: EntityName)
      registerAttribute (e :: EntityPath)

