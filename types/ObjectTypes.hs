{- Here we define ObjectType's and their factories

   For each object we have two Haskell values with
      associated types.  These two types are declared to be
      functionally dependent (in the ObjectType class) so that
      either determines the other.
   The object itself has of course a Haskell type.
   The operations on an object, and various other parameters of it,
      are defined by the object's objectType.
   Both objectTypes and objects are instances of HasCodedValue.
   This is how they can be read in and out.

   We do not fix the available objectTypes in this part of the
   repository.  Instead, we provide a register function, which
   is passed a type parameter.
      
   Both objectTypes and objects must be instances of HasCodedValue.

   See the file RECURSION
   -}
module ObjectTypes(
   ObjectType(..), 
      -- the class giving ALL the methods (thank heaven for functional
      -- dependencies).
   WrappedObject(..), -- monomorphic object type
   WrappedObjectType(..), -- monomorphic objectType type

   NodeDisplayData(..), -- how to display a particular node type with
      -- a particular display type.
   ArcTypeTo(..), -- gives edge information
   ArcTo(..), -- ditto
   ArcTypeFrom(..), -- ditto
   ArcFrom(..), -- ditto


   registerObjectType, -- :: ObjectType objectType object 
      -- => objectType -> IO ()
   -- Register a new Haskell type objectType.  NB - this must not be
   -- done for each new objectType, just for each Haskell type for an
   -- objectType.


   -- monomorphic links and versions
   WrappedLink(..),
   WrappedVersioned(..),

   wrapFetchLink, -- :: View -> WrappedLink -> IO WrapVersioned
   wrapReadObject, -- :: View -> WrappedVersioned -> IO WrappedObject

   -- How to save references to object types
   ShortObjectType(..),

   -- These functions are used (by the View module) to import and export object
   -- types.
   importObjectTypes, -- :: CodedValue -> View -> IO ()
   exportObjectTypes, -- :: View -> IO CodedValue
   
   ) where

import qualified IOExts(unsafePerformIO)

import Registry
import AtomString
import Computation
import Dynamics

import GraphDisp
import GraphConfigure

import CodedValue
import DisplayTypes
import ViewType
import Link
import GlobalRegistry

-- ----------------------------------------------------------------
-- The ObjectType class
-- ----------------------------------------------------------------

class (HasCodedValue objectType,HasCodedValue object) =>
   ObjectType objectType object
      | objectType -> object, object -> objectType where
   objectTypeTypeIdPrim :: objectType -> String
      -- This function should not look at its argument but return a
      -- unique identifier for this objectType, which is totally unique
      -- across everything.  
      -- To preserve uniqueness, the string should begin with the
      -- module name where the instance is defined.  If there is further 
      -- information, the module name should be followed by a period.  So
      -- for a module named "A", "A" and "A.B" are legal values for this
      -- string, but not "AB" or "C".

   objectTypeIdPrim :: objectType -> AtomString
      -- Returns the unique identifier for this objectType in this
      -- version.  NB - this may be changed from version to version
      -- unlike objectTypeTypeIdPrim
   objectTypeGlobalRegistry :: objectType -> GlobalRegistry objectType
      -- Returns a global registry associated with all objectTypes with
      -- this Haskell value.  This function should not look at its argument.
      -- The keys in this registry should be indexed according to
      -- objectTypeIdPrim
   objectIdPrim :: object -> AtomString
      -- Returns the unique identifier for this object in this version.
      -- Like objectTypeId, this may change from version to version
   getObjectTypePrim :: object -> objectType
      -- Extracts the type of an object.
   getNodeDisplayData :: 
      (HasNodeTypeConfigs nodeTypeParms,HasArcTypeConfigs arcTypeParms) 
      => WrappedDisplayType -> objectType ->
      Maybe(NodeDisplayData nodeTypeParms arcTypeParms objectType object)
      -- Get everything we need to display objects of this type.
      -- This will be called for each existing object type
      -- when we start a new display.
      -- Returning Nothing stops any value of this type being displayed.

-- ----------------------------------------------------------------
-- Basic Types
-- ----------------------------------------------------------------

data WrappedObject = forall objectType object . 
   ObjectType objectType object => WrappedObject object

data WrappedObjectType = forall objectType object .
   ObjectType objectType object => WrappedObjectType objectType

data WrappedVersioned = forall objectType object . 
   ObjectType objectType object => WrappedVersioned (Versioned object)

data WrappedLink = forall objectType object .
   ObjectType objectType object => WrappedLink (Link object)

-- ----------------------------------------------------------------
-- NodeDisplayData
-- ----------------------------------------------------------------

data NodeDisplayData nodeTypeParms arcTypeParms objectType object = 
      NodeDisplayData {
   topObjects :: View -> [Link object],
      -- extract the root objects to scan from for this particular view.
   nodeTypeParms :: nodeTypeParms (String,object),
      -- the node type parameters for constructing a node type corresponding
      -- to this object type
   displayThis :: object -> Maybe String,
      -- Return "Just String" if this object should be displayed, with the
      -- label to be used for the node.
   arcsTo :: [ArcTypeTo arcTypeParms object],
      -- Information on arcs to this object we need to construct and follow,
      -- if object is displayed
   arcsFrom :: [ArcTypeFrom arcTypeParms object],
      -- Information on arcs from this object we need to construct and follow.
   considerThese :: object -> [WrappedObject]
   }
   -- Meaning of these pointers.  For each object type we consider for display
   -- (1) all objects returned by topObjects
   -- (2) if object is considered for display, all objects in 
   --     considerThese object
   -- (3) if object is displayed, all objects at the ends of arcs returned
   --     by arcsTo or arcFrom. 
   -- If an object is considered for display, it is displayed if
   -- (1) getNodeDisplayData for its object type returns something.
   -- (2) displayThis on the object returns True.
   -- An arc returned by arcsTo or arcsFrom is displayed only when the objects
   -- at both ends are displayed.


data ArcTypeTo arcTypeParms object = ArcTypeTo {
   arcTypeParmsTo :: arcTypeParms (ArcTo object), 
      -- parameters for this arc type.
   getArcsTo :: object -> [WrappedObject]
      -- construct arcs with this arc type.
   }

data ArcTypeFrom arcTypeParms object = ArcTypeFrom {
   -- see ArcTypeTo
   arcTypeParmsFrom :: arcTypeParms (ArcFrom object),
      -- parameters
   getArcFrom :: object -> [WrappedObject]
       -- construct arcs
   }

data ArcFrom object = ArcFrom object WrappedLink
   -- arc from object to object indicated by WrappedLink
data ArcTo object = ArcTo WrappedLink object
   -- arc from object indicated by WrappedLink to object

-- ----------------------------------------------------------------
-- Registry of Object Types
-- ----------------------------------------------------------------

data WrappedObjectTypeTypeData = forall objectType object .
   ObjectType objectType object => WrappedObjectTypeTypeData objectType

objectTypeTypeDataRegistry :: Registry String WrappedObjectTypeTypeData
objectTypeTypeDataRegistry = IOExts.unsafePerformIO newRegistry

registerObjectType :: ObjectType objectType object => objectType -> IO ()
registerObjectType objectType =
   do
      let
         typeTypeId = objectTypeTypeIdPrim objectType
      transformValue objectTypeTypeDataRegistry typeTypeId
         (\ previous ->
            do
               case previous of
                  Nothing -> done
                  Just _ -> putStrLn
                     ("Warning: for ObjectTypes.rejisterObjectType, "++
                        typeTypeId ++ " is multiply registered.")
               return (Just (WrappedObjectTypeTypeData objectType),())
            )

-- ----------------------------------------------------------------
-- Processing wrapped links and wrapped versioned objects.
-- ----------------------------------------------------------------

wrapFetchLink :: View -> WrappedLink -> IO WrappedVersioned
wrapFetchLink view (WrappedLink link) =
   do
      versioned <- fetchLink view link
      return (WrappedVersioned versioned)

wrapReadObject :: View -> WrappedVersioned -> IO WrappedObject
wrapReadObject view (WrappedVersioned versioned) =
   do
      object <- readObject view versioned
      return (WrappedObject object)

-- ----------------------------------------------------------------
-- Accessing the GlobalRegistry's
-- ----------------------------------------------------------------

newtype ShortObjectType objectType = ShortObjectType objectType

-- Tycon for it
shortObjectType_tyCon =  mkTyCon "ObjectTypes" "ShortObjectType"

instance HasTyCon1 ShortObjectType where
   tyCon1 _ = shortObjectType_tyCon

instance ObjectType objectType object
       => HasCodedValue (ShortObjectType objectType) where
   encodeIO (ShortObjectType objectType) codedValue view =
      do
         let 
            globalRegistry = objectTypeGlobalRegistry objectType
            key = objectTypeIdPrim objectType

         addToGlobalRegistry globalRegistry view key objectType
         encodeIO (Str key) codedValue view

   decodeIO codedValue0 view =
      do
         (Str key,codedValue1) <- decodeIO codedValue0 view
         let 
            globalRegistry = objectTypeGlobalRegistry 
               (error "Don't look at me" :: objectType)
         objectType  <- lookupInGlobalRegistry globalRegistry view key
         return (ShortObjectType objectType,codedValue1)
         
-- -----------------------------------------------------------------
-- Initialising and writing the Global Registries
-- -----------------------------------------------------------------

---
-- The String is the key into the objectTypeTypeDataRegistry; 
type ObjectTypeData = [(String,CodedValue)]

---
-- Decode all the object type data for this value and put it in the
-- object type registers.
importObjectTypes :: CodedValue -> View -> IO ()
importObjectTypes codedValue view =
   do
      (objectTypeData :: ObjectTypeData) <- doDecodeIO codedValue view
      sequence_ (
         map
            (\ (typeKey,codedValue) ->
               do
                  Just (WrappedObjectTypeTypeData objectType) <-
                     getValueOpt objectTypeTypeDataRegistry typeKey
                  importOneObjectType objectType codedValue view
               )
            objectTypeData
         )

---
-- This decodes all the object types associated with a particular
-- Haskell type objectType, which is not looked at.  The codedValue represents
--  a list of type [objectType], encoded as for CodedValue.doEncodeMultipleIO.
importOneObjectType :: ObjectType objectType object 
   => objectType -> CodedValue -> View -> IO ()
importOneObjectType objectType codedValue view =
   do
      let globalRegistry = objectTypeGlobalRegistry objectType
 
      (objectTypes :: [objectType]) <- doDecodeMultipleIO codedValue view
      sequence_ (
         map
            (\ objectType -> addToGlobalRegistry globalRegistry view 
                  (objectTypeIdPrim objectType) objectType
               )     
            objectTypes
         )

---
-- Inverse to importObjectTypes, producing a CodedValue for all types
-- present in this view.
exportObjectTypes :: View -> IO CodedValue
exportObjectTypes view =
-- We do however have to work slightly differently to importObjectTypes,
-- going through the possible types rather than the coded value.
   do
      allObjectTypes <- listRegistryContents objectTypeTypeDataRegistry
      let
         processObjectTypes [] acc = return acc
         processObjectTypes 
            ((key,WrappedObjectTypeTypeData objectType):rest) acc =
            do
               codedValueOpt <- exportOneObjectType objectType view
               processObjectTypes rest (
                  case codedValueOpt of
                  Nothing -> acc
                  Just codedValue -> (key,codedValue) : acc
                  )
      (objectTypeData :: ObjectTypeData) 
         <- processObjectTypes allObjectTypes []
      doEncodeMultipleIO objectTypeData view

---
-- This is the inverse to importOneObjectType
exportOneObjectType :: ObjectType objectType object
   => objectType -> View -> IO (Maybe CodedValue)
exportOneObjectType objectType view =
   do
      let globalRegistry = objectTypeGlobalRegistry objectType
      exportViewFromGlobalRegistry globalRegistry view

-- -----------------------------------------------------------------
-- We make WrappedObjectType an instance of HasCodedValue.
-- The representation is as 
-- (displayTypeTypeIdPrim,ShortObjectType displayType)
-- -----------------------------------------------------------------

wrappedObjectType_tyCon = mkTyCon "ObjectTypes" "WrappedObjectType"
instance HasTyCon WrappedObjectType where
   tyCon _ = wrappedObjectType_tyCon

instance HasCodedValue WrappedObjectType where
   encodeIO (WrappedObjectType objectType) codedValue0 view =
      do
         codedValue1 
            <- encodeIO (ShortObjectType objectType) codedValue0 view
         codedValue2 
            <- encodeIO (objectTypeTypeIdPrim objectType) codedValue1 view
         return codedValue2

   decodeIO codedValue0 view =
      do
         (typeKey :: String,codedValue1) <- decodeIO codedValue0 view
         Just (WrappedObjectTypeTypeData objectType') <-
            getValueOpt objectTypeTypeDataRegistry typeKey
         (objectType,codedValue2) <- decodeIO' objectType' codedValue1 view
         return (WrappedObjectType objectType,codedValue2)

decodeIO' :: ObjectType objectType object 
   => objectType -> CodedValue -> View -> IO (objectType,CodedValue)
decodeIO' _ codedValue0 view = decodeIO codedValue0 view

      


   





