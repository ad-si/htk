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
      -- Instance of HasCodedValue

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
   nodeTypeParms :: nodeTypeParms object,
      -- the node type parameters for constructing a node type corresponding
      -- to this object type
   displayThis :: object -> Bool,
      -- Return True if this object should be displayed
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

type ObjectTypeTypeData objectType = CodedValue -> View -> 
   IO (objectType,CodedValue)

data WrappedObjectTypeTypeData = forall objectType object .
   ObjectType objectType object =>
      WrappedObjectTypeTypeData (ObjectTypeTypeData objectType)

objectTypeTypeDataRegistry :: Registry String WrappedObjectTypeTypeData
objectTypeTypeDataRegistry = IOExts.unsafePerformIO newRegistry

registerObjectType :: ObjectType objectType object => objectType -> IO ()
registerObjectType (_ :: objectType) =
   do
      let
         (objectTypeData :: ObjectTypeTypeData objectType) = decodeIO
         typeTypeId = objectTypeTypeIdPrim (undefined :: objectType)
      transformValue objectTypeTypeDataRegistry typeTypeId
         (\ previous ->
            do
               case previous of
                  Nothing -> done
                  Just _ -> putStrLn
                     ("Warning: for ObjectTypes.rejisterObjectType, "++
                        typeTypeId ++ " is multiply registered.")
               return (Just (WrappedObjectTypeTypeData objectTypeData),())
            )

wrappedObjectType_tag = mkTyCon "ObjectTypes" "WrappedObjectType"

instance HasTyCon WrappedObjectType where
   tyCon _ = wrappedObjectType_tag

instance HasCodedValue WrappedObjectType where
   encodeIO (WrappedObjectType objectType) codedValue0 view =
      do
         let typeTypeId = objectTypeTypeIdPrim objectType
         encode2IO typeTypeId objectType codedValue0 view
   decodeIO codedValue0 view =
      do
         (typeTypeId :: String,codedValue1) <- decodeIO codedValue0 view
         (decoderOpt :: Maybe WrappedObjectTypeTypeData) 
            <- getValueOpt objectTypeTypeDataRegistry typeTypeId
         case decoderOpt of
           Just (WrappedObjectTypeTypeData decoder) ->
              do
                 (objectType,codedValue2) 
                    <- decoder codedValue1 view
                 return (WrappedObjectType objectType,codedValue2)   
           Nothing -> error 
              ("ObjectTypes: objectTypeType "++typeTypeId++
              " not registered")


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
      


   





