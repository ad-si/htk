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
   ArcType,  -- ArcType and NodeType are labels provided by the object type
      -- implementation for particular arcs and arc types in a display.
      -- They correspond to Strings and are instances of StringClass; this is
      -- how you create and read them.
   NodeType, 


   registerObjectType, -- :: ObjectType objectType object 
      -- => objectType -> IO ()
   -- Register a new Haskell type objectType.  NB - this must not be
   -- done for each new objectType, just for each Haskell type for an
   -- objectType.


   -- monomorphic links and versions
   WrappedLink(..),
   WrappedVersioned(..),


   -- Unpacking wrapped types.
   unpackWrappedLink,  -- :: ObjectType objectType object =>
     -- WrappedLink -> Maybe (Link object)

   wrapFetchLink, -- :: View -> WrappedLink -> IO WrapVersioned
   wrapReadObject, -- :: View -> WrappedVersioned -> IO WrappedObject
   wrapReadLink, -- :: View -> WrappedLink -> IO WrappedObject
   
   -- Extract the object type of an object, wrapped.
   getObjectType, -- :: WrappedObject -> WrappedObjectType
   -- Extract the title of an object (the name used to describe it
   -- in daVinci) wrapped.
   nodeTitle, -- :: WrappedObject -> String

   -- Get the object type's entry in the object creation menu.
   createObjectMenuItem, -- :: WrappedObjectType 
   -- -> Maybe (String,View -> IO (Maybe WrappedLink))

   -- How to save references to object types
   ShortObjectType(..),

   -- These functions are used (by the View module) to import and export object
   -- types.
   importObjectTypes, -- :: CodedValue -> View -> IO ()
   exportObjectTypes, -- :: View -> IO CodedValue
   

   -- getAllObjectTypes returns every object type known to the view.
   getAllObjectTypes, -- :: View -> IO [WrappedObjectType]


   -- toObjectValue produces the object value corresponding to a given
   -- objectType.  This value should not be evaluated.
   toObjectValue, -- :: ObjectType objectType type :: objectType -> object
   ) where

import qualified IOExts(unsafePerformIO)

import Registry
import Computation
import Dynamics
import Sink
import VariableSet

import GraphDisp
import GraphConfigure
import Graph(ArcType,NodeType)

import VersionDB(Location)
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

   objectTypeIdPrim :: objectType -> GlobalKey
      -- Returns the unique identifier for this objectType in this
      -- version.  NB - this may be changed from version to version
      -- unlike objectTypeTypeIdPrim
   objectTypeGlobalRegistry :: objectType -> GlobalRegistry objectType
      -- Returns a global registry associated with all objectTypes with
      -- this Haskell value.  This function should not look at its argument.
      -- The keys in this registry should be indexed according to
      -- objectTypeIdPrim
   getObjectTypePrim :: object -> objectType
      -- Extracts the type of an object.

   nodeTitlePrim :: object -> String
      -- Returns a title for the object, to be used to index it in containing
      -- folders.

   createObjectMenuItemPrim :: objectType 
      -> Maybe (String,View -> IO (Maybe (Link object)))
      -- This is a menu item (label + creation function) which creates
      -- a link to an object of this type.

   getNodeDisplayData :: 
      (HasNodeTypeConfigs nodeTypeParms,HasArcTypeConfigs arcTypeParms) 
      => View -> WrappedDisplayType -> objectType ->
         IO (Maybe
           (NodeDisplayData nodeTypeParms arcTypeParms objectType object))
      -- Get everything we need to display objects of this type.
      -- This will be called for each existing object type
      -- when we start a new display.
      -- Returning Nothing stops any value of this type being displayed.

      -- NB.  Although this is an IO action, the display code assumes that
      -- the result is a constant; once you've returned a value for a
      -- particular WrappedDisplayType, it's fixed.

toObjectValue :: ObjectType objectType object => objectType -> object
toObjectValue _ = error "ObjectTypes.toObjectValue value evaluted!"

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
-- Unpacking wrapped types
-- ----------------------------------------------------------------

---
-- Returns Nothing if the types don't match.
unpackWrappedLink :: ObjectType objectType object =>
    WrappedLink -> Maybe (Link object)
unpackWrappedLink (WrappedLink link) = fromDyn (toDyn link) 

-- ----------------------------------------------------------------
-- Some non-prim functions on WrappedObject's.
-- ----------------------------------------------------------------

getObjectType :: WrappedObject -> WrappedObjectType
getObjectType (WrappedObject object) = 
   WrappedObjectType (getObjectTypePrim object)

nodeTitle :: WrappedObject -> String
nodeTitle (WrappedObject object) = nodeTitlePrim object

createObjectMenuItem :: WrappedObjectType 
   -> Maybe (String,View -> IO (Maybe WrappedLink))
createObjectMenuItem (WrappedObjectType objectType) =
   fmap
      (\ (str,fn) ->
         let
            newfn view =
               fmap (fmap WrappedLink) (fn view)
         in
            (str,newfn)
         ) 
      (createObjectMenuItemPrim objectType)

-- ----------------------------------------------------------------
-- NodeDisplayData
-- We make heavy use of Sinks for these.
-- ----------------------------------------------------------------


instance HasKey WrappedLink Location where
   toKey (WrappedLink link) = toKey link

instance HasKey (WrappedLink,ArcType) Location where
   toKey (wrappedLink,arcType) = toKey wrappedLink

data NodeDisplayData nodeTypeParms arcTypeParms objectType object =
   NodeDisplayData {
      topLinks :: [Link object],
         -- topLinks displays the links to start display on

      -- For the time being, we assume that arc and node types are
      -- constant.
      -- Note that ArcType, NodeType and Arc all have local scope to
      -- this NodeDisplayData.

      arcTypes :: [(ArcType,arcTypeParms ())],
      nodeTypes :: [(NodeType,nodeTypeParms (String,Link object))],

      -- getNodeType retrieves the node type for a particular node.
      getNodeType :: object -> NodeType,

      -- We maintain a set of objects called the knownSet.  The knownSet
      -- contains known objects of this type; in particular it includes
      -- all new created objects of this type in this view.
      knownSet :: VariableSetSource (Link object),

      -- Returns True if we must focus the object (display edges from
      -- focus) whenever we display it.
      mustFocus :: Link object -> IO Bool,

      -- focus returns variable sets for the arcs from and to a given
      -- object.  Every arc should appear in exactly one of these sets.
      focus :: Link object -> 
         IO (VariableSetSource (WrappedLink,ArcType),
            VariableSetSource (WrappedLink,ArcType)),

      closeDown :: IO ()
         -- This tells the display implementation it is OK to stop
         -- updating the variable set (though it may choose to do so
         -- anyway, if other people are interested).
      }

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

wrapReadLink :: View -> WrappedLink -> IO WrappedObject
wrapReadLink view wrappedLink =
   do
      versioned <- wrapFetchLink view wrappedLink
      wrapReadObject view versioned 

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
         encodeIO key codedValue view

   decodeIO codedValue0 view =
      do
         (key,codedValue1) <- safeDecodeIO codedValue0 view
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
      (objectTypeData :: ObjectTypeData) <- doDecodeMultipleIO codedValue view
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
-- Extract all ObjectTypes in a view (used for doing displays)
-- -----------------------------------------------------------------

getAllObjectTypes :: View -> IO [WrappedObjectType]
getAllObjectTypes view =
   do
      allObjectTypeTypes <- listRegistryContents objectTypeTypeDataRegistry
      allWrappedObjectTypes <- mapM
         (\ (_,WrappedObjectTypeTypeData objectType) ->
            do
               let globalRegistry = objectTypeGlobalRegistry objectType
               objectTypes <- getAllElements globalRegistry view
               return (map WrappedObjectType objectTypes)
            )
         allObjectTypeTypes
      return (concat allWrappedObjectTypes)

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
         (typeKey :: String,codedValue1) <- safeDecodeIO codedValue0 view
         Just (WrappedObjectTypeTypeData objectType') <-
            getValueOpt objectTypeTypeDataRegistry typeKey
         (objectType,codedValue2) <- decodeIO' objectType' codedValue1 view
         return (WrappedObjectType objectType,codedValue2)

decodeIO' :: ObjectType objectType object 
   => objectType -> CodedValue -> View -> IO (objectType,CodedValue)
decodeIO' _ codedValue0 view = safeDecodeIO codedValue0 view

-- -----------------------------------------------------------------
-- Similarly, we make WrappedLink an instance of HasCodedValue
-- -----------------------------------------------------------------

wrappedLink_tyCon = mkTyCon "ObjectTypes" "WrappedLink"
instance HasTyCon WrappedLink where
   tyCon _ = wrappedLink_tyCon

---
-- The only important thing about the value returned by toObjectType is 
-- its types; the value itself are undefined.
toObjectType :: ObjectType objectType object => Link object -> objectType
toObjectType link = error "toLinkType"

---
-- toLinkType is similar but returns a decodeIO action for the link type.
toLinkType :: ObjectType objectType object => objectType -> 
   (CodedValue -> View -> IO (Link object,CodedValue))
toLinkType objectType = safeDecodeIO

instance HasCodedValue WrappedLink where
   encodeIO (WrappedLink link) codedValue0 view =
      do
         let objectType = toObjectType link
         codedValue1 <- encodeIO link codedValue0 view
         codedValue2 
            <- encodeIO (objectTypeTypeIdPrim objectType) codedValue1 view
         return codedValue2

   decodeIO codedValue0 view =
      do
         (typeKey :: String,codedValue1) <- safeDecodeIO codedValue0 view
         Just (WrappedObjectTypeTypeData objectType) <-
            getValueOpt objectTypeTypeDataRegistry typeKey
         (link,codedValue2) <- toLinkType objectType codedValue1 view
         return (WrappedLink link,codedValue2)

-- -----------------------------------------------------------------
-- We make WrappedObjectType an instance of HasKey
-- -----------------------------------------------------------------

instance HasKey WrappedObjectType (String,GlobalKey) where
   toKey (WrappedObjectType objectType) =
      (objectTypeTypeIdPrim objectType,objectTypeIdPrim objectType)

