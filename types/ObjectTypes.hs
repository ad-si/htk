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
   WrappedObjectTypeTypeData(..), 
      -- monomorphic objectType type representing a particular class (or
      -- Haskell type) of object types.
   

   NodeDisplayData(..), -- how to display a particular node type with
      -- a particular display type.
   ArcEnds,
      -- ArcEnds is a type synonym describing how the arcs to or from
      -- a node are to be described.
   emptyArcEnds,
      -- :: ArcEnds
      -- For the simple case of no arcs.

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
   WrappedLink(..), -- instance of Eq,Ord
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
   nodeTitleSource, -- :: WrappedObject -> SimpleSource String

   -- Get the String corresponding to an object type's registry.
   objectTypeTypeId, -- :: WrappedObjectType -> String

   -- extract the current title of an object.
   nodeTitleIOPrim, -- :: ObjectType objectType object => object -> IO String
   nodeTitleIO, -- :: WrappedObject -> IO String

   -- get the links which need to be preserved for this object type during
   -- merging.
   fixedLinks, -- :: View -> WrappedObjectType -> IO [WrappedLink]

   -- Get the object type's entry in the object creation menu.
   -- The object creation function is returned.  The object should be inserted
   -- in the folder.
   createObjectMenuItem, -- :: WrappedObjectType 
   -- -> Maybe (String,View -> LinkedObject -> IO (Maybe WrappedLink))

   -- How to save references to object types
   ShortObjectType(..),

   -- These functions are used (by the View module) to import and export object
   -- types.
   importObjectTypes, -- :: CodedValue -> View -> IO ()
   exportObjectTypes, -- :: View -> IO CodedValue
   

   -- getAllObjectTypes returns every object type known to the view.
   getAllObjectTypes, -- :: View -> IO [WrappedObjectType]

   -- Like getAllObjectTypes, but additionally attaches a sink to
   -- monitor new object types.
   getAllObjectTypesSinked, 
      -- :: View -> Sink WrappedObjectType -> IO [WrappedObjectType] 

   -- getAllObjectTypeTypes returns every registered sort of object type..
   getAllObjectTypeTypes, -- :: IO [WrappedObjectTypeTypeData]


   -- toObjectValue produces the object value corresponding to a given
   -- objectType.  This value should not be evaluated.
   toObjectValue, -- :: ObjectType objectType type :: objectType -> object
   ) where

import qualified IOExts(unsafePerformIO)

import Registry
import Computation
import ExtendedPrelude
import Dynamics
import Sink
import qualified VariableList
import VariableSet(HasKey(..))
import Sources

import GraphDisp
import GraphConfigure
import Graph(ArcType,NodeType)

import VersionDB(Location)
import CodedValue

import qualified LinkDrawer
import DisplayTypes
import ViewType
import Link
import GlobalRegistry
import {-# SOURCE #-} DisplayView
import {-# SOURCE #-} LinkManager
import MergeTypes

-- ----------------------------------------------------------------
-- The ObjectType class
-- ----------------------------------------------------------------

class (HasCodedValue objectType,HasCodedValue object) 
   => ObjectType objectType object
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
   extraObjectTypes :: IO [objectType]
      -- Extract any extra object types not listed in the global registry.
   getObjectTypePrim :: object -> objectType
      -- Extracts the type of an object.

   createObjectTypeMenuItemPrim :: objectType -> Maybe (String,View -> IO ())
      -- This is a menu item (label + creation function) which creates a new
      -- object type and inserts it in the global registry.  We do not
      -- look at the argument.

      -- This is what the outside actually calls, but the implementation may
      -- instead choose to provide createObjectTypeMenuItemNoInsert.

   createObjectTypeMenuItemNoInsert :: 
        Maybe (String,View -> IO (Maybe objectType))
      -- This is a menu item (label + creation function) which creates a new
      -- object type but does NOT insert it in the global registry.
      --      

   createObjectMenuItemPrim :: objectType 
      -> Maybe (String,View -> LinkedObject -> IO (Maybe (Link object)))
      -- This is a menu item (label + creation function) which creates
      -- a link to an object of this type in the supplied linked object, and 
      -- inserts it in the folder.

   toLinkedObjectOpt :: object -> Maybe LinkManager.LinkedObject
      -- Extract the object's LinkedObject, if any.

   nodeTitlePrim :: object -> String
      -- Returns a title for the object.
      -- Either this function or nodeTitleSourcePrim should be defined.

   nodeTitleSourcePrim :: object -> SimpleSource String
      -- Returns a title, which may change.

   getMergeLinks :: MergeTypes.MergeLinks object
      -- Retuns those links which need to be preserved by merging.

   attemptMerge :: MergeTypes.LinkReAssigner -> View -> Link object
      -> [(View,Link object,object)] -> IO (WithError ())
      -- Attempt to merge the links supplied in the last argument to produce
      -- a single object in (View,Link object), or return an error message.

   nodeTitleSourcePrim object = staticSimpleSource (nodeTitlePrim object)

   getNodeDisplayData :: 
      (GraphAllConfig graph graphParms node nodeType nodeTypeParms 
         arc arcType arcTypeParms)
      => View -> WrappedDisplayType -> objectType 
      -> IO (DisplayedView graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms)
      -> IO (Maybe (NodeDisplayData graph node nodeTypeParms arcTypeParms 
            objectType object))
      -- Get everything we need to display objects of this type.
      -- This will be called for each existing object type
      -- when we start a new display.
 
      -- Nothing means that this object is not displayed at all in the
      -- display.  The implementation is also responsible for making sure
      -- it never occurs on the RHS of a getNodeLinks.

      -- NB.  Although this is an IO action, the display code assumes that
      -- the result is a constant; once you've returned a value for a
      -- particular WrappedDisplayType, it's fixed.

      -- The IO DisplayedView action returns the DisplayedView in which
      -- this node is being displayed.  However it should not be executed
      -- to produce the NodeDisplayData or we will get deadlock; it should only
      -- be executed as part of the actions attached to nodes and edges, when
      -- it will return quickly (provided the displayed view has actually been
      -- set up.

   extraObjectTypes = return []

   createObjectTypeMenuItemPrim badObjectType =
      fmap
         (\ (label,createAct0) ->
            let
               createAct1 view =
                  do
                     objectTypeOpt <- createAct0 view
                     case objectTypeOpt of
                        Nothing -> done
                        Just (objectType :: objectType) ->
                           do
                              let 
                                 registry = objectTypeGlobalRegistry objectType
                                 key = objectTypeIdPrim objectType
                              addToGlobalRegistry registry view key objectType
            in
               (label,createAct1)
            )
         createObjectTypeMenuItemNoInsert

   createObjectTypeMenuItemNoInsert = Nothing
      -- Don't provide any way for the user to create new types.

   createObjectMenuItemPrim objectType = Nothing

   fixedLinksPrim _ _ = return [] 

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

instance Eq WrappedLink where
   (==) (WrappedLink link1) (WrappedLink link2) = eqLink link1 link2

instance Ord WrappedLink where
   compare (WrappedLink link1) (WrappedLink link2) = compareLink link1 link2

-- ----------------------------------------------------------------
-- Unpacking wrapped types
-- ----------------------------------------------------------------

---
-- Returns Nothing if the types don't match.
unpackWrappedLink :: ObjectType objectType object =>
    WrappedLink -> Maybe (Link object)
unpackWrappedLink (WrappedLink link) = fromDyn (toDyn link) 

-- ----------------------------------------------------------------
-- Some miscellaneous utilities constructed from the primitives.
-- ----------------------------------------------------------------

objectTypeTypeId :: WrappedObjectType -> String
objectTypeTypeId (WrappedObjectType objectType) =
   objectTypeTypeIdPrim objectType

getObjectType :: WrappedObject -> WrappedObjectType
getObjectType (WrappedObject object) = 
   WrappedObjectType (getObjectTypePrim object)

nodeTitleSource :: WrappedObject -> SimpleSource String
nodeTitleSource (WrappedObject object) = nodeTitleSourcePrim object

nodeTitleIOPrim :: ObjectType objectType object => object -> IO String
nodeTitleIOPrim object = readContents (nodeTitleSourcePrim object)

nodeTitleIO :: WrappedObject -> IO String
nodeTitleIO (WrappedObject object) = nodeTitleIOPrim object


fixedLinks :: View -> WrappedObjectType -> IO [WrappedLink]
fixedLinks view (WrappedObjectType objectType) =
   do
      links <- fixedLinksPrim view objectType
      return (map WrappedLink links)

createObjectMenuItem :: WrappedObjectType 
   -> Maybe (String,View -> LinkedObject -> IO (Maybe WrappedLink))
createObjectMenuItem (WrappedObjectType objectType) =
   fmap
      (\ (str,fn) ->
         let
            newfn view linkedObject =
               do
                  resultOpt <- fn view linkedObject
                  return (fmap
                     (\ link -> WrappedLink link)
                     resultOpt
                     )
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

data NodeDisplayData graph node nodeTypeParms arcTypeParms objectType object =
   NodeDisplayData {
      topLinks :: [Link object],
         -- topLinks displays the links to start display on

      -- For the time being, we assume that arc and node types are
      -- constant.
      -- Note that ArcType, NodeType and Arc all have local scope to
      -- this NodeDisplayData.

      arcTypes :: [(ArcType,arcTypeParms ())],
      nodeTypes :: [(NodeType,nodeTypeParms (Link object))],

      -- getNodeType retrieves the node type for a particular node.
      getNodeType :: object -> NodeType,

      -- getNodeLinks returns the arcs from this node.
      getNodeLinks :: Link object -> IO ArcEnds,

      closeDown :: IO (),
         -- This tells the display implementation it is OK to stop
         -- updating the variable set (though it may choose to do so
         -- anyway, if other people are interested).

      specialNodeActions :: object -> 
         SimpleSource (graph -> node (Link object) -> IO ())
         -- The specialNodeActions allow the object to make dynamic
         -- modifications to graph nodes representing it.
         -- The module SpecialNodeActions can be used to generate this type.
      }

type ArcEnds = VariableList.VariableList (
   LinkDrawer.ArcData WrappedLink ArcType)

emptyArcEnds :: ArcEnds
emptyArcEnds = VariableList.emptyVariableList

-- ----------------------------------------------------------------
-- Registry of Object Types
-- ----------------------------------------------------------------

data WrappedObjectTypeTypeData = forall objectType object .
   ObjectType objectType object => WrappedObjectTypeTypeData objectType

objectTypeTypeDataRegistry :: Registry String WrappedObjectTypeTypeData
objectTypeTypeDataRegistry = IOExts.unsafePerformIO newRegistry
{-# NOINLINE objectTypeTypeDataRegistry #-}

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
shortObjectType_tyRep =  mkTyRep "ObjectTypes" "ShortObjectType"

instance HasTyRep1 ShortObjectType where
   tyRep1 _ = shortObjectType_tyRep

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
--  a list of type [objectType].
importOneObjectType :: ObjectType objectType object 
   => objectType -> CodedValue -> View -> IO ()
importOneObjectType objectType codedValue view =
   do
      let globalRegistry = objectTypeGlobalRegistry objectType
      addViewToGlobalRegistry globalRegistry view codedValue 

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
      doEncodeIO objectTypeData view

---
-- This is the inverse to importOneObjectType
exportOneObjectType :: ObjectType objectType object
   => objectType -> View -> IO (Maybe CodedValue)
exportOneObjectType objectType view =
   do
      let globalRegistry = objectTypeGlobalRegistry objectType
      exportViewFromGlobalRegistry globalRegistry view

-- -----------------------------------------------------------------
-- Extract all object type-types.
-- -----------------------------------------------------------------

getAllObjectTypeTypes :: IO [WrappedObjectTypeTypeData]
getAllObjectTypeTypes = 
   do
      contents <- listRegistryContents objectTypeTypeDataRegistry
      return (map snd contents)

-- -----------------------------------------------------------------
-- Extract all ObjectTypes in a view (used for doing displays)
-- -----------------------------------------------------------------

getAllObjectTypes :: View -> IO [WrappedObjectType]
getAllObjectTypes view =
   do
      allObjectTypeTypes <- getAllObjectTypeTypes
      allWrappedObjectTypes <- mapM
         (\ (WrappedObjectTypeTypeData objectType) ->
            do
               let globalRegistry = objectTypeGlobalRegistry objectType
               objectTypes1 <- getAllElements globalRegistry view
               objectTypes2 <- extraObjectTypes
               return (map WrappedObjectType (objectTypes1 ++ objectTypes2))
            )
         allObjectTypeTypes
      return (concat allWrappedObjectTypes)

-- -----------------------------------------------------------------
-- Extract all ObjectTypes in a view and also get any updates
-- -----------------------------------------------------------------

getAllObjectTypesSinked :: View -> Sink WrappedObjectType 
   -> IO [WrappedObjectType] 
getAllObjectTypesSinked view sink =
   do
      allObjectTypeTypes <- getAllObjectTypeTypes
      allWrappedObjectTypes <- mapM
         (\ (WrappedObjectTypeTypeData objectType) ->
            do
               let 
                  globalRegistry = objectTypeGlobalRegistry objectType
                  sink' = coMapSink WrappedObjectType sink
               objectTypes1 <- getAllElementsSinked globalRegistry view sink'
               objectTypes2 <- extraObjectTypes 
               return (map WrappedObjectType (objectTypes1 ++ objectTypes2))
            )
         allObjectTypeTypes
      return (concat allWrappedObjectTypes)


-- -----------------------------------------------------------------
-- We make WrappedObjectType an instance of HasCodedValue.
-- The representation is as 
-- (displayTypeTypeIdPrim,ShortObjectType displayType)
-- -----------------------------------------------------------------

wrappedObjectType_tyRep = mkTyRep "ObjectTypes" "WrappedObjectType"
instance HasTyRep WrappedObjectType where
   tyRep _ = wrappedObjectType_tyRep

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

wrappedLink_tyRep = mkTyRep "ObjectTypes" "WrappedLink"
instance HasTyRep WrappedLink where
   tyRep _ = wrappedLink_tyRep

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
-- We make WrappedObjectType and WrappedObjectTypeTypeData instance 
-- HasKey, Eq and Ord
-- -----------------------------------------------------------------

instance HasKey WrappedObjectType (String,GlobalKey) where
   toKey (WrappedObjectType objectType) =
      (objectTypeTypeIdPrim objectType,objectTypeIdPrim objectType)

instance Eq WrappedObjectType where
   (==) = mapEq toKey

instance Ord WrappedObjectType where
   compare = mapOrd toKey

instance HasKey WrappedObjectTypeTypeData String where
   toKey (WrappedObjectTypeTypeData objectType) =
      objectTypeTypeIdPrim objectType

instance Eq WrappedObjectTypeTypeData where
   (==) = mapEq toKey

instance Ord WrappedObjectTypeTypeData where
   compare = mapOrd toKey

