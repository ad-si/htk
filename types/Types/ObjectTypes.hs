-- |
-- Description: General class repository objects need to instance.
--
-- Here we define ObjectType's and their factories
--
-- For each object we have two Haskell values with
--    associated types.  These two types are declared to be
--    functionally dependent (in the ObjectType class) so that
--    either determines the other.
-- The object itself has of course a Haskell type.
-- The operations on an object, and various other parameters of it,
--    are defined by the object's objectType.
-- Both objectTypes and objects are instances of HasCodedValue.
-- This is how they can be read in and out.
--
-- We do not fix the available objectTypes in this part of the
-- repository.  Instead, we provide a register function, which
-- is passed a type parameter.
--
-- Both objectTypes and objects must be instances of HasCodedValue.
module Types.ObjectTypes(
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
   emptySpecialNodeActions,
      -- :: HasCodedValue object => object
      -- -> SimpleSource (graph -> node (Link object) -> IO ())
      -- For the simple case of no actions.

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
   unpackWrappedLinkWE,
      -- :: ObjectType objectType object =>
      -- WrappedLink -> WithError (Link object)
   toWrappedMergeLink, -- :: WrappedLink -> WrappedMergeLink
   fromWrappedMergeLink, -- :: WrappedLink -> WrappedMergeLink -> WrappedLink
   wrappedLinkTypeName, -- :: WrappedLink -> String
      -- Get the name of the (Haskell) type within a WrappedLink.

   wrapFetchLink, -- :: View -> WrappedLink -> IO WrapVersioned
   wrapReadObject, -- :: View -> WrappedVersioned -> IO WrappedObject
   wrapReadLink, -- :: View -> WrappedLink -> IO WrappedObject
   wrapCreateLink,
      -- :: HasCodedValue x => View -> WrappedLink -> x -> IO (Link x)
   wrapNewEmptyLink,
      -- :: HasCodedValue x => View -> WrappedLink -> IO (Link x)
   wrapMoveLink,
      -- :: View -> WrappedLink -> WrappedLink -> IO ()

   wrapPreFetchLinks, -- :: View -> [WrappedLink] -> IO ()

   wrapToLinkedObjectOpt, -- :: WrappedObject -> Maybe LinkedObject

   -- Extract the object type of an object, wrapped.
   getObjectType, -- :: WrappedObject -> WrappedObjectType
   -- Extract the title of an object (the name used to describe it
   -- in daVinci) wrapped.
   nodeTitleSource, -- :: WrappedObject -> SimpleSource String

   -- Get the String corresponding to an object type's registry.
   objectTypeTypeId, -- :: WrappedObjectType -> String
   linkObjectTypeTypeId, -- :: WrappedLink -> String

   -- Get the GlobalKey for an object type, which identifies
   -- the object type among others of the same Haskell type.
   objectTypeId, -- :: WrappedObjectType -> GlobalKey

   -- extract the current title of an object.
   nodeTitleIOPrim, -- :: ObjectType objectType object => object -> IO String
   nodeTitleIO, -- :: WrappedObject -> IO String

   -- get the links which need to be preserved for this object type during
   -- merging.
   fixedLinks, -- :: View -> WrappedObjectType -> IO [WrappedLink]

   -- Get the object type's entry in the object creation menu.
   -- We return True if the object was created.  The object should be inserted
   -- in the folder.
   createObjectMenuItem, -- :: WrappedObjectType
   -- -> Maybe (String,View -> LinkedObject -> IO Bool

   -- How to save references to object types
   ShortObjectType(..),

   getObjectTypeByKey,
      -- :: ObjectType objectType object => View -> GlobalKey -> IO objectType
   getObjectTypeByKeyOpt,
      -- :: ObjectType objectType object => View -> GlobalKey
      -- -> IO (Maybe objectType)
      -- How to look up an object by its global key.

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

   getWrappedLinkFromLocation,
      -- :: View -> Location -> IO (Maybe WrappedLink)

   ) where

import System.IO.Unsafe
import qualified Data.Dynamic

import Util.Computation
import Util.Registry
import Util.ExtendedPrelude
import Util.Dynamics
import Util.Sink
import qualified Util.VariableList as VariableList
import Util.VariableSet(HasKey(..))
import Util.Sources
import Util.Thread

import Graphs.GraphDisp
import Graphs.GraphConfigure
import Graphs.Graph(ArcType,NodeType)

import SimpleDB.ServerErrors

import Types.VersionDB(Location)
import Types.CodedValue

import qualified Types.LinkDrawer as LinkDrawer
import Types.DisplayTypes
import Types.ViewType
import Types.Link
import Types.GlobalRegistry
import {-# SOURCE #-} Types.DisplayView
import {-# SOURCE #-} Types.LinkManager
import Types.MergeTypes

-- ----------------------------------------------------------------
-- The ObjectType class
-- ----------------------------------------------------------------

class (HasCodedValue objectType,HasCodedValue object,HasMerging object)
   => ObjectType objectType object
      | objectType -> object, object -> objectType where
   objectTypeTypeIdPrim :: objectType -> String
      -- ^ This function should not look at its argument but return a
      -- unique identifier for this objectType, which is totally unique
      -- across everything.
      -- To preserve uniqueness, the string should begin with the
      -- module name where the instance is defined.  If there is further
      -- information, the module name should be followed by a period.  So
      -- for a module named \"A\", \"A\" and \"A.B\" are legal values for this
      -- string, but not \"AB\" or \"C\".

   objectTypeIdPrim :: objectType -> GlobalKey
      -- ^ Returns the unique identifier for this objectType in this
      -- version.  NB - this may be changed from version to version
      -- unlike 'objectTypeTypeIdPrim'
   objectTypeGlobalRegistry :: objectType -> GlobalRegistry objectType
      -- ^ Returns a global registry associated with all objectTypes with
      -- this Haskell value.  This function should not look at its argument.
      -- The keys in this registry should be indexed according to
      -- 'objectTypeIdPrim'
   extraObjectTypes :: IO [objectType]
      -- ^ Extract any extra object types not listed in the global registry.
   getObjectTypePrim :: object -> objectType
      -- ^ Extracts the type of an object.

   createObjectTypeMenuItemPrim :: objectType -> Maybe (String,View -> IO ())
      -- ^ This is a menu item (label + creation function) which creates a new
      -- object type and inserts it in the global registry.  We do not
      -- look at the argument.

      -- This is what the outside actually calls, but the implementation may
      -- instead choose to provide 'createObjectTypeMenuItemNoInsert'.

   createObjectTypeMenuItemNoInsert ::
        Maybe (String,View -> IO (Maybe objectType))
      -- ^ This is a menu item (label + creation function) which creates a new
      -- object type but does NOT insert it in the global registry.
      --

   createObjectMenuItemPrim :: objectType
      -> Maybe (String,View -> LinkedObject -> IO Bool)
      -- ^ This is a menu item (label + creation function) which creates
      -- a link to an object of this type in the supplied linked object, and
      -- inserts it in the folder, returning True if successful.

   toLinkedObjectOpt :: object -> Maybe LinkedObject
      -- ^ Extract the object's 'LinkedObject', if any.

   nodeTitlePrim :: object -> String
      -- ^ Returns a title for the object.
      -- Either this function or 'nodeTitleSourcePrim' should be defined.

   nodeTitleSourcePrim :: object -> SimpleSource String
      -- ^ Returns a title, which may change.

   fixedLinksPrim :: View -> objectType -> IO [Link object]
      -- ^ Returns set of links which must be absolutely fixed for this object
      -- type during merging.  The merging process starts from these links
      -- and then uses 'getMergeLinks' to derive other links which need to be
      -- identified.
      --
      -- The length of the list should be independent of the view.  Indeed it
      -- is envisaged (though not assumed) that this list will be null except
      -- for the plain folder type, where it will contain just the top folder.

   nodeTitleSourcePrim object = staticSimpleSource (nodeTitlePrim object)

   nodeTitlePrim object =
      seq (nodeTitleSourcePrim object)
         (error "Only nodeTitleSourcePrim should call nodeTitlePrim!")
      -- pointless definition except that it avoids warning messages
      -- about not defining nodeTitlePrim, when nodeTitleSourcePrim is
      -- defined.

   getNodeDisplayData ::
      (GraphAllConfig graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms)
      => View -> WrappedDisplayType -> objectType
      -> IO (DisplayedView graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms)
      -> IO (Maybe (NodeDisplayData graph node nodeTypeParms arcTypeParms
            objectType object))
      -- ^ Get everything we need to display objects of this type.
      -- This will be called for each existing object type
      -- when we start a new display.

      -- Nothing means that this object is not displayed at all in the
      -- display.  The implementation is also responsible for making sure
      -- it never occurs on the RHS of a 'getNodeLinks'.

      -- NB.  Although this is an 'IO' action, the display code assumes that
      -- the result is a constant; once you've returned a value for a
      -- particular 'WrappedDisplayType', it's fixed.

      -- The 'IO' 'DisplayedView action' returns the DisplayedView in which
      -- this node is being displayed.  However it should not be executed
      -- to produce the 'NodeDisplayData' or we will get deadlock;
      -- it should only
      -- be executed as part of the actions attached to nodes and edges, when
      -- it will return quickly (provided the displayed view has actually been
      -- set up.

   getNodeDisplayData1 ::
      (GraphAllConfig graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms)
      => Graph graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms
      -> View -> WrappedDisplayType -> objectType
      -> IO (DisplayedView graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms)
      -> IO (Maybe (NodeDisplayData graph node nodeTypeParms arcTypeParms
            objectType object))
      -- ^ Slightly generalised version of getNodeDisplayData which also
      -- takes the enclosing graph (to be used as a display sort for
      -- opening new graphs).

   getNodeDisplayData1 graph = getNodeDisplayData


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
   ObjectType objectType object => WrappedObject object deriving (Typeable)

data WrappedObjectType = forall objectType object .
   ObjectType objectType object => WrappedObjectType objectType

data WrappedVersioned = forall objectType object .
   ObjectType objectType object => WrappedVersioned (Versioned object)

data WrappedLink = forall objectType object .
   ObjectType objectType object => WrappedLink (Link object)
      deriving (Typeable)

instance Eq WrappedLink where
   (==) (WrappedLink link1) (WrappedLink link2) = eqLink link1 link2

instance Ord WrappedLink where
   compare (WrappedLink link1) (WrappedLink link2) = compareLink link1 link2

-- ----------------------------------------------------------------
-- Unpacking wrapped types
-- ----------------------------------------------------------------

-- | Returns Nothing if the types don\'t match.
unpackWrappedLink :: ObjectType objectType object =>
   WrappedLink -> Maybe (Link object)
unpackWrappedLink (WrappedLink link) = fromDynamic (toDyn link)

unpackWrappedLinkWE :: forall objectType object .
   ObjectType objectType object => WrappedLink -> WithError (Link object)
unpackWrappedLinkWE (WrappedLink link) =
   case fromDynamic (toDyn link) of
      Just link2 -> hasValue link2
      (Nothing :: Maybe (Link object)) ->
         hasError ("Type failure: looking for a "
            ++ showLink (undefined :: Link object)
            ++ " but found a " ++ showLink link
            )
   where
      showLink :: forall objectType object .
          ObjectType objectType object => Link object -> String
      showLink link = objectTypeTypeIdPrim (typeHack link)

      typeHack :: forall objectType object .
          ObjectType objectType object => Link object -> objectType
      typeHack = undefined


toWrappedMergeLink :: WrappedLink -> WrappedMergeLink
toWrappedMergeLink (WrappedLink link) = WrappedMergeLink link

-- | This conversion requires an extra 'WrappedLink'.  We use the type of its
-- its contents to work out what type the new WrappedLink should contain.
-- Of course that will be the same type as is inside the 'WrappedMergeLink',
-- but we have no way of getting at that, or deducing ObjectTypes for it.
fromWrappedMergeLink :: WrappedLink -> WrappedMergeLink -> WrappedLink
fromWrappedMergeLink (WrappedLink (_ :: Link object)) (WrappedMergeLink link0)
      =
   let
      link1 :: Link object
      link1 = dynCast "ObjectTypes.fromWrappedMergeLink error" link0
   in
      WrappedLink link1

wrappedLinkTypeName :: WrappedLink -> String
wrappedLinkTypeName (WrappedLink (_ :: Link object)) =
   show (typeOf (undefined :: object))

-- ----------------------------------------------------------------
-- Some miscellaneous utilities constructed from the primitives.
-- ----------------------------------------------------------------

objectTypeTypeId :: WrappedObjectType -> String
objectTypeTypeId (WrappedObjectType objectType) =
   objectTypeTypeIdPrim objectType

linkObjectTypeTypeId :: WrappedLink -> String
linkObjectTypeTypeId (WrappedLink link)
      = objectTypeTypeIdPrim (toObjectType link)
   where
      toObjectType :: ObjectType objectType object => Link object -> objectType
      toObjectType _ = getObjectTypePrim (
         error "ObjectTypes: undefined object type")

objectTypeId :: WrappedObjectType -> GlobalKey
objectTypeId (WrappedObjectType objectType) = objectTypeIdPrim objectType

getObjectType :: WrappedObject -> WrappedObjectType
getObjectType (WrappedObject object) =
   WrappedObjectType (getObjectTypePrim object)

nodeTitleSource :: WrappedObject -> SimpleSource String
nodeTitleSource (WrappedObject object) = nodeTitleSourcePrim object

nodeTitleIOPrim :: ObjectType objectType object => object -> IO String
nodeTitleIOPrim object = readContents (nodeTitleSourcePrim object)

nodeTitleIO :: WrappedObject -> IO String
nodeTitleIO (WrappedObject object) = nodeTitleIOPrim object


fixedLinks :: View -> WrappedObjectType -> IO [WrappedMergeLink]
fixedLinks view (WrappedObjectType objectType) =
   do
      links <- fixedLinksPrim view objectType
      return (map WrappedMergeLink links)

createObjectMenuItem :: WrappedObjectType
   -> Maybe (String,View -> LinkedObject -> IO Bool)
createObjectMenuItem (WrappedObjectType objectType)
   = createObjectMenuItemPrim objectType

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
         -- ^ topLinks displays the links to start display on

      arcTypes :: [(ArcType,arcTypeParms ())],
         -- ^ the 'ArcType's used by this particular object type
         -- and the associated parameters.

      nodeTypes :: [(NodeType,nodeTypeParms (Link object))],
         -- ^ the 'NodeType's used by this particular object type
         -- and the associated parameters.

      getNodeType :: object -> NodeType,
         -- ^ retrieves the node type for a particular node.

      getNodeLinks :: Link object -> IO ArcEnds,
         -- ^ the arcs out of this node.

      specialNodeActions :: object ->
         SimpleSource (graph -> node (Link object) -> IO ())
         -- ^ allow the object to make dynamic
         -- modifications to graph nodes representing it.
         -- The module "SpecialNodeActions" can be used to generate this type.
         --
         -- NB.  This is not really a frightfully good way of getting the
         -- objects to make dynamic changes to the way they are represented,
         -- since you have to keep track of the last change if objects are
         -- deleted and then redisplayed.  A better way is to attach a
         -- 'FontStyleSource' (or appropriate source for whatever the changing
         -- appearance is) to the node.
      }

type ArcEnds = VariableList.VariableList (
   LinkDrawer.ArcData WrappedLink ArcType)

-- | Used for 'getNodeLinks' with no outarcs.
emptyArcEnds :: ArcEnds
emptyArcEnds = VariableList.emptyVariableList

-- | Used for empty 'specialNodeActions'
emptySpecialNodeActions :: HasCodedValue object => object
   -> SimpleSource (graph -> node (Link object) -> IO ())
emptySpecialNodeActions _ = SimpleSource (staticSource (\ graph node -> done))

-- ----------------------------------------------------------------
-- Registry of Object Types
-- ----------------------------------------------------------------

data WrappedObjectTypeTypeData = forall objectType object .
   ObjectType objectType object => WrappedObjectTypeTypeData objectType

objectTypeTypeDataRegistry :: Registry String WrappedObjectTypeTypeData
objectTypeTypeDataRegistry = unsafePerformIO newRegistry
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
                     ("Warning: for ObjectTypes.registerObjectType, "++
                        typeTypeId ++ " is multiply registered.")
               return (Just (WrappedObjectTypeTypeData objectType),())
            )

-- ----------------------------------------------------------------
-- Processing wrapped links and wrapped versioned objects.
-- ----------------------------------------------------------------

-- | Fetch a 'WrappedLink'
wrapFetchLink :: View -> WrappedLink -> IO WrappedVersioned
wrapFetchLink view (WrappedLink link) =
   do
      versioned <- fetchLink view link
      return (WrappedVersioned versioned)

-- | Read a 'WrappedVersioned'
wrapReadObject :: View -> WrappedVersioned -> IO WrappedObject
wrapReadObject view (WrappedVersioned versioned) =
   do
      object <- readObject view versioned
      return (WrappedObject object)

-- | Read a 'WrappedLink'
wrapReadLink :: View -> WrappedLink -> IO WrappedObject
wrapReadLink view wrappedLink =
   do
      versioned <- wrapFetchLink view wrappedLink
      wrapReadObject view versioned

-- | Create a child for a 'WrappedLink', like 'createLink'.
wrapCreateLink :: HasCodedValue x => View -> WrappedLink -> x -> IO (Link x)
wrapCreateLink view (WrappedLink parentLink) x =
   createLink view parentLink x


-- | Create a child for a 'WrappedLink', like 'newEmptyLink'
wrapNewEmptyLink :: HasCodedValue x => View -> WrappedLink -> IO (Link x)
wrapNewEmptyLink view (WrappedLink parentLink) = newEmptyLink view parentLink

wrapMoveLink :: View -> WrappedLink -> WrappedLink -> IO ()
wrapMoveLink view (WrappedLink parentLink) (WrappedLink thisLink) =
   moveLink view parentLink thisLink

-- | Get on with fetching the given 'WrappedLink's concurrently.
-- If there are any errors (for example, because the user lacks read access)
-- we ignore them.
wrapPreFetchLinks :: View -> [WrappedLink] -> IO ()
wrapPreFetchLinks view wrappedLinks =
   mapMConcurrent_
      (\ (WrappedLink link) ->
         catchError (
            do
               fetchLink view link
               done
             )
             (\ _ _ -> ())
         )
      wrappedLinks

-- | Get the LinkedObject for an object, if there is one.
wrapToLinkedObjectOpt :: WrappedObject -> Maybe LinkedObject
wrapToLinkedObjectOpt (WrappedObject object) = toLinkedObjectOpt object

-- ----------------------------------------------------------------
-- Accessing the GlobalRegistry's
-- ----------------------------------------------------------------

newtype ShortObjectType objectType = ShortObjectType objectType
   deriving (Typeable)

instance ObjectType objectType object
       => HasBinary (ShortObjectType objectType) CodingMonad where

   writeBin = mapWrite (\ (ShortObjectType objectType) ->
      objectTypeIdPrim objectType)
   readBin = mapReadViewIO (\ view key ->
      do
         objectType <- getObjectTypeByKey view key
         return (ShortObjectType objectType)
      )

-- | Retrieve an 'ObjectType' by its key.
getObjectTypeByKey
   :: ObjectType objectType object => View -> GlobalKey -> IO objectType
getObjectTypeByKey view key =
   do
      objectTypeOpt <- getObjectTypeByKeyOpt view key
      case objectTypeOpt of
         Just objectType -> return objectType
         Nothing ->
            error ("Error in ObjectTypes.getObjectTypeByKey: "
               ++ "no type with key " ++ describeGlobalKey key
               ++ " found in registry or extraObjectTypes"
               )


-- | Retrieve an 'ObjectType' by its key.
getObjectTypeByKeyOpt
   :: ObjectType objectType object => View -> GlobalKey
   -> IO (Maybe objectType)
getObjectTypeByKeyOpt view key =
   do
      let
         globalRegistry = objectTypeGlobalRegistry
            (error "Don't look at me" :: objectType)
      objectTypeOpt  <- lookupInGlobalRegistryOpt globalRegistry view key
      case objectTypeOpt of
         Just objectType -> return (Just objectType)
         Nothing ->
            do
               objectTypes <- extraObjectTypes
               let
                  objectTypeOpt = findJust
                     (\ objectType0 ->
                        if objectTypeIdPrim objectType0 == key
                           then
                              Just objectType0
                           else
                              Nothing
                        )
                     objectTypes

               return objectTypeOpt

-- -----------------------------------------------------------------
-- Initialising and writing the Global Registries
-- -----------------------------------------------------------------

-- | The String is the key into the objectTypeTypeDataRegistry;
type ObjectTypeData = [(String,CodedValue)]

-- | Decode all the object type data for this value and put it in the
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

-- | This decodes all the object types associated with a particular
-- Haskell type objectType, which is not looked at.  The codedValue represents
-- a list of type [objectType].
importOneObjectType :: ObjectType objectType object
   => objectType -> CodedValue -> View -> IO ()
importOneObjectType objectType codedValue view =
   do
      let globalRegistry = objectTypeGlobalRegistry objectType
      addViewToGlobalRegistry globalRegistry view codedValue

-- | Inverse to importObjectTypes, producing a CodedValue for all types
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

-- | This is the inverse to importOneObjectType
exportOneObjectType :: ObjectType objectType object
   => objectType -> View -> IO (Maybe CodedValue)
exportOneObjectType objectType view =
   do
      let globalRegistry = objectTypeGlobalRegistry objectType
      exportViewFromGlobalRegistry globalRegistry view

-- -----------------------------------------------------------------
-- Extract all object type-types.
-- -----------------------------------------------------------------

-- | Extract all object type-types.
getAllObjectTypeTypes :: IO [WrappedObjectTypeTypeData]
getAllObjectTypeTypes =
   do
      contents <- listRegistryContents objectTypeTypeDataRegistry
      return (map snd contents)

-- -----------------------------------------------------------------
-- -----------------------------------------------------------------

-- | Turn a 'Location' in a 'View' into a 'WrappedLink'.
-- NB.  This function will have to fail if the object is not checked out.
getWrappedLinkFromLocation :: View -> Location -> IO (Maybe WrappedLink)
getWrappedLinkFromLocation view location =
   do
      objectDataOpt <- getValueOpt (objects view) location
      let
         versionedOpt :: Maybe Dyn
         versionedOpt =
            do
               objectData <- objectDataOpt
               case objectData of
                  PresentObject {thisVersioned = thisVersioned}
                     -> return thisVersioned
                  _ -> Nothing

      case versionedOpt of
         Nothing -> return Nothing
         Just versioned ->
            do
               let
                  tryType :: ObjectType objectType object
                     => objectType -> Maybe (Versioned object)
                  tryType _ = Data.Dynamic.fromDynamic versioned

                  tryLink :: WrappedObjectTypeTypeData -> Maybe WrappedLink
                  tryLink (WrappedObjectTypeTypeData objectType) =
                     do
                        versioned <- tryType objectType
                        return (WrappedLink (makeLink versioned))

               objectTypeTypeDatas <- getAllObjectTypeTypes

               return (findJust tryLink objectTypeTypeDatas)

-- -----------------------------------------------------------------
-- Extract all ObjectTypes in a view (used for doing displays)
-- -----------------------------------------------------------------

-- | Extract all ObjectTypes in a view (used for doing displays)
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

instance HasBinary WrappedObjectType CodingMonad where
   writeBin = mapWrite
      (\ (WrappedObjectType objectType) ->
         (objectTypeTypeIdPrim objectType,
            WrapBinary (ShortObjectType objectType)
               :: WrapBinary CodingMonad
               )
         )
   readBin =
      mapReadPairViewIO
         (\ view (typeKey :: String) ->
            do
               Just (WrappedObjectTypeTypeData objectType') <-
                  getValueOpt objectTypeTypeDataRegistry typeKey
               return (WrappedRead
                  (ShortObjectType objectType')
                  (\ (ShortObjectType objectType)
                     -> WrappedObjectType objectType)
                  )
            )

-- -----------------------------------------------------------------
-- Similarly, we make WrappedLink an instance of HasCodedValue
-- -----------------------------------------------------------------

-- | Getting from an object link to the corresponding object type.
-- The value of the argument is not important.
toObjectType :: ObjectType objectType object => Link object -> objectType
toObjectType link = error "toObjectType"


-- | Getting from an object type to the corresponding object link.
-- The value of the argument is not important.
toLinkType :: ObjectType objectType object => objectType -> Link object
toLinkType objectType = error "toLinkType"


instance HasBinary WrappedLink CodingMonad where
   writeBin = mapWrite
      (\ (WrappedLink link) ->
         let
            objectType = toObjectType link
         in
            (objectTypeTypeIdPrim objectType,
               WrapBinary link :: WrapBinary CodingMonad)
         )
   readBin = mapReadPairViewIO
      (\ view (typeKey :: String) ->
         do
            Just (WrappedObjectTypeTypeData objectType) <-
               getValueOpt objectTypeTypeDataRegistry typeKey
            let
               link0 = toLinkType objectType
            return (WrappedRead link0 WrappedLink)
         )

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

