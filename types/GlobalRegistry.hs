{- A GlobalRegistry stores all the ObjectType values for a particular
   class of ObjectType, giving the map from (View,identifier) to a value
   of type ObjectType.  Thus we can think of this as a map, for each
   ObjectType, of type View -> identifier -> IO ObjectType.

   In fact GlobalRegistries are also used for DisplayTypes, but for clarity
   we ignore that in the rest of this file, since exactly the same interface
   is used and I don't want to keep having to say "DisplayType or ObjectType".

   The intention of this is that there will be exactly one of these for each
   Haskell ObjectType.  This scheme may look rather odd; why not store the
   maps in the View type?  The reason is that the View type doesn't know
   what the Haskell ObjectType's are.  So we need to set up these maps
   completely separately.  At the same time, we need to be able to look things
   up depending on the object type.
   -}
module GlobalRegistry(
   GlobalRegistry,
   GlobalKey,

   addViewToGlobalRegistry,
   createNewGlobalRegistryView,
   exportViewFromGlobalRegistry,
   deleteViewFromGlobalRegistry,

   lookupViewData,
   createGlobalRegistry,
   newKey,
   firstKey,
   oneOffKey,
   lookupInGlobalRegistry,
   addToGlobalRegistry,
   addToGlobalRegistryOpt,
   getAllElements,
   getAllElementsSinked,
   ) where

import Computation(done)
import Dynamics
import Registry
import AtomString
import UniqueString
import Sink
import VariableSet
import VariableMap

import ViewType
import CodedValue


---
-- GlobalRegistry objectType
-- contains all values of the Haskell type objectType, and a source
-- of names for keying them.
data ViewData objectType = ViewData {
   names :: UniqueStringSource, -- source of keys for objectTypes
   objectTypes :: VariableMap GlobalKey objectType
   }

newtype GlobalRegistry objectType =  GlobalRegistry 
   (Registry ViewId (ViewData objectType))

newtype GlobalKey = GlobalKey AtomString deriving (Ord,Eq)

-- ---------------------------------------------------------------
-- Instances for GlobalKey
-- ---------------------------------------------------------------

globalKey_tyRep = mkTyRep "GlobalRegistry" "GlobalKey"

instance HasTyRep GlobalKey where
   tyRep _ = globalKey_tyRep

instance HasCodedValue GlobalKey where
   encodeIO = mapEncodeIO (\ (GlobalKey a) -> Str a)
   decodeIO = mapDecodeIO (\ (Str a) -> GlobalKey a)

-- ---------------------------------------------------------------
-- What the ObjectTypes module needs to know about GlobalRegistry's.
-- ---------------------------------------------------------------

---
-- Encoding objectType is how the information for a particular sort of 
-- objectType is represented in the repository.
type Encoding objectType = ([(GlobalKey,objectType)],UniqueStringSource)

---
-- Done when we import a view, to get all information for it out.
addViewToGlobalRegistry :: HasCodedValue objectType =>
   GlobalRegistry objectType -> View -> CodedValue -> IO ()
addViewToGlobalRegistry ((GlobalRegistry globalRegistry) 
      :: GlobalRegistry objectType) view codedValue =
   do
      ((objectTypesList,names) :: Encoding objectType)
         <- doDecodeIO codedValue view
      objectTypes <- newVariableMap objectTypesList
      let
         (viewData :: ViewData objectType) = ViewData {  
            names = names,
            objectTypes = objectTypes
            }
      setValue globalRegistry (viewId view) viewData

---
-- Create data for a wholly new view
createNewGlobalRegistryView :: GlobalRegistry objectType -> View -> IO ()
createNewGlobalRegistryView 
      ((GlobalRegistry globalRegistry) :: GlobalRegistry objectType) view =
   do
      (viewData :: ViewData objectType) <- newViewData
      setValue globalRegistry (viewId view) viewData

---
-- Used to export a view to the repository.  Nothing means
-- this view doesn't appear.
exportViewFromGlobalRegistry :: HasCodedValue objectType =>
   GlobalRegistry objectType -> View -> IO (Maybe CodedValue) 
exportViewFromGlobalRegistry 
      ((GlobalRegistry globalRegistry) :: GlobalRegistry objectType) view =
   do
      viewDataOpt <- getValueOpt globalRegistry (viewId view)
      case viewDataOpt of
         Nothing -> return Nothing
         Just (
            ViewData {
               names = names,
               objectTypes = objectTypes
               } :: ViewData objectType) -> 
             do
               objectTypesList <- variableMapToList objectTypes
               codedValue <- doEncodeIO 
                  ((objectTypesList,names) :: Encoding objectType) view 
               return (Just codedValue)

---
-- Delete the objectTypes in this view.
deleteViewFromGlobalRegistry :: HasCodedValue objectType =>
   GlobalRegistry objectType -> View -> IO ()
deleteViewFromGlobalRegistry (GlobalRegistry globalRegistry) view =
  deleteFromRegistry globalRegistry (viewId view)

-- ---------------------------------------------------------------
-- What the DisplayView module needs to know about global registries.
-- ---------------------------------------------------------------

getAllElements :: GlobalRegistry objectType -> View -> IO [objectType]
getAllElements globalRegistry view =
   do
      viewData <- lookupViewData globalRegistry view
      contents <- variableMapToList (objectTypes viewData)
      return (map snd contents)

---
-- Add a sink which monitors new object types.
getAllElementsSinked :: GlobalRegistry objectType -> View -> Sink objectType 
   -> IO [objectType]
getAllElementsSinked globalRegistry view sink =
   do
      viewData <- lookupViewData globalRegistry view
      let
         sink' = coMapSink' 
            (\ newData -> case newData of
               (VariableMapUpdate (AddElement (_,object))) -> Just object
               _ -> Nothing
               )
            sink
      contents <- addOldSink (objectTypes viewData) sink'
      return (map snd (mapToList contents))

-- ---------------------------------------------------------------
-- What the implementors of object types need to know about them.
-- ---------------------------------------------------------------


---
-- create a new registry, done once at the start for each objectType.
createGlobalRegistry :: IO (GlobalRegistry objectType)
createGlobalRegistry =
   do
      globalRegistry <- newRegistry
      return (GlobalRegistry globalRegistry)

---
-- create a new name for an object type.
newKey :: GlobalRegistry objectType -> View -> IO GlobalKey
newKey globalRegistry view =
   do
      viewData <- lookupViewData globalRegistry view
      str <- newUniqueString (names viewData)
      return (mkGlobalKey str)

---
-- First key generated by newKey
firstKey :: GlobalKey
firstKey = mkGlobalKey firstUniqueString


---
-- Returns an AtomString which is different from any generated by newKey and
-- any generated oneOffKey, provided that the second argument does not contain
-- a period.
--
-- We recommend that the format should be similar to that of Dynamics.mkTyRep;
-- the first argument should be the module name, the second some module-unique
-- identifier. 
oneOffKey :: String -> String -> GlobalKey
oneOffKey mname tname = mkGlobalKey (newNonUnique (mname ++ "." ++ tname))

---
-- Lookup in the global registry.  This will normally be done from the
-- module ObjectTypes.
lookupInGlobalRegistry :: GlobalRegistry objectType -> View -> GlobalKey ->
   IO objectType
lookupInGlobalRegistry globalRegistry view key =
   do
      viewData <- lookupViewData globalRegistry view
      Just objectType <- lookupVariableMap (objectTypes viewData) key
      return objectType

---
-- Add a new object type (with name created by newKey) 
-- This is harmless if done more than once.
addToGlobalRegistry :: GlobalRegistry objectType -> View -> GlobalKey ->
      objectType -> IO ()
addToGlobalRegistry globalRegistry view key objectType =
   do
      viewData <- lookupViewData globalRegistry view
      addToVariableMap (objectTypes viewData) key objectType

---
-- Like addToGlobalRegistry, but only adds the object type if there is
-- nothing already in this map for this key.
-- NB.  Not thread-safe.
addToGlobalRegistryOpt :: GlobalRegistry objectType -> View -> GlobalKey ->
      IO objectType -> IO ()
addToGlobalRegistryOpt globalRegistry view key objectTypeAct =
   do
      viewData <- lookupViewData globalRegistry view
      variableMapData <- readContents (objectTypes viewData)
      let
         previous = lookupMap variableMapData key
      case previous of
         Nothing -> 
            do
               objectType <- objectTypeAct
               addToGlobalRegistry globalRegistry view key objectType
         Just _ -> done

---
-- (not exported but used in this section)
mkGlobalKey :: String -> GlobalKey 
mkGlobalKey str = GlobalKey (fromString str)

-- ---------------------------------------------------------------
-- Functions of internal interest.
-- ---------------------------------------------------------------

---
-- Retrieve the viewData for a view, if necessary creating a new
-- one.
lookupViewData :: GlobalRegistry objectType -> View 
   -> IO (ViewData objectType)
lookupViewData ((GlobalRegistry globalRegistry) :: GlobalRegistry objectType) 
      view =
   transformValue globalRegistry (viewId view) 
      (\ viewDataOpt ->
         do
            (viewData :: ViewData objectType) <-
               case viewDataOpt of
                  Just viewData -> return viewData
                  Nothing -> newViewData
            return (Just viewData,viewData)
         )

---
-- Creates a new viewData
newViewData :: IO (ViewData objectType)
newViewData =
   do
      names <- newUniqueStringSource
      objectTypes <- newEmptyVariableMap
      let
         viewData = ViewData {names = names,objectTypes = objectTypes}
      return viewData         
               




          



