{- In this module we implement the folder type, and a display type for
   displaying the directory structure. -}
module Folders(
   registerFolders, -- :: IO ()
      -- to be done at initialisation
   Folder,
   FolderType,
   getTopFolder, 
   getInFolder,
   lookupFileName,
   newEmptyFolder,
   insertInFolder,
   getPlainFolderType,
   plainFolderKey,

   FolderDisplayType(FolderDisplayType),
   folderDisplayKey,
   ) where

import Maybe

import FiniteMap
import qualified IOExts(unsafePerformIO)

import Dynamics
import Computation
import Sink
import VariableSet
import VariableMap
import UniqueString
import AtomString(fromString)

import BSem

import SimpleForm
import DialogWin

import GraphDisp
import GraphConfigure
import Graph(ArcType,NodeType)

import View
import CodedValue
import Link
import BasicObjects
import AttributesType
import DisplayTypes
import ObjectTypes
import DisplayParms
import GlobalRegistry
import CreateObjectMenu
import DisplayView

------------------------------------------------
-- The Display Type
-- ------------------------------------------------------------------

data FolderDisplayType = FolderDisplayType
   -- This just holds the key to the folder.

folderDisplayType_tyRep = mkTyRep "Folders" "FolderDisplayType"

instance HasTyRep FolderDisplayType where
   tyRep _ = folderDisplayType_tyRep

instance HasCodedValue FolderDisplayType where
   encodeIO = mapEncodeIO (\ FolderDisplayType -> ())
   decodeIO = mapDecodeIO (\ () -> FolderDisplayType)

instance DisplayType FolderDisplayType where
   displayTypeTypeIdPrim _  = "Folders"

   graphParmsPrim view FolderDisplayType =
      do
         globalMenu <- newObjectTypeMenu view
         return (
            globalMenu $$
            AllowDragging True $$
            GraphTitle "Directory Listing" $$
            -- We will need to add more options later for menus.
            emptyGraphParms
            )

   displayTypeGlobalRegistry _ = displayTypeRegistry

   displayTypeIdPrim FolderDisplayType = folderDisplayKey

displayTypeRegistry :: GlobalRegistry FolderDisplayType
displayTypeRegistry = IOExts.unsafePerformIO createGlobalRegistry
{-# NOINLINE displayTypeRegistry #-}

newObjectTypeMenu :: View -> IO GlobalMenu
newObjectTypeMenu view =
   do
      wrappedObjectTypeTypes <- getAllObjectTypeTypes
      let
         menuItem :: WrappedObjectTypeTypeData -> Maybe (String,IO ())
         menuItem (WrappedObjectTypeTypeData objectType) =
            fmap
               (\ (label,mkAction) -> (label,mkAction view))
               (createObjectTypeMenuItemPrim objectType) 

         menuItems :: [(String,IO ())]
         menuItems = catMaybes (map menuItem wrappedObjectTypeTypes)

         menu :: MenuPrim (Maybe String) (IO ())
         menu = Menu (Just "Create Object Type")
            (map (\ (label,action) -> Button label action) menuItems)

         globalMenu :: GlobalMenu
         globalMenu = GlobalMenu menu

      return globalMenu

-- ------------------------------------------------------------------
-- FolderType and its instance of HasCodedValue
-- ------------------------------------------------------------------

data FolderType = FolderType {
   folderTypeId :: GlobalKey,
   folderTypeLabel :: Maybe String,
      -- Menu label to be used for creating objects of this type.
   requiredAttributes :: AttributesType,
   displayParms :: NodeTypes (String,Link Folder),
   topFolderLinkOpt :: Maybe (Link Folder),
   knownFolders :: VariableSet (Link Folder)
   }

folderType_tyRep = mkTyRep "Folders" "FolderType"
instance HasTyRep FolderType where
   tyRep _ = folderType_tyRep

instance HasCodedValue FolderType where
   encodeIO = mapEncodeIO 
      (\ (FolderType {folderTypeId = folderTypeId,
            folderTypeLabel = folderTypeLabel,
            requiredAttributes = requiredAttributes,
            displayParms = displayParms,topFolderLinkOpt = topFolderLinkOpt})
         -> (folderTypeId,folderTypeLabel,requiredAttributes,displayParms,
               topFolderLinkOpt))
   decodeIO codedValue0 view =
      do
         ((folderTypeId,folderTypeLabel,requiredAttributes,displayParms,
            topFolderLinkOpt),
            codedValue1) <- safeDecodeIO codedValue0 view
         knownFolders <- newEmptyVariableSet
         return (FolderType {folderTypeId = folderTypeId,
            folderTypeLabel = folderTypeLabel,
            requiredAttributes = requiredAttributes,
            displayParms = displayParms,topFolderLinkOpt = topFolderLinkOpt,
            knownFolders = knownFolders},codedValue1)


-- ------------------------------------------------------------------
-- Folder and its instance of HasAttributes and HasCodedValue
-- ------------------------------------------------------------------


data Folder = Folder {
   folderType :: FolderType,
   attributes :: Attributes,
   name :: String,
   contents :: VariableMap String WrappedLink,
   contentsLock :: BSem -- The contentsLock should be set whenever the
      -- contents are in the process of being updated.
   }

folder_tyRep = mkTyRep "Folders" "Folder"
instance HasTyRep Folder where
   tyRep _ = folder_tyRep

instance HasAttributes Folder where
   readPrimAttributes object = attributes object

instance HasCodedValue Folder where
   encodeIO = mapEncodeIO 
      (\ (Folder {folderType = folderType,attributes = attributes,
             name = name,contents = contents}) ->
         (folderTypeId folderType,attributes,name,contents)
         )
   decodeIO codedValue0 view =
      do
         ((folderTypeId,attributes,name,contents),codedValue1) <-
            safeDecodeIO codedValue0 view
         folderType <- lookupInGlobalRegistry globalRegistry view folderTypeId
         contentsLock <- newBSem
         return (Folder {folderType = folderType,attributes = attributes,
             name = name,contents = contents,contentsLock = contentsLock},
             codedValue1)

-- ------------------------------------------------------------------
-- The instance of ObjectType
-- ------------------------------------------------------------------


theArcType :: ArcType
theArcType = fromString ""

theNodeType :: NodeType
theNodeType = fromString ""

instance ObjectType FolderType Folder where
   objectTypeTypeIdPrim _ = "Folders"
   objectTypeIdPrim objectType = folderTypeId objectType
   objectTypeGlobalRegistry _ = globalRegistry
   getObjectTypePrim folder = folderType folder
   nodeTitlePrim folder = name folder 

   createObjectMenuItemPrim folderType =
      fmap
         (\ label -> (label,newEmptyFolder folderType))
         (folderTypeLabel folderType)

   getNodeDisplayData view wrappedDisplayType folderType 
         displayedViewAction =
      return (
         let
            nodeTypeParmsOpt = getNodeTypeParms wrappedDisplayType 
               (displayParms folderType)
            focusAction (_,link) =
               do
                  displayedView <- displayedViewAction
                  focusLink displayedView link 
         in
            case nodeTypeParmsOpt of
               Just nodeTypeParms ->
                  Just (NodeDisplayData {
                     topLinks = case topFolderLinkOpt folderType of
                        Nothing -> []
                        Just link -> [link],
                     arcTypes = [(theArcType,emptyArcTypeParms)],
                     nodeTypes = [(theNodeType,
                        ValueTitle (\ (str,_) -> return str) $$$
                        (DoubleClickAction focusAction) $$$
                        addFileGesture view $$$
                        nodeTypeParms
                        )],
                     getNodeType = const theNodeType,
                     knownSet = SinkSource (knownFolders folderType),
                     mustFocus = (\ _ -> return False),
                     focus = (\ link ->
                        do
                           folder <- readLink view link
                           updateSet (knownFolders folderType) 
                              (AddElement link)
                           return (mkArcs (contents folder),
                              SinkSource emptyVariableSet)
                        ),
                     closeDown = done
                     })
               Nothing -> Nothing
         )              

-- ------------------------------------------------------------------
-- Extra option so that folders can add files.
-- ------------------------------------------------------------------

addFileGesture :: View -> NodeGesture (String,Link Folder)
addFileGesture view =
   let
      addFile (_,folderLink) =
         do
            newLinkOpt <- createObjectMenu view
            case newLinkOpt of
               Nothing -> createAlertWin "Object creation cancelled" []
               Just newLink -> 
                  do
                     success <- insertInFolder view folderLink newLink
                     if success 
                        then
                           done
                        else
                           createErrorWin 
                           "Object with this name already exists in folder" []
   in
      NodeGesture addFile

-- ------------------------------------------------------------------
-- The VariableSetSource interface to the contents list.
-- ------------------------------------------------------------------

mkArcs :: VariableMap String WrappedLink ->
    VariableSetSource (WrappedLink,ArcType)
mkArcs variableMap = mapToSinkSource 
   (\ str wrappedLink -> (wrappedLink,theArcType)) variableMap


-- ------------------------------------------------------------------
-- The global registry and a permanently empty variable set
-- ------------------------------------------------------------------

globalRegistry :: GlobalRegistry FolderType
globalRegistry = IOExts.unsafePerformIO createGlobalRegistry
{-# NOINLINE globalRegistry #-}


emptyVariableSet :: VariableSet (WrappedLink,ArcType)
emptyVariableSet = IOExts.unsafePerformIO newEmptyVariableSet
{-# NOINLINE emptyVariableSet #-}
 
-- ------------------------------------------------------------------
-- Registering the folder type and display
-- ------------------------------------------------------------------

registerFolders :: IO ()
registerFolders = 
   do
      registerObjectType (error "Unknown FolderType" :: FolderType)
      registerDisplayType 
         (error "Unknown FolderDisplayType" :: FolderDisplayType)



-- ------------------------------------------------------------------
-- The plain folder type
-- ------------------------------------------------------------------

plainFolderNodeTypeParms :: NodeTypes value
plainFolderNodeTypeParms =
   addNodeRule 
      AllDisplays
      (SimpleNodeAttributes { shape = Just Triangle, 
         nodeColor = Just (Color "green")}) 
      emptyNodeTypes

---
-- mkPlainFolderType is used to construct the folder type
-- when the repository is initialised (in getTopFolder),
-- and add it to the global registry.  It also adds the
-- folder display type to the display type registry.
mkPlainFolderType :: View -> IO FolderType
mkPlainFolderType view = 
   do
      knownFolders <- newEmptyVariableSet
      let
         folderType = FolderType {
            folderTypeId = plainFolderKey,
            folderTypeLabel = Just "Plain",
            requiredAttributes = emptyAttributesType,
            displayParms = plainFolderNodeTypeParms,
            topFolderLinkOpt = Just topLink,
            knownFolders = knownFolders
            }

      addToGlobalRegistry globalRegistry view plainFolderKey folderType
      addToGlobalRegistry displayTypeRegistry view folderDisplayKey 
         FolderDisplayType

      return folderType

getPlainFolderType :: View -> IO FolderType
getPlainFolderType view =
   lookupInGlobalRegistry globalRegistry view plainFolderKey

plainFolderKey :: GlobalKey
plainFolderKey = oneOffKey "Folders" ""

folderDisplayKey :: GlobalKey
folderDisplayKey = oneOffKey "Folders" "Display"


-- ------------------------------------------------------------------
-- Retrieving the top folder.
-- ------------------------------------------------------------------

---
-- getTopFolder returns a link to the topFolder, creating it in the exceptional
-- circumstance that it doesn't already exist in the view. 
getTopFolder :: View -> IO (Link Folder)
getTopFolder view =
   do
      versioned <- setOrGetTopLink view (
         do
            -- Create the topFolder.
            folderType <- mkPlainFolderType view
            attributes <- newEmptyAttributes view
            contents <- newEmptyVariableMap
            contentsLock <- newBSem
            return (Folder {
               folderType = folderType,
               attributes = attributes,
               name = "TOP",
               contents = contents,
               contentsLock = contentsLock
               })               
         )
      makeLink view versioned

-- ------------------------------------------------------------------
-- Indexing in a folder
-- ------------------------------------------------------------------

---
-- getInFolder returns the wrapped link indexed in the folder by the given
-- string, if it exists.
getInFolder :: View -> Link Folder -> String -> IO (Maybe WrappedLink)
getInFolder view link str =
   do
      folder <- readLink view link
      map <- readContents (contents folder)
      return (lookupMap map str)

---
-- lookupLink looks up a link by file name, given by components, with
-- the name in the highest directory first.
lookupFileName :: View -> [String] -> IO (Maybe WrappedLink)
lookupFileName view [] = return (Just (WrappedLink (topLink :: Link Folder)))
lookupFileName view (first:rest) =
   do
      topFolderLink <- getTopFolder view
      doLookup topFolderLink first rest
   where
      doLookup :: Link Folder -> String -> [String] -> IO (Maybe WrappedLink)
      doLookup link first rest =
         do
            wrappedLinkOpt <- getInFolder view link first
            case rest of
               [] -> return wrappedLinkOpt
               (first2 : rest2)  ->
                  case wrappedLinkOpt of
                     Nothing -> return Nothing
                     Just wrappedLink ->
                        case unpackWrappedLink wrappedLink of
                           Nothing -> return Nothing
                           Just link -> doLookup link first2 rest2
                     


-- ------------------------------------------------------------------
-- Updating the folder network.
-- ------------------------------------------------------------------

---
-- Create a new empty folder in the view.
-- We use the inputAttributes method to get the attributes, and
-- return Nothing if the user cancels.
newEmptyFolder :: FolderType -> View -> IO (Maybe (Link Folder))
newEmptyFolder folderType view =
   do
      -- Construct an extraFormItem for the name.
      extraFormItem <- mkExtraFormItem (newFormEntry "Name" "")
      attributesOpt <- inputAttributes view (requiredAttributes folderType)
         (Just extraFormItem)
      case attributesOpt of
         Nothing -> return Nothing
         Just attributes ->
            do
               name <- readExtraFormItem extraFormItem
               contents <- newEmptyVariableMap
               contentsLock <- newBSem
               let
                  folder = Folder {
                     folderType = folderType,
                     attributes = attributes,
                     name = name,
                     contents = contents,
                     contentsLock = contentsLock
                     }
               versioned <- createObject view folder
               link <- makeLink view versioned
               return (Just link)

---
-- insertInFolder attempts to insert an object into a folder.  It
-- fails and returns False if the attempt fails because an object 
-- with that name is already in the folder.
insertInFolder :: View -> Link Folder -> WrappedLink -> IO Bool
insertInFolder view folderLink wrappedLink =
   do
      versioned <- fetchLink view folderLink
      folder <- readObject view versioned

      wrappedObject <- wrapReadLink view wrappedLink
      let
         name = nodeTitle wrappedObject

      dirtyObject view versioned 
      -- To be on the safe side, we dirty the folder before changing it.
      synchronize (contentsLock folder) (
         do
            map <- readContents (contents folder)
            case lookupMap map name of
               Just _ -> return False
               Nothing ->
                  do
                     updateMap (contents folder) 
                        (VariableMapUpdate (AddElement (name,wrappedLink)))
                     return True
         )