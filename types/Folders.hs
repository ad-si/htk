{- In this module we implement the folder type, and a display type for
   displaying the directory structure. -}
module Folders(
   registerFolders, -- :: IO ()
      -- to be done at initialisation
   getTopFolder, 
   getInFolder,
   lookupFileName,
   newEmptyFolder,
   insertInFolder,
   ) where

import FiniteMap
import qualified IOExts(unsafePerformIO)

import Dynamics
import Computation
import AtomString
import Sink
import VariableSet
import VariableMap
import UniqueString

import BSem

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

------------------------------------------------
-- The Display Type
-- ------------------------------------------------------------------

newtype FolderDisplayType = FolderDisplayType AtomString
   -- This just holds the key to the folder.

folderDisplayType_tyCon = mkTyCon "Folders" "FolderDisplayType"

instance HasTyCon FolderDisplayType where
   tyCon _ = folderDisplayType_tyCon

instance HasCodedValue FolderDisplayType where
   encodeIO = mapEncodeIO (\ (FolderDisplayType str) -> Str str)
   decodeIO = mapDecodeIO (\ (Str str) -> FolderDisplayType str)

instance DisplayType FolderDisplayType where
   displayTypeTypeIdPrim (FolderDisplayType _) = "Folders"

   graphParmsPrim (FolderDisplayType _) = 
      GraphTitle "Directory Listing" $$
      -- We will need to add more options later for menus.
      emptyGraphParms

   displayTypeGlobalRegistry _ = displayTypeRegistry

   displayTypeIdPrim (FolderDisplayType as) = as

displayTypeRegistry :: GlobalRegistry FolderDisplayType
displayTypeRegistry = IOExts.unsafePerformIO createGlobalRegistry
{-# NOINLINE displayTypeRegistry #-}

-- ------------------------------------------------------------------
-- FolderType and its instance of HasCodedValue
-- ------------------------------------------------------------------

data FolderType = FolderType {
   folderTypeId :: AtomString,   
   requiredAttributes :: AttributesType,
   displayParms :: NodeTypes (String,Folder),
   topFolderLinkOpt :: Maybe (Link Folder),
   knownFolders :: VariableSet (Link Folder)
   }

folderType_tyCon = mkTyCon "Folders" "FolderType"
instance HasTyCon FolderType where
   tyCon _ = folderType_tyCon

instance HasCodedValue FolderType where
   encodeIO = mapEncodeIO 
      (\ (FolderType {folderTypeId = folderTypeId,
            requiredAttributes = requiredAttributes,
            displayParms = displayParms,topFolderLinkOpt = topFolderLinkOpt})
         -> (Str folderTypeId,requiredAttributes,displayParms,
               topFolderLinkOpt))
   decodeIO codedValue0 view =
      do
         ((Str folderTypeId,requiredAttributes,displayParms,topFolderLinkOpt),
            codedValue1) <- decodeIO codedValue0 view
         knownFolders <- newEmptyVariableSet
         return (FolderType {folderTypeId = folderTypeId,
            requiredAttributes = requiredAttributes,
            displayParms = displayParms,topFolderLinkOpt = topFolderLinkOpt,
            knownFolders = knownFolders},codedValue1)


-- ------------------------------------------------------------------
-- Folder and its instance of HasCodedValue
-- ------------------------------------------------------------------


data Folder = Folder {
   folderType :: FolderType,
   attributes :: Attributes,
   name :: String,
   contents :: VariableMap String WrappedLink,
   contentsLock :: BSem -- The contentsLock should be set whenever the
      -- contents are in the process of being updated.
   }

folder_tyCon = mkTyCon "Folders" "Folder"
instance HasTyCon Folder where
   tyCon _ = folder_tyCon

instance HasCodedValue Folder where
   encodeIO = mapEncodeIO 
      (\ (Folder {folderType = folderType,attributes = attributes,
             name = name,contents = contents}) ->
         (folderType,attributes,name,contents)
         )
   decodeIO codedValue0 view =
      do
         ((folderType,attributes,name,contents),codedValue1) <-
            decodeIO codedValue0 view
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

   getNodeDisplayData view wrappedDisplayType folderType =
      return (
         let
            nodeTypeParmsOpt = getNodeTypeParms wrappedDisplayType 
               (displayParms folderType)
         in
            case nodeTypeParmsOpt of
               Just nodeTypeParms ->
                  Just (NodeDisplayData {
                     topLinks = case topFolderLinkOpt folderType of
                        Nothing -> []
                        Just link -> [link],
                     arcTypes = [(theArcType,emptyArcTypeParms)],
                     nodeTypes = [(theNodeType,
                        ValueTitle (\ (str,f::Folder) -> return str) $$
                           nodeTypeParms
                        )],
                     getNodeType = const theNodeType,
                     knownSet = SinkSource (knownFolders folderType),
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

---
-- mkPlainFolderType is used to construct the folder type
-- when the repository is initialised (in getTopFolder),
-- and add it to the global registry.  It also adds the
-- folder display type to the display type registry.
getPlainFolderType :: View -> IO FolderType
getPlainFolderType view = 
   do
      knownFolders <- newEmptyVariableSet
      key <- newKey globalRegistry view
      let
         folderType = FolderType {
            folderTypeId = key,
            requiredAttributes = emptyAttributesType,
            displayParms = emptyNodeTypes,
            topFolderLinkOpt = Just topLink,
            knownFolders = knownFolders
            }

      addToGlobalRegistry globalRegistry view key folderType

      displayTypeKey <- newKey displayTypeRegistry view
      let
         displayType = FolderDisplayType displayTypeKey
      addToGlobalRegistry displayTypeRegistry view key displayType

      return folderType

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
            folderType <- getPlainFolderType view
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
-- Create a new empty folder in the view with the given name.
-- We use the inputAttributes method to get the attributes, and
-- return Nothing if the user cancels.
newEmptyFolder :: View -> FolderType -> String -> IO (Maybe (Link Folder))
newEmptyFolder view folderType name =
   do
      attributesOpt <- inputAttributes view (requiredAttributes folderType)
      case attributesOpt of
         Nothing -> return Nothing
         Just attributes ->
            do
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