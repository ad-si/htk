{- In this module we implement the folder type, and a display type for
   displaying the directory structure. -}
module Folders(
   registerFolders, -- :: IO ()
      -- to be done at initialisation
   getTopFolder, 
   getInFolder,
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

   displayTypeGlobalRegistry _ = folderTypeRegistry

   displayTypeIdPrim (FolderDisplayType as) = as

folderTypeRegistry :: GlobalRegistry FolderDisplayType
folderTypeRegistry = IOExts.unsafePerformIO createGlobalRegistry
{-# NOINLINE folderTypeRegistry #-}

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
   contents :: VariableMap String WrappedLink
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
   decodeIO = mapDecodeIO
      (\ (folderType,attributes,name,contents) ->
         Folder {folderType = folderType,attributes = attributes,
             name = name,contents = contents})



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
   nodeTitle folder = name folder 

   getNodeDisplayData view wrappedDisplayType folderType =
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
                  nodeTypes = [(theNodeType,nodeTypeParms)],
                  getNodeType = const theNodeType,
                  knownSet = SinkSource (knownFolders folderType),
                  focus = (\ link ->
                     do
                        folder <- readLink view link
                        updateSet (knownFolders folderType) (AddElement link)
                        return (mkArcs (contents folder),
                           SinkSource emptyVariableSet)
                     ),
                  closeDown = done
                  })
            Nothing -> Nothing
                  
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
-- and add it to the global registry.
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
            return (Folder {
               folderType = folderType,
               attributes = attributes,
               name = "TOP",
               contents = contents
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