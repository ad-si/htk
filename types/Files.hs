{- Files are simple files together with attributes

   This file is very similar to Folders.hs -}
module Files(
   registerFiles, -- :: IO ()
      -- to be done at initialisation
   File,
      -- instance of HasAttributes, HasFilePath
   FileType,
   newEmptyFile,
   getPlainFileType,
   mkPlainFileType,   
   ) where

import qualified IOExts(unsafePerformIO)

import Dynamics
import Computation
import AtomString(fromString)
import Sink
import VariableSet
import UniqueString

import SimpleForm

import GraphDisp
import GraphConfigure
import Graph(ArcType,NodeType)

import View
import CodedValue
import Link
import BasicObjects
import AttributesType
import ObjectTypes
import DisplayParms
import GlobalRegistry
import CallEditor

-- ------------------------------------------------------------------
-- FileType and its instance of HasCodedValue
-- ------------------------------------------------------------------

data FileType = FileType {
   fileTypeId :: GlobalKey,
   fileTypeLabel :: Maybe String,
   requiredAttributes :: AttributesType,
   displayParms :: NodeTypes (String,Link File),
   knownFiles :: VariableSet (Link File),
   canEdit :: Bool
   }

fileType_tyRep = mkTyRep "Files" "FileType"
instance HasTyRep FileType where
   tyRep _ = fileType_tyRep

instance HasCodedValue FileType where
   encodeIO = mapEncodeIO 
      (\ (FileType {fileTypeId = fileTypeId,fileTypeLabel = fileTypeLabel,
            requiredAttributes = requiredAttributes,
            displayParms = displayParms,canEdit = canEdit})
         -> (fileTypeId,fileTypeLabel,requiredAttributes,displayParms,
            canEdit)
         )
   decodeIO codedValue0 view =
      do
         ((fileTypeId,fileTypeLabel,requiredAttributes,displayParms,
               canEdit),
            codedValue1) <- safeDecodeIO codedValue0 view
         knownFiles <- newEmptyVariableSet
         return (FileType {fileTypeId = fileTypeId,
            fileTypeLabel = fileTypeLabel,
            requiredAttributes = requiredAttributes,
            displayParms = displayParms,
            knownFiles = knownFiles,canEdit = canEdit},codedValue1)


-- ------------------------------------------------------------------
-- File and its instance of HasAttributes and HasCodedValue
-- ------------------------------------------------------------------


data File = File {
   fileType :: FileType,
   attributes :: Attributes,
   name :: String,
   simpleFile :: SimpleFile
   }

file_tyRep = mkTyRep "Files" "File"
instance HasTyRep File where
   tyRep _ = file_tyRep

instance HasAttributes File where
   readPrimAttributes object = attributes object

instance HasCodedValue File where
   encodeIO = mapEncodeIO 
      (\ (File {fileType = fileType,attributes = attributes,
             name = name,simpleFile = simpleFile}) ->
         (fileTypeId fileType,attributes,name,simpleFile)
         )
   decodeIO codedValue0 view =
      do
         ((fileTypeId,attributes,name,simpleFile),codedValue1) <-
            safeDecodeIO codedValue0 view
         fileType <- lookupInGlobalRegistry globalRegistry view fileTypeId
         return (File {fileType = fileType,attributes = attributes,
             name = name,simpleFile = simpleFile},
             codedValue1)

-- ------------------------------------------------------------------
-- The instance of ObjectType
-- ------------------------------------------------------------------

---
-- Node type 
theNodeType :: NodeType
theNodeType = fromString ""

instance ObjectType FileType File where
   objectTypeTypeIdPrim _ = "Files"
   objectTypeIdPrim objectType = fileTypeId objectType
   objectTypeGlobalRegistry _ = globalRegistry
   getObjectTypePrim file = fileType file
   nodeTitlePrim file = name file

   createObjectMenuItemPrim fileType =
      fmap
         (\ label -> (label,newEmptyFile fileType))
         (fileTypeLabel fileType)

   getNodeDisplayData view wrappedDisplayType fileType displayedViewAction =
      return (
         let
            nodeTypeParmsOpt = getNodeTypeParms wrappedDisplayType 
               (displayParms fileType)
         in
            case nodeTypeParmsOpt of
               Just nodeTypeParms ->
                  Just (NodeDisplayData {
                     topLinks = [],
                     arcTypes = [],
                     nodeTypes = 
                        let
                           parms1 = 
                              ValueTitle (\ (str,_) -> return str) $$$
                              nodeTypeParms
                        in
                           [(theNodeType,
                              if canEdit fileType
                                 then
                                    DoubleClickAction 
                                       (\ (_,link) -> editObject view link) $$$
                                    parms1
                                 else
                                    parms1
                              )],
                     getNodeType = const theNodeType,
                     knownSet = SinkSource (knownFiles fileType),
                     mustFocus = (\ _ -> return False),
                     focus = (\ link ->
                        return (SinkSource emptyVariableSet,
                           SinkSource emptyVariableSet)
                        ),
                     closeDown = done
                     })
               Nothing -> Nothing
         )              

-- ------------------------------------------------------------------
-- Getting at the SimpleFile
-- ------------------------------------------------------------------

instance HasFilePath File where
   toFilePath file = toFilePath (simpleFile file)

-- ------------------------------------------------------------------
-- The global registry and a permanently empty variable set
-- ------------------------------------------------------------------

globalRegistry :: GlobalRegistry FileType
globalRegistry = IOExts.unsafePerformIO createGlobalRegistry
{-# NOINLINE globalRegistry #-}


emptyVariableSet :: VariableSet (WrappedLink,ArcType)
emptyVariableSet = IOExts.unsafePerformIO newEmptyVariableSet
{-# NOINLINE emptyVariableSet #-}
         
-- ------------------------------------------------------------------
-- newEmptyFile
-- ------------------------------------------------------------------

-- Creating a new empty file with the given name
-- We use the inputAttributes method to get the attributes, and
-- return Nothing if the user cancels.
newEmptyFile :: FileType -> View -> IO (Maybe (Link File))
newEmptyFile fileType view =
   do
      -- Construct an extraFormItem for the name.
      extraFormItem <- mkExtraFormItem (newFormEntry "Name" "")
      attributesOpt <- inputAttributes view (requiredAttributes fileType)
         (Just extraFormItem)
      case attributesOpt of
         Nothing -> return Nothing
         Just attributes ->
            do
               name <- readExtraFormItem extraFormItem
               simpleFile <- newSimpleFile view
               let
                  file = File {
                     fileType = fileType,
                     attributes = attributes,
                     name = name,
                     simpleFile = simpleFile
                     }
               versioned <- createObject view file
               link <- makeLink view versioned
               return (Just link)

-- ------------------------------------------------------------------
-- Registering the file type
-- ------------------------------------------------------------------

registerFiles :: IO ()
registerFiles = 
   do
      registerObjectType (error "Unknown FileType" :: FileType)

-- ------------------------------------------------------------------
-- The plain file type
-- ------------------------------------------------------------------


plainFileNodeTypeParms :: NodeTypes value
plainFileNodeTypeParms =
   addNodeRule 
      AllDisplays
      (SimpleNodeAttributes { shape = Nothing, 
         nodeColor = Just (Color "green")}) 
      emptyNodeTypes

---
-- mkPlainFileType is used to construct the file type
-- when the repository is initialised (in createRepository),
-- and add it to the global registry.  It also adds the
-- file display type to the display type registry.
mkPlainFileType :: View -> IO FileType
mkPlainFileType view =
   do
      knownFiles <- newEmptyVariableSet
      let
         fileType = FileType {
            fileTypeId = plainFileKey,
            fileTypeLabel = Just "Plain file",
            requiredAttributes = emptyAttributesType,
            displayParms = plainFileNodeTypeParms,
            knownFiles = knownFiles,
            canEdit = True
            }

      addToGlobalRegistry globalRegistry view plainFileKey fileType

      return fileType


getPlainFileType :: View -> IO FileType
getPlainFileType view =
   lookupInGlobalRegistry globalRegistry view plainFileKey

plainFileKey :: GlobalKey
plainFileKey =  oneOffKey "Files" ""

