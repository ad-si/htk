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
import AtomString(fromString,toString)
import Sink
import Sources
import Broadcaster
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
import GetAttributesType
import Folders
import EntityNames
import LinkManager
import MergeTypes
import MergePrune


-- ------------------------------------------------------------------
-- FileType and its instance of HasCodedValue and HasAttributesType
-- ------------------------------------------------------------------

data FileType = FileType {
   fileTypeId :: GlobalKey,
   fileTypeLabel :: Maybe String,
   requiredAttributes :: AttributesType,
   displayParms :: NodeTypes (Link File),
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
         return (FileType {fileTypeId = fileTypeId,
            fileTypeLabel = fileTypeLabel,
            requiredAttributes = requiredAttributes,
            displayParms = displayParms,
            canEdit = canEdit},codedValue1)

instance HasAttributesType FileType where
   toAttributesType fileType = requiredAttributes fileType

-- ------------------------------------------------------------------
-- File and its instance of HasAttributes and HasCodedValue
-- ------------------------------------------------------------------


data File = File {
   fileType :: FileType,
   attributes :: Attributes,
   linkedObject :: LinkedObject,
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
             linkedObject = linkedObject,simpleFile = simpleFile}) ->
         (fileTypeId fileType,attributes,linkedObject,simpleFile)
         )
   decodeIO codedValue0 view =
      do
         ((fileTypeId,attributes,linkedObject,simpleFile),codedValue1) <-
            safeDecodeIO codedValue0 view
         fileType <- lookupInGlobalRegistry globalRegistry view fileTypeId
         return (File {fileType = fileType,attributes = attributes,
             linkedObject = linkedObject,simpleFile = simpleFile},
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
   toLinkedObjectOpt file = Just (linkedObject file)
   nodeTitleSourcePrim file =
      fmap toString 
         (getLinkedObjectTitle (linkedObject file) (fromString "NOT INSERTED"))

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
                           allowTextEdit = canEdit fileType

                           textEdit link = editObject view link

                           editOptions1 = [
                              Button "Edit Attributes" 
                                 (\ link -> editObjectAttributes view link)
                                 ]

                           editOptions2 =
                              if allowTextEdit
                                 then
                                    (Button "Edit Text" textEdit) 
                                       : editOptions1
                                 else
                                    editOptions1

                           menu = 
                              LocalMenu (Menu (Just "File edits") editOptions2)

                           parms1 = 
                              menu $$$ 
                              valueTitleSource view $$$
                              nodeTypeParms

                           parms2 =
                              if allowTextEdit 
                                 then
                                    (DoubleClickAction textEdit) $$$ parms1
                                 else
                                    parms1
                        in
                           [(theNodeType,parms2)],
                     getNodeType = const theNodeType,
                     getNodeLinks = (\ link -> return emptyArcEnds),
                     closeDown = done,
                     specialNodeActions = (\ _ ->
                        SimpleSource (staticSource (\ graph node -> done))
                        )
                     })
               Nothing -> Nothing
         )


   -- Merging information
   getMergeLinks = MergeLinks (\ view link ->
      do
         file <- readLink view link
         return (getMergeLinksSimpleFile (simpleFile file))
      )

   attemptMerge = (\ linkReAssigner newView newLink vlos ->
      -- We don't do any serious merging, just removing obvious prunable
      -- versions.
      do
         vlosPruned <- mergePrune vlos
         case vlosPruned of
            [] -> error "Files: strange, empty merge list??"
            a:b:_ -> return (hasError
               ("Sorry, can't merge simple files at all"))
            [(view,fileLink,file)] ->
               do
                  let
                     fileType1 = fileType file
                     attributes1 = attributes file
                  linkedObject1WE <- attemptLinkedObjectMerge
                     linkReAssigner newView newLink [(view,linkedObject file)]
                  mapWithErrorIO
                     (\ linkedObject1 ->
                        do
                           let
                              simpleFile1 = attemptMergeSimpleFile 
                                 linkReAssigner newView (simpleFile file)

                              newFile = File {
                                 fileType = fileType1,
                                 attributes = attributes1,
                                 simpleFile = simpleFile1,
                                 linkedObject = linkedObject1
                                 }
                           setLink newView newFile newLink
                           done
                        )
                     linkedObject1WE
      )              

instance HasLinkedObject File where
   toLinkedObject object = linkedObject object

-- ------------------------------------------------------------------
-- Getting at the SimpleFile
-- ------------------------------------------------------------------

instance HasFilePath File where
   toFilePath file = toFilePath (simpleFile file)

-- ------------------------------------------------------------------
-- The global registry
-- ------------------------------------------------------------------

globalRegistry :: GlobalRegistry FileType
globalRegistry = IOExts.unsafePerformIO createGlobalRegistry
{-# NOINLINE globalRegistry #-}
         
-- ------------------------------------------------------------------
-- newEmptyFile
-- ------------------------------------------------------------------

-- Creating a new empty file with the given name
-- We use the inputAttributes method to get the attributes, and
-- return Nothing if the user cancels.
newEmptyFile :: FileType -> View -> LinkedObject -> IO (Maybe (Link File))
newEmptyFile fileType view parentLinkedObject =
   do
      -- Construct an extraFormItem for the name.
      extraFormItem <- 
         mkExtraFormItem(
            guardNothing "File name not specified"
               (newFormEntry "Name" Nothing))
      attributesOpt <- inputAttributes view (requiredAttributes fileType)
         (Just extraFormItem)
      case attributesOpt of
         Nothing -> return Nothing
         Just attributes ->
            do
               name <- readExtraFormItem extraFormItem
               simpleFile <- newSimpleFile view
               createLinkedObjectChild view parentLinkedObject name
                  (\ linkedObject ->
                     return (
                        File {
                           fileType = fileType,
                           attributes = attributes,
                           linkedObject = linkedObject,
                           simpleFile = simpleFile
                           }
                        )
                     )

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
      let
         fileType = FileType {
            fileTypeId = plainFileKey,
            fileTypeLabel = Just "Plain file",
            requiredAttributes = emptyAttributesType,
            displayParms = plainFileNodeTypeParms,
            canEdit = True
            }

      addToGlobalRegistry globalRegistry view plainFileKey fileType

      return fileType


getPlainFileType :: View -> IO FileType
getPlainFileType view =
   lookupInGlobalRegistry globalRegistry view plainFileKey

plainFileKey :: GlobalKey
plainFileKey =  oneOffKey "Files" ""

-- ------------------------------------------------------------------
-- creating a new file type
-- ------------------------------------------------------------------

createNewFileType :: View -> IO (Maybe FileType)
createNewFileType view =
   do
      let
         firstForm :: Form (String,(Bool,NodeTypes (Link File))) =
            titleForm //
            canEditForm //
            simpleNodeTypesForm

         titleForm0 :: Form String
         titleForm0 = newFormEntry "Title" "" 

         titleForm = guardForm (/= "") "Title must be non-empty" titleForm0

         canEditForm :: Form Bool
         canEditForm = newFormEntry "Editable" False

      typeData1Opt <- doForm "Node Type Appearance" firstForm
      case typeData1Opt of
         Nothing -> return Nothing
         Just (title,(canEdit,displayParms)) ->
            do
               requiredAttributesOpt <- getAttributesType 
               case requiredAttributesOpt of
                  Nothing -> return Nothing
                  Just requiredAttributes ->
                     do
                        fileTypeId <- newKey globalRegistry view
                        return (Just(FileType {
                           fileTypeId = fileTypeId,
                           fileTypeLabel = Just title,
                           requiredAttributes = requiredAttributes,
                           displayParms = displayParms,
                           canEdit = canEdit
                           }))

