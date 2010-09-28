-- |
-- Description: Simple repository 'Files.File's
--
-- Files are simple files together with attributes
--
-- This file is very similar to Folders.hs
module Types.Files(
   registerFiles, -- :: IO ()
      -- to be done at initialisation
   File,
      -- instance of HasAttributes, HasFilePath
   FileType(requiredAttributes),
   newEmptyFile,
   plainFileType, -- :: FileType

   writeToFile,
      -- :: View -> Link File -> GlobalKey -> ICStringLen
      --  -> IO ()
      -- write to a file link, which may be empty.  If it is empty,
      -- create it using the type specified by the
      -- given key.
      -- We assume that the type exists in the view, that there are no errors
      -- creating the LinkedObject, and that the file type has no attributes
   ) where

import Maybe

import System.IO.Unsafe

import Util.Dynamics
import Util.Computation
import Util.AtomString(fromString,toString)
import Util.ICStringLen

import HTk.Toolkit.SimpleForm

import Posixutil.CopyFile

import Graphs.GraphConfigure
import Graphs.Graph(NodeType)

import Types.View
import Types.CodedValue
import Types.Link
import Types.BasicObjects
import Types.AttributesType
import Types.ObjectTypes
import Types.DisplayParms
import Types.GlobalRegistry
import Types.CallEditor
import Types.LocalMenus
import Types.LinkManager
import Types.MergeTypes
import Types.MergePrune
import Types.ManagePermissions


-- ------------------------------------------------------------------
-- FileType and its instance of HasCodedValue and HasAttributesType
-- ------------------------------------------------------------------

data FileType = FileType {
   fileTypeId :: GlobalKey,
   fileTypeLabel :: Maybe String,
   requiredAttributes :: AttributesType,
   displayParms :: NodeTypes (Link File),
   canEdit :: Bool
   } deriving (Typeable)

instance HasBinary FileType CodingMonad where
   writeBin = mapWrite
      (\ (FileType {fileTypeId = fileTypeId,fileTypeLabel = fileTypeLabel,
            requiredAttributes = requiredAttributes,
            displayParms = displayParms,canEdit = canEdit})
         -> (fileTypeId,fileTypeLabel,requiredAttributes,displayParms,
            canEdit)
         )
   readBin = mapRead
      (\ (fileTypeId,fileTypeLabel,requiredAttributes,displayParms,
               canEdit) ->
         FileType {fileTypeId = fileTypeId,
            fileTypeLabel = fileTypeLabel,
            requiredAttributes = requiredAttributes,
            displayParms = displayParms,
            canEdit = canEdit}
         )

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
   } deriving (Typeable)

instance HasAttributes File where
   readPrimAttributes object = attributes object

instance HasBinary File CodingMonad where
   writeBin = mapWrite
      (\ (File {fileType = fileType,attributes = attributes,
             linkedObject = linkedObject,simpleFile = simpleFile}) ->
         (fileTypeId fileType,attributes,linkedObject,simpleFile)
         )
   readBin = mapReadViewIO
      (\ view (fileTypeId,attributes,linkedObject,simpleFile) ->
         do
            fileType <- getObjectTypeByKey view fileTypeId
            return (File {fileType = fileType,attributes = attributes,
             linkedObject = linkedObject,simpleFile = simpleFile})
         )

-- ------------------------------------------------------------------
-- Merging
-- ------------------------------------------------------------------

instance HasMerging File where
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
                     linkedObject0 = linkedObject file

                  linkedObject1WE <- attemptLinkedObjectMerge
                     linkReAssigner newView newLink [(view,linkedObject file)]
                  mapWithErrorIO
                     (\ linkedObject1 ->
                        do
                           isSame
                              <- linkedObjectsSame linkedObject0 linkedObject1
                           if isSame
                              then
                                 cloneLink view fileLink newView newLink
                              else
                                 do
                                    let
                                       simpleFile1 = attemptMergeSimpleFile
                                          linkReAssigner newView
                                             (simpleFile file)

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

-- ------------------------------------------------------------------
-- The instance of ObjectType
-- ------------------------------------------------------------------

-- | Node type
theNodeType :: NodeType
theNodeType = fromString ""

instance ObjectType FileType File where
   objectTypeTypeIdPrim _ = "Files"
   objectTypeIdPrim objectType = fileTypeId objectType
   objectTypeGlobalRegistry _ = globalRegistry
   getObjectTypePrim file = fileType file
   toLinkedObjectOpt file = Just (linkedObject file)
   extraObjectTypes = return [plainFileType]

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
                                 (\ link -> editObjectAttributes view link),
                              Button "Delete" (deleteObject view)
                              ]
                              ++ permissionsMenu view

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
                              fontStyleSource view $$$
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
                     specialNodeActions = emptySpecialNodeActions
                     })
               Nothing -> Nothing
         )

instance HasLinkedObject File where
   toLinkedObject object = linkedObject object

-- ------------------------------------------------------------------
-- Getting at the SimpleFile
-- ------------------------------------------------------------------

instance HasFilePath File where
   toFilePath file = toFilePath (simpleFile file)

instance HasContents File where
   getAsICSL view file = getAsICSL view (simpleFile file)

-- | write to a file link, which may be empty.  If it is empty,
-- create it using the type specified by the
-- given key.
-- We assume that the type exists in the view, that there are no errors
-- creating the LinkedObject, and that the file type has no attributes
writeToFile :: View -> Link File -> GlobalKey -> ICStringLen
   -> IO ()
writeToFile view fileLink typeKey icsl =
   do
      isEmpty <- isEmptyLink view fileLink
      simpleFile <- if isEmpty
         then
            do
               simpleFile <- newSimpleFile view
               linkedObjectWE <- newLinkedObject view (WrappedLink fileLink)
                  Nothing
               let
                  linkedObject = coerceWithError linkedObjectWE
               attributes <- newEmptyAttributes view
               fileType <- getObjectTypeByKey view typeKey
               let
                  file =
                     File {
                        fileType = fileType,
                        attributes = attributes,
                        linkedObject = linkedObject,
                        simpleFile = simpleFile
                        }
               writeLink view fileLink file
               return simpleFile
         else
            do
               file <- readLink view fileLink
               return (simpleFile file)

      copyICStringLenToFile icsl (toFilePath simpleFile)
      dirtyLink view fileLink




-- ------------------------------------------------------------------
-- The global registry
-- ------------------------------------------------------------------

globalRegistry :: GlobalRegistry FileType
globalRegistry = unsafePerformIO createGlobalRegistry
{-# NOINLINE globalRegistry #-}

-- ------------------------------------------------------------------
-- newEmptyFile
-- ------------------------------------------------------------------

-- Creating a new empty file with the given name
-- We use the inputAttributes method to get the attributes, and
-- return False if unsuccessful.
newEmptyFile :: FileType -> View -> LinkedObject -> IO Bool
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
         Nothing -> return False
         Just attributes ->
            do
               name <- readExtraFormItem extraFormItem
               simpleFile <- newSimpleFile view
               linkOpt <- createLinkedObjectChild view parentLinkedObject name
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
               return (isJust linkOpt)

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

plainFileType :: FileType
plainFileType = FileType {
   fileTypeId = plainFileKey,
   fileTypeLabel = Just "Plain file",
   requiredAttributes = emptyAttributesType,
   displayParms = plainFileNodeTypeParms,
   canEdit = True
   }

plainFileNodeTypeParms :: NodeTypes value
plainFileNodeTypeParms =
   addNodeRule
      AllDisplays
      (SimpleNodeAttributes { shape = Nothing,
         nodeColor = Just (Color "green")})
      emptyNodeTypes

plainFileKey :: GlobalKey
plainFileKey =  oneOffKey "Files" ""


-- ------------------------------------------------------------------
-- creating a new file type
-- (disabled for now).
-- ------------------------------------------------------------------

{-

createNewFileType :: View -> IO (Maybe FileType)
createNewFileType view =
   do
      let
         firstForm :: Form (String,(Bool,NodeTypes (Link File)))
         firstForm =
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

-}
