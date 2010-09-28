-- |
-- Description: Simple repository 'Folders.Folder's.
--
-- In this module we implement the folder type, and a display type for
-- displaying the directory structure.
module Types.Folders(
   registerFolders, -- :: IO ()
      -- to be done at initialisation
   Folder,
   FolderType(requiredAttributes),
   getTopFolder,
   getTopLinkedObject,
   getImportsState,
   getInFolder,
   mkFolderInsertion,
   lookupFileName,
   newEmptyFolder,
   newEmptyFolder1, -- :: FolderType -> View -> LinkedObject -> String -> IO Bool

   FolderDisplayType(FolderDisplayType),
   createWithLinkedObject,
   createWithLinkedObjectIO,
   createWithLinkedObjectSplitIO,

   registerExtraFolderType, -- :: FolderType -> IO ()
   mkFolderType0, -- :: GlobalKey -> NodeTypes (Link Folder) -> FolderType

   describeLinkedObject, -- :: View -> LinkedObject -> IO String
   plainFolderType, -- :: FolderType

   writeEmptyFolder,
      -- :: View -> Link Folder -> GlobalKey -> IO ()

   toArcEndsGeneral,
      -- :: (Ord a,HasKey a key)
      -- -> Blocker a -> BlockID -> (a -> ArcData WrappedLink ArcType)
      -- -> IO ArcEnds


   registerAsMoveable,
      -- :: (HasCodedValue object,HasLinkedObject object)
      -- => (object :: object) -> IO ()
      -- Register that objects of this type may be moved into folders.
      -- The value of the argument is ignored.
   ) where

import Maybe

import System.IO.Unsafe
import Data.IORef

import Util.Dynamics
import Util.Computation
import Util.Thread(forkIODebug)
import Util.Sources
import Util.Broadcaster
import Util.VariableSet
import Util.VariableSetBlocker
import Util.VariableList
import Util.AtomString(fromString,toString)
import Util.Delayer(toDelayer,delay)
import Util.ExtendedPrelude
import Util.Store
import Util.Messages

import HTk.Toolkit.SimpleForm

import Graphs.GraphDisp
import Graphs.GraphConfigure
import Graphs.Graph(ArcType,NodeType)

import Imports.FolderStructure(getName)
import Imports.Imports

import Types.ViewType(getViewTitleSource,importsState,delayer)
import Types.VersionDB(displayErrors)
import Types.View
import Types.CodedValue
import Types.Link
import Types.BasicObjects
import Types.AttributesType
import Types.DisplayTypes
import Types.ObjectTypes
import Types.DisplayParms
import Types.GlobalRegistry
import Types.CreateObjectMenu
import Types.DisplayView
import Types.GetAttributesType
import Types.GlobalMenus
import Imports.EntityNames
import Types.LinkDrawer (toArcData,ArcData)
import Types.LinkManager
import Types.MergeTypes
import Types.MergePrune
import Types.ManagePermissions

------------------------------------------------
-- The Display Type
-- ------------------------------------------------------------------

data FolderDisplayType = FolderDisplayType deriving (Typeable)
   -- This just holds the key to the folder.

instance Monad m => HasBinary FolderDisplayType m where
   writeBin = mapWrite (\ FolderDisplayType -> ())
   readBin = mapRead (\ () -> FolderDisplayType)

instance DisplayType FolderDisplayType where
   displayTypeTypeIdPrim _  = "Folders"

   graphParmsPrim displaySort view FolderDisplayType =
      do
         globalMenu <- newDefaultMenu displaySort view
         let
            graphTitleSource =
               fmap
                  (\ versionTitle ->
                     GraphTitle (versionTitle++": structure graph")
                     )
                  (getViewTitleSource view)
         return (
            (toDelayer view) $$
            globalMenu $$
            ActionWrapper (\ act ->
               do
                  forkIODebug (displayErrors act)
                  done
               ) $$
            AllowDragging True $$
            LeftRight $$
            graphTitleSource $$
            emptyGraphParms
            )

   displayTypeGlobalRegistry _ = displayTypeRegistry

   displayTypeIdPrim FolderDisplayType = folderDisplayKey

   openDisplayMenuItemPrim displaySort displayType =
      Just ("New Folders Display",openGeneralDisplay displaySort displayType)

displayTypeRegistry :: GlobalRegistry FolderDisplayType
displayTypeRegistry = unsafePerformIO createGlobalRegistry
{-# NOINLINE displayTypeRegistry #-}

-- ------------------------------------------------------------------
-- FolderType and its instance of HasCodedValue and HasAttributesType
-- ------------------------------------------------------------------

data FolderType = FolderType {
   folderTypeId :: GlobalKey,
   allowAddFiles :: Bool,
      -- If this is set, allow new objects to be created within a folder of
      -- this type by dragging from it using addFileGesture.
      -- Also allows folders to be dragged into other files and for other
      -- folders to be dragged into this one.  Maybe we need three bools
      -- here rather than one but for now I can't be bothered.
   folderTypeLabel :: Maybe String,
      -- If set, allow folders of this type to be added with addFileGesture;
      -- the String will be the menu label the use selects.
   requiredAttributes :: AttributesType,
   displayParms :: NodeTypes (Link Folder),
   topFolderLinkOpt :: Maybe (Link Folder)
   } deriving (Typeable)

instance HasBinary FolderType CodingMonad where
   writeBin = mapWrite
      (\ (FolderType {folderTypeId = folderTypeId,
            allowAddFiles = allowAddFiles,
            folderTypeLabel = folderTypeLabel,
            requiredAttributes = requiredAttributes,
            displayParms = displayParms,topFolderLinkOpt = topFolderLinkOpt})
         ->
         (folderTypeId,allowAddFiles,folderTypeLabel,
            requiredAttributes,displayParms,topFolderLinkOpt)
         )
   readBin = mapRead
      (\ (folderTypeId,allowAddFiles,folderTypeLabel,
            requiredAttributes,displayParms,topFolderLinkOpt)
         ->
         (FolderType {folderTypeId = folderTypeId,
            allowAddFiles = allowAddFiles,
            folderTypeLabel = folderTypeLabel,
            requiredAttributes = requiredAttributes,
            displayParms = displayParms,topFolderLinkOpt = topFolderLinkOpt})
         )

instance HasAttributesType FolderType where
   toAttributesType folderType = requiredAttributes folderType


-- ------------------------------------------------------------------
-- Folder and its instance of HasAttributes and HasCodedValue
-- ------------------------------------------------------------------


data Folder = Folder {
   folderType :: FolderType,
   attributes :: Attributes,
   linkedObject :: LinkedObject,
   hideFolderArcs :: SimpleBroadcaster (Maybe NodeArcsHidden),
   openContents :: Blocker WrappedLink
      -- Contains blocker for contents of linkedObject.
   } deriving (Typeable)

instance HasAttributes Folder where
   readPrimAttributes object = attributes object

instance HasBinary Folder CodingMonad where
   writeBin = mapWrite
      (\ (Folder {folderType = folderType,attributes = attributes,
             linkedObject = linkedObject}) ->
         (folderTypeId folderType,attributes,linkedObject)
         )
   readBin = mapReadViewIO
      (\ view (folderTypeId,attributes,linkedObject) ->
         createFolder view folderTypeId attributes linkedObject
         )

-- | Thus function is also used during merging.
createFolder :: View -> GlobalKey -> Attributes -> LinkedObject -> IO Folder
createFolder view folderTypeId attributes linkedObject =
   do
      folderType <- getObjectTypeByKey view folderTypeId
      hideFolderArcs <- mkArcsHiddenSource
      openContents <- newOpenContents view linkedObject
      return (Folder {folderType = folderType,attributes = attributes,
         linkedObject = linkedObject,openContents = openContents,
         hideFolderArcs = hideFolderArcs})

-- ------------------------------------------------------------------
-- Merging
-- ------------------------------------------------------------------

instance HasMerging Folder where

   getMergeLinks = getLinkedObjectMergeLinks

   attemptMerge linkReAssigner newView newLink vlos =
      addFallOutWE (\ break ->
         do
            (vlos @ ((vlo1 @ (view1,link1,folder1))  : vlosRest))
               <- mergePrune vlos

            -- (1) check that the folder types match and compute the new
            -- folder type.
            let
               folderType1 = folderType folder1
               folderType1Id = folderTypeId folderType1

            mapM_
               (\ (_,_,folder) ->
                  if folderType1Id
                        /= folderTypeId (folderType folder)
                     then
                        do
                           folderTitle <- nodeTitleIOPrim folder
                           break ("Type mismatch attempting to merge folder "
                              ++ folderTitle)
                     else
                        done
                  )
               vlosRest

            let
               newFolderTypeId = folderType1Id

            -- (2) we just ignore all but the first attributes.
            let
               newAttributes = attributes folder1

            -- (3) now for the interesting bit ...
            newLinkedObjectWE <- attemptLinkedObjectMerge
               linkReAssigner newView newLink
                  (map
                     (\ (view,link,folder) -> (view,toLinkedObject folder))
                     vlos
                     )

            newLinkedObject <- coerceWithErrorOrBreakIO break newLinkedObjectWE

            isSame
               <- linkedObjectsSame (toLinkedObject folder1) newLinkedObject

            if isSame
               then
                  cloneLink view1 link1 newView newLink
               else
                  do
                     -- (4) create ...
                     folder <- createFolder newView newFolderTypeId
                        newAttributes newLinkedObject

                     setLink newView folder newLink
                     done
      )


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

   extraObjectTypes = getExtraFolderTypes

   getObjectTypePrim folder = folderType folder
   nodeTitleSourcePrim folder =
      do
         entityNameOpt <- getLinkedObjectTitleOpt (linkedObject folder)
         return (case entityNameOpt of
            Nothing -> "Root"
            Just entityName -> toString entityName
            )

   createObjectTypeMenuItemNoInsert =
      Just ("Folder type",createNewFolderType)

   createObjectMenuItemPrim folderType =
      fmap
         (\ label -> (label,newEmptyFolder folderType))
         (folderTypeLabel folderType)

   toLinkedObjectOpt folder = Just (linkedObject folder)

   getNodeDisplayData view wrappedDisplayType folderType
         displayedViewAction =
      do
         blockID <- newBlockID

         let
            nodeTypeParmsOpt = getNodeTypeParms wrappedDisplayType
               (displayParms folderType)

            openAction link =
               do
                  folder <- readLink view link
                  bracketForImportErrors view
                     (openBlocker (openContents folder) blockID)


            closeAction link =
               do
                  folder <- readLink view link
                  delay view (closeBlocker (openContents folder) blockID)

         return (
            case nodeTypeParmsOpt of
               Just nodeTypeParms ->
                  Just (NodeDisplayData {
                     topLinks = case topFolderLinkOpt folderType of
                        Nothing -> []
                        Just link -> [link],
                     arcTypes = [(theArcType,emptyArcTypeParms)],
                     nodeTypes =
                        let
                           editOptions1 = [
                              Button "Open Folder" (\ link
                                 -> openAction link
                                 ),
                              Button "Close Folder" (\ link
                                 -> closeAction link
                                 ),
                              Button "Hide Links" (\ link ->
                                 hideAction link True
                                 ),
                              Button "Reveal Links" (\ link ->
                                 hideAction link False
                                 )
                              ]
                              ++ permissionsMenu view
                              ++ if isEmptyAttributesType
                                    (requiredAttributes folderType)
                                 then
                                    []
                                 else [
                                    Button "Edit Attributes" (\ link
                                       -> editObjectAttributes view link)
                                    ]
                           menu = LocalMenu (Menu (Just "Folder options")
                              editOptions1)
                        in
                          [(theNodeType,
                           menu $$$
                           (valueTitleSource view) $$$
                           (fontStyleSource view) $$$
                           (DoubleClickAction openAction) $$$
                           (if allowAddFiles folderType
                              then
                                 Just (addFileGesture view)
                              else
                                 Nothing
                              ) $$$?
                           (if allowAddFiles folderType
                              then
                                 Just (moveFileGesture view)
                              else
                                 Nothing
                              ) $$$?
                           nodeTypeParms
                           )],
                     getNodeType = const theNodeType,
                     getNodeLinks = (\ link ->
                        do
                           folder <- readLink view link
                           toArcEnds (openContents folder) blockID
                        ),
                     specialNodeActions =
                        (\ object ->
                           fmap
                              (\ (arcsHidden :: Maybe NodeArcsHidden) ->
                                 (\ graph node ->
                                    modify arcsHidden graph node
                                 ))
                              (toSimpleSource (hideFolderArcs object))
                           )
                     })
               Nothing -> Nothing
            )
      where
         hideAction :: Link Folder -> Bool -> IO ()
         hideAction link bool =
            do
               folder <- readLink view link
               broadcast (hideFolderArcs folder) (Just (NodeArcsHidden bool))


   -- Merging
   fixedLinksPrim =
      (\ view folderType -> return (
         case topFolderLinkOpt folderType of
            Nothing -> []
            Just link -> [link]
            )
         )

-- ------------------------------------------------------------------
-- Extra option so that folders can add files.
-- ------------------------------------------------------------------

addFileGesture :: View -> NodeGesture (Link Folder)
addFileGesture view =
   let
      addFile folderLink =
         do
            objectCreated <- createObjectMenu view folderLink
            if objectCreated
               then
                  done
               else
                  alertMess "Object creation cancelled"
   in
      NodeGesture addFile

instance HasLinkedObject Folder where
   toLinkedObject folder = linkedObject folder

-- ------------------------------------------------------------------
-- Option allowing Folders to be moved into other Folders
-- ------------------------------------------------------------------

moveFileGesture :: View -> NodeDragAndDrop (Link Folder)
moveFileGesture view =
   let
      moveFile dyn folderLink =
         do
            moveableList <- readIORef moveableListRef
            let
               scanMoveableList [] = return Nothing
               scanMoveableList (getLinkedObject : moveableList1) =
                  do
                     linkedObjectOpt <- getLinkedObject view dyn
                     case linkedObjectOpt of
                        Just _ -> return linkedObjectOpt
                        Nothing -> scanMoveableList moveableList1

            linkedObjectOpt <- scanMoveableList moveableList
            case linkedObjectOpt of
               Nothing -> errorMess "You cannot move this object"
               Just linkedObject ->
                  do
                     thisNameOpt <- readContents (
                        getLinkedObjectTitleOpt linkedObject)
                     case thisNameOpt of
                        Nothing -> errorMess "You cannot move the root object"
                        Just thisName ->
                           do
                              folder <- readLink view folderLink
                              let
                                 folderLinkedObject = toLinkedObject folder
                              resultWE <- moveObject linkedObject
                                 (Just (mkInsertion folderLinkedObject
                                    thisName))
                              case fromWithError resultWE of
                                 Left mess -> errorMess mess
                                 Right () -> done
   in
      NodeDragAndDrop moveFile


moveableListRef :: IORef [View -> Dyn -> IO (Maybe LinkedObject)]
moveableListRef = unsafePerformIO (newIORef [])
{-# NOINLINE moveableListRef #-}

registerAsMoveable ::
   (HasCodedValue object,HasLinkedObject object)
   => object -> IO ()
registerAsMoveable = registerAsMoveable1 (const True)

registerAsMoveable1 ::
   (HasCodedValue object,HasLinkedObject object)
   => (object -> Bool) -> object -> IO ()
registerAsMoveable1 objectIsMoveable (_ :: object) =
   do
      let
         getLinkedObject view linkDyn =
            case fromDynamic linkDyn of
               Nothing -> return Nothing
               Just (link :: Link object) ->
                  do
                     object <- readLink view link
                     return (if objectIsMoveable object
                        then
                           Just (toLinkedObject object)
                        else
                           Nothing
                        )

      atomicModifyIORef moveableListRef
         (\ moveableList -> (getLinkedObject : moveableList,()))

-- ------------------------------------------------------------------
-- The global registry and a permanently empty variable set
-- ------------------------------------------------------------------

globalRegistry :: GlobalRegistry FolderType
globalRegistry = unsafePerformIO createGlobalRegistry
{-# NOINLINE globalRegistry #-}

-- ------------------------------------------------------------------
-- Registering the folder type and display
-- ------------------------------------------------------------------

registerFolders :: IO ()
registerFolders =
   do
      registerObjectType (error "Unknown FolderType" :: FolderType)
      registerDisplayType
         (error "Unknown FolderDisplayType" :: FolderDisplayType)
      registerAsMoveable1 (allowAddFiles . folderType)
         (error "Unknown Folder" :: Folder)

-- ------------------------------------------------------------------
-- The list of extra folder types.
-- ------------------------------------------------------------------

getExtraFolderTypes :: IO [FolderType]
getExtraFolderTypes =
   atomicModifyIORef extraFolderTypesList
      (\ ftl0 ->
         let
            ftl1 = ftl0 {isUsed = True}
         in
            (ftl1,folderTypes ftl1)
         )

-- This MUST be done before any structure displays are opened.
registerExtraFolderType :: FolderType -> IO ()
registerExtraFolderType folderType =
   do
      ftl1 <- atomicModifyIORef extraFolderTypesList
         (\ ftl0 ->
            let
               ftl1 = ftl0 {folderTypes = folderType : folderTypes ftl0}
            in
               (ftl1,ftl1)
            )
      if isUsed ftl1
         then
            error ("Folders.registerExtraFolderType: attempt to use this for "
               ++ describeFolderType folderType
               ++ " when a structure display has already been opened.")
         else
            done

describeFolderType :: FolderType -> String
describeFolderType ft =
   fromMaybe
      (describeGlobalKey (folderTypeId ft))
      (folderTypeLabel ft)


data FolderTypeList = FolderTypeList {
   folderTypes :: [FolderType],
   isUsed :: Bool
      -- becomes True when extraObjectTypes is accessed.  Used to
      -- detect attempts to add extra folder types too late.
   }

extraFolderTypesList :: IORef FolderTypeList
extraFolderTypesList = unsafePerformIO mkExtraFolderTypesList
{-# NOINLINE extraFolderTypesList #-}

mkExtraFolderTypesList :: IO (IORef FolderTypeList)
mkExtraFolderTypesList =
   let
      folderTypeList = FolderTypeList {
         folderTypes = [plainFolderType],
         isUsed = False
         }
   in
      newIORef folderTypeList


-- ------------------------------------------------------------------
-- The plain folder type
-- ------------------------------------------------------------------

plainFolderType :: FolderType
plainFolderType = FolderType {
   folderTypeId = plainFolderKey,
   allowAddFiles = True,
   folderTypeLabel = Just "Plain Folder",
   requiredAttributes = emptyAttributesType,
   displayParms = plainFolderNodeTypeParms,
   topFolderLinkOpt = Just topLink
   }

plainFolderNodeTypeParms :: NodeTypes value
plainFolderNodeTypeParms =
   addNodeRule
      AllDisplays
      (SimpleNodeAttributes { shape = Just Triangle,
         nodeColor = Just (Color "green")})
      emptyNodeTypes

plainFolderKey :: GlobalKey
plainFolderKey = oneOffKey "Folders" ""

folderDisplayKey :: GlobalKey
folderDisplayKey = oneOffKey "Folders" "Display"

-- ------------------------------------------------------------------
-- Creating other FolderType's.
-- NB.  These must still be either (1) put in the GlobalRegistry of
-- views where they are used, or (2) registered with registerExtraFolderType.
-- ------------------------------------------------------------------

mkFolderType0 :: GlobalKey -> NodeTypes (Link Folder) -> FolderType
mkFolderType0 thisKey displayParms =
   FolderType {
      folderTypeId = thisKey,
      allowAddFiles = False,
      folderTypeLabel = Nothing,
      requiredAttributes = emptyAttributesType,
      displayParms = displayParms,
      topFolderLinkOpt = Nothing
      }


-- ------------------------------------------------------------------
-- Retrieving the top folder.
-- ------------------------------------------------------------------

-- | getTopFolder returns a link to the topFolder, creating it in the exceptional
-- circumstance that it doesn\'t already exist in the view.
getTopFolder :: View -> IO (Link Folder)
getTopFolder view =
   do
      versioned <- setOrGetTopLink view (
         do
            -- Create the topFolder.
            attributes <- newEmptyAttributes view
            linkedObjectWE <- newLinkedObject view (
               WrappedLink (topLink :: Link Folder)) Nothing
            linkedObject <- coerceWithErrorIO linkedObjectWE
            openContents <- newOpenContents view linkedObject
            hideFolderArcs <- mkArcsHiddenSource
            return (Folder {
               folderType = plainFolderType,
               attributes = attributes,
               linkedObject = linkedObject,
               openContents = openContents,
               hideFolderArcs = hideFolderArcs
               })
         )
      return (makeLink versioned)

-- ------------------------------------------------------------------
-- Getting the FolderStructure for a view.
-- ------------------------------------------------------------------

getImportsState :: View -> IO (ImportsState LinkedObject)
getImportsState view =
   takeStore
      (do
         folderLink <- getTopFolder view
         folder <- readLink view folderLink
         let
            folderStructure = toFolderStructure (linkedObject folder)
         newImportsState folderStructure (delayer view)
         )
      (importsState view)

describeLinkedObject :: View -> LinkedObject -> IO String
describeLinkedObject view linkedObject =
   do
      importsState <- getImportsState view
      fullName <- getName (folders importsState) linkedObject
      return (toString fullName)


-- ------------------------------------------------------------------
-- Indexing in a folder
-- ------------------------------------------------------------------

{-# DEPRECATED getInFolder,lookupFileName "Use LinkManager functions instead"
    #-}

-- | getInFolder returns the wrapped link indexed in the folder by the given
-- string, if it exists.
getInFolder :: View -> Link Folder -> String -> IO (Maybe WrappedLink)
getInFolder view link str =
   do
      folder <- readLink view link
      linkedObjectOpt <- lookupNameSimple (linkedObject folder) str
      return (fmap toWrappedLink linkedObjectOpt)

-- | lookupLink looks up a link by file name, given by components, with
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

-- | Create a new empty folder in the view and insert it in
-- the given parent.
--
-- We use the inputAttributes method to get the attributes, and
-- return False if the user cancels, or there was some other error.
newEmptyFolder :: FolderType -> View -> LinkedObject
   -> IO Bool
newEmptyFolder folderType view parentLinkedObject =
   do
      -- Construct an extraFormItem for the name.
      extraFormItem <-
         mkExtraFormItem (
            guardNothing "Folder name not specified"
               (newFormEntry "Name" Nothing))
      attributesOpt <- inputAttributes view (requiredAttributes folderType)
         (Just extraFormItem)
      case attributesOpt of
         Nothing -> return False
         Just attributes ->
            do
               name <- readExtraFormItem extraFormItem
               hideFolderArcs <- mkArcsHiddenSource
               linkOpt <- createLinkedObjectChild view parentLinkedObject name
                  (\ linkedObject ->
                     do
                        openContents <- newOpenContents view linkedObject
                        let
                           folder =
                              Folder {
                                 folderType = folderType,
                                 attributes = attributes,
                                 linkedObject = linkedObject,
                                 openContents = openContents,
                                 hideFolderArcs = hideFolderArcs
                                 }
                        return folder
                     )
               return (isJust linkOpt)


-- | If the FolderLink is empty, create a new empty folder with the given
-- key and with empty attributes.
writeEmptyFolder :: View -> Link Folder -> GlobalKey -> IO ()
writeEmptyFolder view folderLink typeKey =
   do
      isEmpty <- isEmptyLink view folderLink
      if isEmpty
         then
            do
               linkedObjectWE <- newLinkedObject view (WrappedLink folderLink)
                  Nothing
               let
                  linkedObject = coerceWithError linkedObjectWE
               attributes <- newEmptyAttributes view
               folderType <- getObjectTypeByKey view typeKey
               openContents <- newOpenContents view linkedObject
               hideFolderArcs <- mkArcsHiddenSource
               let
                  folder =
                     Folder {
                        folderType = folderType,
                        attributes = attributes,
                        linkedObject = linkedObject,
                        openContents = openContents,
                        hideFolderArcs = hideFolderArcs
                        }
               writeLink view folderLink folder
         else
            done


newEmptyFolder1 :: FolderType -> View -> LinkedObject -> String
   -> IO (Maybe LinkedObject)
newEmptyFolder1 folderType view parentLinkedObject name =
    do
       hideFolderArcs <- mkArcsHiddenSource
       emptyAttributes <- newEmptyAttributes view
       linkOpt <- createLinkedObjectChild view parentLinkedObject (EntityName name)
          (\ linkedObject ->
             do
                openContents <- newOpenContents view linkedObject
                let
                   folder =
                      Folder {
                         folderType = folderType,
                         attributes = emptyAttributes,
                         linkedObject = linkedObject,
                         openContents = openContents,
                         hideFolderArcs = hideFolderArcs
                         }
                return folder
             )
       case linkOpt of
         Nothing -> return(Nothing)
         Just folderLink ->
           do folder <- readLink view folderLink
              return (Just(toLinkedObject folder))


-- | Making an insertion into a folder
mkFolderInsertion :: Folder -> EntityName -> Insertion
mkFolderInsertion folder = mkInsertion (linkedObject folder)

-- ------------------------------------------------------------------
-- creating a new folder type
-- ------------------------------------------------------------------

createNewFolderType :: View -> IO (Maybe FolderType)
createNewFolderType view =
   do
      let
         firstForm :: Form (String,NodeTypes (Link Folder))
         firstForm =
            titleForm //
            simpleNodeTypesForm

         titleForm0 :: Form String
         titleForm0 = newFormEntry "Title" ""

         titleForm = guardForm (/= "") "Title must be non-empty" titleForm0
      typeData1Opt <- doForm "Node Type Appearance" firstForm
      case typeData1Opt of
         Nothing -> return Nothing
         Just (title,displayParms) ->
            do
               requiredAttributesOpt <- getAttributesType
               case requiredAttributesOpt of
                  Nothing -> return Nothing
                  Just requiredAttributes ->
                     do
                        folderTypeId <- newKey globalRegistry view
                        return (Just(FolderType {
                           folderTypeId = folderTypeId,
                           allowAddFiles = True,
                           folderTypeLabel = Just title,
                           requiredAttributes = requiredAttributes,
                           displayParms = displayParms,
                           topFolderLinkOpt = Nothing
                           }))


-- ------------------------------------------------------------------
-- Creating the folder actions
-- ------------------------------------------------------------------

mkArcsHiddenSource :: IO (SimpleBroadcaster (Maybe NodeArcsHidden))
mkArcsHiddenSource = newSimpleBroadcaster Nothing

-- ------------------------------------------------------------------
-- Creating new objects in a folder.
-- ------------------------------------------------------------------

--
-- General purpose object creation functions, which also does helpful
-- things like displaying the error message if things go wrong.
createWithLinkedObject :: ObjectType objectType object
   => View -> Link Folder -> EntityName -> (LinkedObject -> object)
   -> IO (Maybe (Link object))
createWithLinkedObject view parentLink name toObject =
   createWithLinkedObjectIO view parentLink name (return . toObject)

createWithLinkedObjectIO :: ObjectType objectType object
   => View -> Link Folder -> EntityName -> (LinkedObject -> IO object)
   -> IO (Maybe (Link object))
createWithLinkedObjectIO view parentLink name getObject =
   do
      parent <- readLink view parentLink
      let
         insertion = mkInsertion (linkedObject parent) name
      act <- createViewObject view parentLink (\ link ->
         do
            linkedObjectWE <- newLinkedObject
               view (WrappedLink link) (Just insertion)
            case fromWithError linkedObjectWE of
               Right linkedObject ->
                  do
                     object <- getObject linkedObject
                     return (Just object,return (Just link))
               Left mess ->
                  return (Nothing,
                     do
                        errorMess mess
                        return Nothing
                     )
         )
      act

-- | This function is like createWithLinkedObjectIO, but does not actually
-- insert the new object into the folder, instead inserting it nowhere.
-- The action it returns DOES insert the object in the folder.
createWithLinkedObjectSplitIO :: ObjectType objectType object
   => View -> Link Folder -> EntityName -> (LinkedObject -> IO object)
   -> IO (Maybe (Link object,IO (WithError ())))
createWithLinkedObjectSplitIO view parentLink name getObject =
   do
      let
         insertFolderAct linkedObject1 =
            do
               parent <- readLink view parentLink
               let
                  insertion = mkInsertion (linkedObject parent) name
               moveObject linkedObject1 (Just insertion)

      act <- createViewObject view parentLink (\ link ->
         do
            linkedObjectWE <- newLinkedObject
               view (WrappedLink link) Nothing
            case fromWithError linkedObjectWE of
               Right linkedObject ->
                  do
                     object <- getObject linkedObject
                     return (Just object,
                        return (Just (link,insertFolderAct linkedObject)))
               Left mess ->
                  return (Nothing,
                     do
                        errorMess mess
                        return Nothing
                     )
         )
      act


-- ------------------------------------------------------------------
-- Initialising the folder's blocker.
-- ------------------------------------------------------------------

toArcEnds :: Blocker WrappedLink -> BlockID -> IO ArcEnds
toArcEnds blocker blockID =
   toArcEndsGeneral blocker blockID
      (\ wrappedLink -> toArcData wrappedLink theArcType True)

toArcEndsGeneral
   :: (Ord a,HasKey a key)
   => Blocker a -> BlockID -> (a -> ArcData WrappedLink ArcType) -> IO ArcEnds
toArcEndsGeneral (blocker :: Blocker a) blockId mkArcData =
   do
      (setSource1 :: VariableSetSource a)
         <- blockVariableSet blocker blockId

      let
         variableSet1 :: VariableList a
         variableSet1 = newVariableListFromSet setSource1

         variableSet2 :: VariableList (ArcData WrappedLink ArcType)
         variableSet2 = fmap mkArcData variableSet1

      return variableSet2

newOpenContents :: View -> LinkedObject -> IO (Blocker WrappedLink)
newOpenContents view linkedObject
   = newBlockerWithPreAction (objectContents linkedObject)
      (wrapPreFetchLinks view)

-- ------------------------------------------------------------------
-- The top linked object.  (Assuming the topLink is a folder, as indeed
-- it will be.)
-- ------------------------------------------------------------------

getTopLinkedObject :: View -> IO LinkedObject
getTopLinkedObject view =
   do
      folderLink <- getTopFolder view
      folder <- readLink view folderLink
      return (toLinkedObject folder)

