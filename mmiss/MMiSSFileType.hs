-- | This module describes the types required for the MMiSS file type. 
module MMiSSFileType(
   importMMiSSFile,
      -- :: HasLinkedObject object 
      -- => View -> object -> FilePath -> String -> String -> MMiSSVariantSpec 
      -- -> IO (WithError (Link MMiSSFile))
      --
      -- Import an object into the given folder, creating a new MMiSSFile.
      -- The object's location is given by containing directory,
      -- name, and extension.

   exportMMiSSFile,
      -- :: View -> FilePath -> Link MMiSSFile -> MMiSSVariantSearch
      -- -> IO (WithError ())
      -- Write an MMiSSFile to a directory.

   findMMiSSFilesInRepository, 
     -- :: HasLinkedObject folder 
     -- => folder -> EntityFullName 
     --    -> IO [(Link MMiSSFile,EntityFullName,String)]
     -- Find matching files inside a folder.

   findMMiSSFilesInDirectory,
      -- :: FilePath -> EntityFullName -> IO [(EntityFullName,String)]
      -- Find matching files inside a directory.

   -- MMiSSFile/MMiSSFileType are the new MMiSS object type.
   MMiSSFile,
   MMiSSFileType,

   constructKey,
      -- How to construct a GlobalKey for a given extension.

   fileTypeExists, -- :: String -> Bool
      -- Returns True if we know about the given file type.
) where

import IO
import Directory
import Monad
import Maybe

import System.IO.Unsafe
import Data.FiniteMap

import WBFiles
import ICStringLen
import BinaryAll
import Dynamics
import Computation
import ExtendedPrelude
import AtomString
import TempFile
import FileNames
import CommandStringSub
import Sources
import Messages
import Thread

import WithDir

import CopyFile

import MenuType
import DialogWin

import GraphConfigure hiding (Menu)
import GraphDisp(emptyNodeTypeParms)
import qualified GraphConfigure

import EntityNames

import ViewType
import Link
import DisplayTypes(WrappedDisplayType)
import ObjectTypes
import LinkManager
import CodedValue
import MergeTypes
import MergePrune
import GlobalRegistry
import DisplayView(DisplayedView)
import DisplayParms(fontStyleSource)

import Text.XML.HaXml.Xml2Haskell
import XmlExtras

import MMiSSVariantObject
import MMiSSVariant
import MMiSSUpdateVariantObject
import MMiSSFiles
import MMiSSRunCommand
import MMiSSBundle hiding (contents)
import qualified MMiSSBundle
import MMiSSBundleNodeWriteClass
import MMiSSBundleSimpleUtils
import MMiSSImportExportErrors

-- ----------------------------------------------------------------------
-- Datatypes
-- ----------------------------------------------------------------------

data MMiSSFileType = MMiSSFileType {
   fileType :: FileType, -- ^ The type auto-generated by DtdToHaskell.
   typeId :: GlobalKey
   } deriving (Typeable)

data MMiSSFile = MMiSSFile {
   mmissFileType :: MMiSSFileType,
   title :: String, -- ^ name of the file, without directory part or extension.
   linkedObject :: LinkedObject,
   contents 
      :: VariantObject (Link MMiSSFileVersion) (Link MMiSSFileVersion)
   } deriving (Typeable)

newtype MMiSSFileVersion = MMiSSFileVersion {
   text :: ICStringLen
   } deriving (Typeable,Eq)

-- | the result of reading the Files.xml file.
data MMiSSFilesState = MMiSSFilesState {
   fileTypes :: FiniteMap String MMiSSFileType,
   menus :: FiniteMap String Menu
   }

-- ----------------------------------------------------------------------
-- Importing new MMiSSFiles
-- ----------------------------------------------------------------------

-- | Import an object into the given folder, creating a new MMiSSFile.
-- The object's location is given by containing directory,
-- name, and extension.
importMMiSSFile :: HasLinkedObject object 
   => View -> object -> FilePath -> String -> String -> MMiSSVariantSpec 
   -> IO (WithError (Link MMiSSFile))
importMMiSSFile view folder dirPath0 name0 ext variantSpec =
   addFallOutWE (\ break ->
      do
         let
            dirPath1 = trimDir dirPath0

            name1 = unsplitExtension name0 ext

            fullName = dirPath1 `combineNames` name1

         fileType0 <- case lookupFM (fileTypes mmissFilesState) ext of
            Nothing -> break 
               ("File " ++ fullName ++ " cannot be imported, because the "
                  ++ "extension " ++ ext ++ " is not known.")
            Just fileType0 -> return fileType0

         icslWE <- copyFileToICStringLenCheck fullName
         icsl <- coerceWithErrorOrBreakIO break icslWE
         let
            fileVersion = MMiSSFileVersion {text = icsl}

         fileVersionLink <- createLink view fileVersion
         let
            -- Function which deletes the fileVersionLink before aborting.
            break2 :: String -> IO a
            break2 mess =
               do
                  deleteLink view fileVersionLink
                  break mess

         let
            -- We create an EntityName containing specialChar, since
            -- "." is not permitted.  NB - any attempt to display
            -- this entityName will mean the EntityNames module
            -- throws an error.
            entityNameWE :: WithError EntityName
            entityNameWE = unsplitSpecialWE name0 ext

         entityName <- coerceWithErrorOrBreakIO break entityNameWE

         -- get variant object.  afterInsertion is an action to be done
         -- after fileVersionLink has been inserted in the variantObject.
         (contents0,fileLink :: Link MMiSSFile,
               afterInsertion :: IO (WithError ())) <-
            do
               let
                  folderLinkedObject = toLinkedObject folder

               linkedObjectOpt <- lookupNameInFolder folderLinkedObject
                  entityName
               case linkedObjectOpt of
                  Just linkedObject -> 
                     do
                        let
                           wrappedLink = toWrappedLink linkedObject
                        (fileLink :: Link MMiSSFile) 
                              <- case unpackWrappedLink wrappedLink of
                           Nothing -> break2 ("Cannot insert file because "
                              ++ name1 ++ " is already in folder, but isn't an "
                              ++ "MMiSS file")
                           Just fileLink -> return fileLink
                        file <- readLink view fileLink
                        return (contents file,fileLink,return (hasValue ()))
                  Nothing ->
                     do
                        contents0 <- newEmptyVariantObject1 converter
                        creationResult <- createLinkedObjectChildSplit
                           view folderLinkedObject entityName         
                              (\ linkedObject0 ->
                                 return (MMiSSFile {
                                    mmissFileType = fileType0,
                                    title = name0,
                                    linkedObject = linkedObject0,
                                    contents = contents0
                                    })
                                 )
                        (fileLink,afterInsertion) <- case creationResult of
                           Just result -> return result
                           Nothing -> break2 ("Unable to create linked object "
                              ++ "for " ++ name0)
                        return (contents0,fileLink,afterInsertion)
         writeVariantObjectAndPoint contents0 variantSpec fileVersionLink
         afterInsertionResultWE <- afterInsertion
         case fromWithError afterInsertionResultWE of
            Left mess -> break2 mess
            Right () -> return fileLink
      )

-- ----------------------------------------------------------------------
-- Exporting MMiSSFiles
-- ----------------------------------------------------------------------

-- | Write an MMiSSFile to a directory.
exportMMiSSFile :: View -> FilePath -> Link MMiSSFile -> MMiSSVariantSearch
   -> IO (WithError ())
exportMMiSSFile view dirPath fileLink variantSearch =
   do
      mmissFile <- readLink view fileLink
      let
         title0 = title mmissFile
         tag = typeTag (mmissFileType mmissFile)

         fileName = unsplitExtension title0 tag
         fullName = combineNames (trimDir dirPath) fileName

      icslOpt <- lookupMMiSSFileVariantWithSpec view mmissFile variantSearch
      case icslOpt of
         Nothing -> return (hasError ("No version of " ++ fileName ++
            " matching " ++ show variantSearch ++ " found"))
         Just (icsl,variantSpec) ->
            do
               copyICStringLenToFile icsl fullName
               return (hasValue ())               


lookupMMiSSFileVariantWithSpec :: View -> MMiSSFile -> MMiSSVariantSearch ->
   IO (Maybe (ICStringLen,MMiSSVariantSpec))
lookupMMiSSFileVariantWithSpec view mmissFile variantSearch =
   do

      fileVersionLinkOpt 
         <- lookupVariantObjectWithSpec (contents mmissFile) variantSearch
      case fileVersionLinkOpt of
         Nothing -> return Nothing
         Just (fileVersionLink,variantSpec) ->
            do
               fileVersion <- readLink view fileVersionLink
               return (Just (text fileVersion,variantSpec))
         

-- ----------------------------------------------------------------------
-- Scanning for MMiSSFiles in the repository or in an external directory.
-- We expect an EntityFullName, which either
-- (1) contains a ".", in which case it should be the exact name of the
--     file, including extension.
-- or
-- (2) does not contain a ".", in which case we select all existing files,
--     which are formed by appending "." and a following extension.
-- We return the files as (name,extension).  For findMMiSSFilesInRepository
-- we also return a link to the MMiSSFile.
-- ----------------------------------------------------------------------

findMMiSSFilesInRepository :: HasLinkedObject folder 
   => folder -> EntityFullName -> IO [(Link MMiSSFile,EntityFullName,String)]
findMMiSSFilesInRepository folder fullName0 =
   do
      let
         folderLinkedObject = toLinkedObject folder

      (resultOpts :: [Maybe (Link MMiSSFile,EntityFullName,String)])
         <- mapM 
            (\ (fullName1,tag) ->
               do
                  fullName2 <- coerceWithErrorOrBreakIO 
                     (error "MMiSSFileType bug 1")
                     (unsplitFullName fullName1 tag)
                  linkedObjectOpt <- lookupFullNameInFolder folderLinkedObject
                     fullName2
                  case linkedObjectOpt of
                     Nothing -> return Nothing
                     Just linkedObject ->
                        do
                           let 
                              wrappedLink = toWrappedLink linkedObject
                           return (fmap
                              (\ link -> (link,fullName1,tag))
                              (unpackWrappedLink wrappedLink)
                              )
               )   
            (possibleNames fullName0)
      return (catMaybes resultOpts)

findMMiSSFilesInDirectory :: FilePath -> EntityFullName 
   -> IO [(EntityFullName,String)]
findMMiSSFilesInDirectory filePath0 fullName0 =
   do
      let
         filePath1 = trimDir filePath0
      filterM
         (\ (fullName1,tag) ->
            do
              let
                 fullNameStr 
                    = filePath1 `combineNames` 
                       ((toString fullName1) `unsplitExtension` tag)
              doesFileExist fullNameStr
            )
         (possibleNames fullName0)

-- | Scan an EntityFullName corresponding to a file, returning all possible
-- (name,extension) names it might have.
possibleNames :: EntityFullName -> [(EntityFullName,String)]
possibleNames fullName0 = case splitFullName fullName0 of
   Nothing ->
      map (\ tag -> (fullName0,tag)) (keysFM (fileTypes mmissFilesState))
   Just (nt @ (fullName1,tag)) -> 
      case lookupFM (fileTypes mmissFilesState) tag of
         Nothing -> [] -- not a recognised tag
         Just _ -> [nt]
      
-- ----------------------------------------------------------------------
-- Functions for reading MMiSSFilesState
-- ----------------------------------------------------------------------

mmissFilesState :: MMiSSFilesState
mmissFilesState = unsafePerformIO getMMiSSFilesState
{-# NOINLINE mmissFilesState #-}

getMMiSSFilesState :: IO MMiSSFilesState
getMMiSSFilesState =
   do
      filePath <- getTOPPath ["mmiss","Files.xml"]
      handleEither <- IO.try (openFile filePath ReadMode)
      let
         handle = case handleEither of
            Left excep ->
               error ("Error opening list of MMiSS file types from "
                  ++ filePath ++ ":" ++ show excep
                  )
            Right handle -> handle
      fileTypes <- hGetXml handle
      return (toMMiSSFilesState fileTypes)
       

toMMiSSFilesState :: FileTypes -> MMiSSFilesState
toMMiSSFilesState (FileTypes []) =
   MMiSSFilesState {
      fileTypes = emptyFM,
      menus = emptyFM
      }
toMMiSSFilesState (FileTypes (fileType : fileTypes1)) =
   let
      filesState0 = toMMiSSFilesState (FileTypes fileTypes1)
   in
      case fileType of
         FileTypes_FileType fileType ->
            let
               tag = fileTypeTag fileType

               mmissFileType = MMiSSFileType {
                  fileType = fileType,
                  typeId = constructKey tag
                  }

               fileTypes0 = fileTypes filesState0
               fileTypes1 = addToFM fileTypes0 tag mmissFileType
            in
               filesState0 {
                  fileTypes = fileTypes1
                  }
         FileTypes_Menu (menu @ (MMiSSFiles.Menu attrs _)) ->
            let
               menus0 = menus filesState0
               menus1 = addToFM menus0 (menuId attrs) menu
            in
               filesState0 {
                  menus = menus1
                  }


-- ----------------------------------------------------------------------
-- HasCodedValue instances
-- ----------------------------------------------------------------------

instance Monad m => HasBinary MMiSSFileType m where
   writeBin = mapWrite typeTag
   readBin = mapRead (\ tag ->
      lookupWithDefaultFM (fileTypes mmissFilesState)
         (error ("MMiSSFileType: unknown tag: " ++ tag))
         tag
       )

instance HasBinary MMiSSFile CodingMonad where
   writeBin = mapWriteIO
      (\ (MMiSSFile {
            mmissFileType = mmissFileType,
            title = title,
            linkedObject = linkedObject,
            contents = contents
            }) ->
         do
            contentsFrozen <- freezeVariantObject contents
            return (mmissFileType,title,linkedObject,contentsFrozen)
         )
   readBin = mapReadIO
      (\ (mmissFileType,title,linkedObject,contentsFrozen) ->
         do
            contents <- unfreezeVariantObject converter contentsFrozen
            return (MMiSSFile {
               mmissFileType = mmissFileType,
               title = title,
               linkedObject = linkedObject,
               contents = contents
               })
         )

instance HasBinary MMiSSFileVersion CodingMonad where
   writeBin = mapWrite text
   readBin = mapRead (\ text0 -> MMiSSFileVersion {text = text0})

-- ----------------------------------------------------------------------
-- HasLinkedObject instance
-- ----------------------------------------------------------------------

instance HasLinkedObject MMiSSFile where
   toLinkedObject = linkedObject

-- ----------------------------------------------------------------------
-- HasMerging instances
-- ----------------------------------------------------------------------

instance HasMerging MMiSSFile where
   getMergeLinks =
      let
         fn :: View -> Link MMiSSFile 
            -> IO (ObjectLinks (MMiSSVariants,FileData))
         fn view link =
            do
               object <- readLink view link
               variantObjectObjectLinks
                  (\ link -> 
                     return (ObjectLinks [(WrappedMergeLink link,FileData)])
                     )
                  (contents object)
      in
         MergeLinks fn

   attemptMerge = (\ linkReAssigner newView newLink vlos0 ->
      addFallOutWE (\ break ->
         do
            -- (0) Prune the objects list
            vlos1 <- mergePrune vlos0

            -- (1) Get the type of the first object, and check that the
            -- other objects also have this type.
            let
               ((headView,headLink,headFile):vlosRest) = vlos1

               tag = typeTag . mmissFileType

               thisTag = tag headFile

            mapM_
               (\ (_,_,file) ->
                  if tag file /= thisTag
                     then
                        break ("Type mismatch attempting to merge MMiSS "
                              ++ "file "++ title file)
                     else
                        done
                  )
               vlosRest

            let
               mmissFileType1 = mmissFileType headFile

            -- (2) Merge linked objects
            linkedObject1WE <- attemptLinkedObjectMerge
               linkReAssigner newView newLink
                  (map 
                     (\ (view,link,folder) -> (view,toLinkedObject folder))
                     vlos1
                     )

            linkedObject1 <- coerceWithErrorOrBreakIO break linkedObject1WE

            -- (3) Merge dictionaries
            let
               -- This is the function passed to 
               -- MMiSSVariantObject.attemptMergeVariantObject.
               reAssign :: View -> Link MMiSSFileVersion 
                  -> IO (Link MMiSSFileVersion)
               reAssign oldView link0 =
                  return (mapLink linkReAssigner oldView link0)

            contents1 <- attemptMergeVariantObject
               converter
               reAssign
               (map (\ (view,_,file) -> (view,contents file)) vlos1)

            canClone <- case vlosRest of
               [] ->
                  do
                     linkedsSame <- linkedObjectsSame linkedObject1 
                        (toLinkedObject headFile)
                     if linkedsSame
                        then
                           do
                              variantsSame <- variantObjectsSame
                                 (==)
                                 contents1 (contents headFile)
                              return variantsSame
                         else
                            return False
               _ -> return False 
                            

            if canClone
               then
                  cloneLink headView headLink newView newLink
               else
                  do
                     -- (4) Create the file, and put it in the view.
                     let
                        mmissFile1 = MMiSSFile {
                           mmissFileType = mmissFileType1,
                           title = title headFile,
                              -- these MUST be the same for all files,
                              -- since files with different titles will
                              -- have different names in the folder, and so
                              -- not be merged.
                           linkedObject = linkedObject1,
                           contents = contents1
                           }

                     setLink newView mmissFile1 newLink

                     done
         )
      )

   copyObject = Just (\ view0 mmissFile0 view1 getNewVersionInfo ->
      do
         let
            contents0 = contents mmissFile0
         contents1 <- copyVariantObject getNewVersionInfo contents0
         let
            mmissFile1 = mmissFile0 {contents = contents1}
         return mmissFile1
      )


data FileData = FileData deriving (Eq,Ord,Show,Typeable)

instance HasMerging MMiSSFileVersion where

   getMergeLinks = emptyMergeLinks

   attemptMerge = (\ linkReAssigner newView newLink vlos ->
      do
         vlosPruned <- mergePrune vlos
         case vlosPruned of
            [(oldView,oldLink,_)] ->
               do
                  cloneLink oldView oldLink newView newLink
                  return (hasValue ())
            _ ->
               return (hasError 
                  "Unexpected merge required of MMiSSFileVersion")
      )


-- ----------------------------------------------------------------------
-- Instance of HasBundleNodeData
-- ----------------------------------------------------------------------

instance HasBundleNodeData MMiSSFile where
   getBundleNodeData view mmissFile exportOpts =
      do
         (allFileVariants :: [(MMiSSVariantSpec,Link MMiSSFileVersion)]) 
            <- getAllVariants (contents mmissFile)
         (variants :: [(Maybe MMiSSVariantSpec,BundleText)]) <-
            mapM
               (\ (variantSpec,versionLink) ->
                  do
                     bundleText <- if getText exportOpts
                        then
                           do
                              mmissFileVersion <- readLink view versionLink
                              return (BundleString {
                                 MMiSSBundle.contents = text mmissFileVersion,
                                 charType = Byte
                                 })
                        else
                           return NoText
                     return (Just variantSpec,bundleText)
                  )
               allFileVariants
         return (MMiSSBundle.Object variants)

   getBundleNodeDataForVariant view mmissFile exportOpts variantSearch =
      do
         icslOpt <- lookupMMiSSFileVariantWithSpec view mmissFile variantSearch
         case icslOpt of
            Nothing -> 
               do
                  errorMess ("No variant matching " ++ show variantSearch 
                     ++ " found")
                  return NoData
            Just (icsl,variantSpec) ->
               let
                  bundleText =
                     if getText exportOpts
                        then
                           BundleString {
                              MMiSSBundle.contents = icsl,
                              charType = Byte
                              }
                        else
                           NoText -- fairly pointless but consistent

                  bundleNodeData = MMiSSBundle.Object 
                     [(Just variantSpec,bundleText)]
               in
                  return bundleNodeData
                  
-- ----------------------------------------------------------------------
-- ObjectType instance
-- ----------------------------------------------------------------------

instance ObjectType MMiSSFileType MMiSSFile where
   objectTypeTypeIdPrim _ = "MMiSSFileType"

   objectTypeIdPrim = typeId

   objectTypeGlobalRegistry _ = globalRegistry

   extraObjectTypes = return (eltsFM (fileTypes mmissFilesState))

   getObjectTypePrim = mmissFileType

   toLinkedObjectOpt object = Just (linkedObject object)

   nodeTitlePrim = fullTitle 

   getNodeDisplayData = getFilesNodeDisplayData
 

getFilesNodeDisplayData :: 
   (GraphAllConfig graph graphParms node nodeType nodeTypeParms 
      arc arcType arcTypeParms)
   => View -> WrappedDisplayType -> MMiSSFileType 
   -> IO (DisplayedView graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms)
   -> IO (Maybe (NodeDisplayData graph node nodeTypeParms arcTypeParms 
         MMiSSFileType MMiSSFile))
getFilesNodeDisplayData view displayType mmissFileType 
      (getDisplayedView :: IO (DisplayedView graph graphParms 
         node nodeType nodeTypeParms arc arcType arcTypeParms)) =
   do
      let
         fileType0 = fileType mmissFileType

         colour1 = case fileTypeColour fileType0 of
            Nothing -> Color "white"
            Just s -> Color s

      shape1 <- case fileTypeShape fileType0 of
         Nothing -> return Box
         Just fileTypeShape0 -> case readCheck fileTypeShape0 of
            Just shape -> return shape
            Nothing ->
               do
                  putStrLn ("Incomprehensible shape " ++ show fileTypeShape0
                     ++ " ignored")
                  return Box
      let
         getTitle :: Link MMiSSFile -> IO String
         getTitle link =
            do
               file <- readLink view link
               return (title file)

         theNodeType = fromString ""

         nodeTypeParms0 :: nodeTypeParms (Link MMiSSFile)
         nodeTypeParms0 =
            shape1 $$$
            colour1 $$$
            (ValueTitle getTitle) $$$
            fontStyleSource view $$$
            emptyNodeTypeParms

         nodeTypeParms = case fileTypeMenu fileType0 of
            Nothing -> nodeTypeParms0
            Just menuId -> 
               LocalMenu (mkMenu menuId) $$$ nodeTypeParms0

         mkMenu :: String -> MenuPrim (Maybe String) (Link MMiSSFile -> IO ())
         mkMenu menuId = case lookupFM (menus mmissFilesState) menuId of
            Just (MMiSSFiles.Menu menuAttrs subMenus) ->
               MenuType.Menu (menuTitle menuAttrs) (map mkSubMenu subMenus)

         mkSubMenu :: Menu_ 
            -> MenuPrim (Maybe String) (Link MMiSSFile -> IO ())
         mkSubMenu (Menu_DisplayVariants 
               (DisplayVariants {displayVariantsTitle = title})) =
            Button (fromDefaultable title) (displayVariants view)
         mkSubMenu (Menu_SelectVariants 
               (SelectVariants {selectVariantsTitle = title})) =
             Button (fromDefaultable title) (selectVariants view)
         mkSubMenu (Menu_SubMenu (SubMenu {subMenuMenu = subMenuMenu})) =
            mkMenu subMenuMenu
         mkSubMenu (Menu_Separator Separator) = Blank
         mkSubMenu (Menu_Command (Command {commandTitle = title,
            commandConfirm = confirm,commandCommand = command})) =
               Button title (execCommand view title confirm command)

         nodeDisplayData = NodeDisplayData {
            topLinks = [],
            arcTypes = [],
            nodeTypes = [(theNodeType,nodeTypeParms)],
            getNodeType = (\ _ -> theNodeType),
            getNodeLinks = (\ _ -> return emptyArcEnds),
            closeDown = done,
            specialNodeActions = emptySpecialNodeActions
            }

      return (Just nodeDisplayData)

displayVariants :: View -> Link MMiSSFile -> IO ()
displayVariants view link = 
   do
      mmissFile <- readLink view link
      displayObjectVariants (contents mmissFile)

-- loosely based on MMiSSEditAttributes
selectVariants :: View -> Link MMiSSFile -> IO ()
selectVariants view link =
   do
      object <- readLink view link
      changed <- editMMiSSSearchObject (title object) (contents object)
      if changed
         then
            dirtyLink view link
         else
            done 

-- ----------------------------------------------------------------------
-- HasBundleNodeWrite instance
-- ----------------------------------------------------------------------

instance HasBundleNodeWrite MMiSSFile where
   bundleNodeWrite view node fileLink =
      do
         isNew <- isEmptyLink view fileLink
         if isNew
            then
               do
                  let
                     Just ext1 = ext . objectType . fileLoc $ node
                     Just mmissFileType = 
                        lookupFM (fileTypes mmissFilesState) ext1
                     Just title = name . fileLoc $ node

                  linkedObjectWE <- newLinkedObject view (WrappedLink fileLink)
                     Nothing
                  linkedObject <- coerceImportExportIO linkedObjectWE
                  contents <- newEmptyVariantObject (return . id)
                  let
                     file = MMiSSFile {
                        mmissFileType = mmissFileType,
                        title = title,
                        linkedObject = linkedObject,
                        contents = contents
                        }
                  writeLink view fileLink file
            else
               done
         let
            newVersions :: [(MMiSSVariantSpec,MMiSSFileVersion)]
            newVersions = 
               map
                  (\ (Just variantSpec,bundleText) ->
                     (variantSpec,
                        MMiSSFileVersion {
                           text = MMiSSBundle.contents bundleText
                           }
                        )
                     )
                  (toVariants node)

         file <- readLink view fileLink
         let
            contents0 = contents file

         updateVariantObject view fileLink (contents file) id (return . id) 
            newVersions

-- ------------------------------------------------------------------
-- Executing commands
-- ------------------------------------------------------------------

execCommand :: View -> String -> Maybe String -> String -> Link MMiSSFile 
   -> IO ()
execCommand view title0 confirm command0 link =
   do
      mmissFile <- readLink view link
      tempDir <- newTempFile
      let
         name = title mmissFile

         tag = fileTypeTag . fileType . mmissFileType $ mmissFile

         fullName = combineNames tempDir (name ++ "." ++ tag)
      
         dict :: Char -> Maybe String
         dict 'F' = Just fullName
         dict 'N' = Just name
         dict _ = Nothing

      goAhead <- case confirm of
         Nothing -> return True
         Just confirmString0 ->
            do
               let
                  confirmString1 = doFormatString confirmString0 dict
               seq confirmString1 done
               confirmMess confirmString1  

      if goAhead
         then
            do
               let
                  command1 = doFormatString command0 dict
               seq command1 done

               createDirectory tempDir
               fileVersionLink 
                  <- readContents . toVariantObjectCache . contents $ mmissFile
               fileVersion <- readLink view fileVersionLink
               copyICStringLenToFile (text fileVersion) fullName
               withDir tempDir (runCommand title0 command1)
               done        
         else
            done            



-- ------------------------------------------------------------------
-- The globalRegistry (currently unused).
-- ------------------------------------------------------------------

globalRegistry :: GlobalRegistry MMiSSFileType
globalRegistry = System.IO.Unsafe.unsafePerformIO createGlobalRegistry
{-# NOINLINE globalRegistry #-}

-- ----------------------------------------------------------------------
-- Miscellaneous utilities
-- ----------------------------------------------------------------------

fullTitle :: MMiSSFile -> String
fullTitle file = unsplitExtension (title file) (typeTag . mmissFileType $ file)

typeTag :: MMiSSFileType -> String
typeTag = fileTypeTag . fileType

-- | Return the key in the global registry for objects with this tag
constructKey :: String -> GlobalKey
constructKey tag = oneOffKey "MMiSSFiles" tag


fileTypeExists :: String -> Bool
fileTypeExists ext = elemFM ext (fileTypes mmissFilesState)

-- Make a name for an MMiSSFile, as known to the LinkManager.  We use the
-- EntityName specialChar.
unsplitSpecialWE :: String -> String -> WithError EntityName
unsplitSpecialWE name ext = fromStringWE (name ++ [specialChar] ++ ext)

-- Our converter function, which is rather trivial.
converter :: MMiSSVariantSpec -> Link MMiSSFileVersion 
   -> IO (Link MMiSSFileVersion)
converter _ fileVersionLink = return fileVersionLink
