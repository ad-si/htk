{- This module defines the MMiSSPackageFolder type.  An MMiSSPackageFolder
   is a special sort of folder containing exactly one package.
   -}
module MMiSSPackageFolder(
   MMiSSPackageFolder,
   MMiSSPackageFolderType,

   toMMiSSPreambleLink, -- :: MMiSSPackageFolder -> Link MMiSSPreamble
   getMMiSSPackageFolder,  
      -- :: HasLinkedObject object => View -> object 
      -- -> IO (WithError MMiSSPackageFolder)
   toMMiSSPackageFolder,  
      -- :: HasLinkedObject object => View -> object 
      -- -> SimpleSource (WithError MMiSSPackageFolder)
   getMMiSSPackageFolderAndName,
      -- :: HasLinkedObject object => View -> object 
      -- -> IO (WithError (MMiSSPackageFolder,EntityFullName))
   toMMiSSPackageFolderLinkedObject, 
     -- :: MMiSSPackageFolder -> LinkManager.LinkedObject

   lookupMMiSSObject,
      -- :: View -> MMiSSPackageFolder -> EntitySearchName 
      -- -> IO (WithError (Maybe (Link MMiSSObject)))
      -- Look up a particular object, starting from a folder.
      -- (Actually is now implemented in MMiSSObjectTypeInstance)
   lookupMMiSSObjectMustExist,
      --  :: View -> MMiSSPackageFolder -> EntitySearchName 
      -- -> IO (WithError (Link MMiSSObject))
      -- Like lookupMMiSSObject, but returns an error if the object does not
      -- exist.
   lookupMMiSSPackageFolder,
      -- :: View -> MMiSSPackageFolder -> EntitySearchName 
      -- -> IO (WithError (Maybe (Link MMiSSPackageFolder)))
      -- Get a package-folder from another one by search name.

   unpackWrappedLinkToMMiSSPackageFolder,
      -- :: WrappedLink -> Maybe (Link MMiSSPackageFolder)
      -- version of unpackWrappedLink for the .hi-boot file.
   newEmptyLinkMMiSSPackageFolder,
      -- :: View -> IO (Link MMiSSPackageFolder)
      -- version of newEmptyLink for the .hi-boot file.
   wrapMMiSSPackageFolderLink,
      -- :: Link MMiSSPackageFolder -> WrappedLink
      -- version of WrappedLink for the .hi-boot file.
   linkToLinkedObjectMMiSSPackageFolder,
      -- :: View -> Link MMiSSPackageFolder -> IO LinkedObject
      -- Yet another function for the .hi-boot file (sigh).

   importMMiSSPackage1,
      -- :: View -> LinkedObject -> Maybe String
      -- -> IO Bool
      -- Import a new MMiSSPackage into a folder designated by a LinkedObject.
      -- The String, if supplied, is the file-path to read it from.
      -- returns True if successful.

   reimportMMiSSPackage1,
      -- :: View -> Link MMiSSPackageFolder -> Maybe String -> IO ()
      -- Reimport an MMiSSPackage into its existing package folder.
      -- The String, if supplied, is the file-path to read it from.
      

   ) where

import Maybe
import List

import System.IO.Unsafe
import Control.Concurrent.MVar
import Data.FiniteMap

import Computation
import ExtendedPrelude
import FileNames
import Dynamics
import Broadcaster
import Sink
import Sources
import AtomString(fromString,toString,fromStringWE)
import VariableSet
import VariableSetBlocker
import VariableList
import Delayer(delay)
import Messages
import WBFiles

import Events(sync)

import WithDir

import MenuType
import SimpleForm
import FileDialog

import GraphDisp
import GraphConfigure

import VersionDB(Location)
import View
import Folders
import Link
import LinkDrawer
import LinkManager
import GlobalRegistry
import CodedValue
import EntityNames
import ObjectTypes
import DisplayParms
import SpecialNodeActions
import MergeTypes
import MergePrune

import LaTeXParser(emptyMMiSSLatexPreamble)

import MMiSSSubFolder
import MMiSSFormat
import MMiSSSplitLink
import MMiSSFileSystemExamples
import MMiSSObjectType hiding (linkedObject)
import MMiSSObjectTypeType hiding (displayParms)
import MMiSSPreamble
import MMiSSImportExportErrors
import MMiSSBundle
import MMiSSBundleSimpleUtils
import MMiSSBundleNodeWriteClass
import MMiSSBundleConvert

import {-# SOURCE #-} MMiSSExportLaTeX(pathRef)
import {-# SOURCE #-} MMiSSObjectTypeInstance
import {-# SOURCE #-} MMiSSBundleWrite

-- ------------------------------------------------------------------------
-- The MMiSSPackageFolderType type and its instance of HasCodedValue
-- ------------------------------------------------------------------------

newtype MMiSSPackageFolderType = MMiSSPackageFolderType {
   displayParms :: NodeTypes (Link MMiSSPackageFolder)
   } deriving (Typeable)

theMMiSSPackageFolderType = MMiSSPackageFolderType {
   displayParms = readDisplay "white box"
   }

mmissPackageFolderType_tyRep
    = mkTyRep "MMiSSPackageFolder" "MMiSSPackageFolderType"

instance Monad m => HasBinary MMiSSPackageFolderType m where
   writeBin = mapWrite (\ _ -> ())
   readBin = mapRead (\ () -> theMMiSSPackageFolderType)

-- ------------------------------------------------------------------------
-- The MMiSSPackageFolder type and its instances of Eq, Ord and HasBinary
-- ------------------------------------------------------------------------

data MMiSSPackageFolder = MMiSSPackageFolder {
   linkedObject :: LinkedObject,
   blocker2 :: Blocker (EntityName,WrappedLink),
      -- blocker for package contents.
   blocker3 :: Blocker WrappedLink,
      -- blocker for preamble link.
   hideFolderArcs :: SimpleBroadcaster (Maybe NodeArcsHidden),
      -- Used for hiding the package folder (a similar mechanism to that
      -- used for plain folders).
   preambleLink :: Link MMiSSPreamble
      -- Link to this package's preamble.
   } deriving (Typeable)

instance Eq MMiSSPackageFolder where
   (==) = mapEq linkedObject


instance Ord MMiSSPackageFolder where
   compare = mapOrd linkedObject

instance HasBinary MMiSSPackageFolder CodingMonad where
   writeBin = mapWrite
      (\ (MMiSSPackageFolder {
            linkedObject = linkedObject,
            preambleLink = preambleLink
            })
         -> (linkedObject,preambleLink)
         )
   readBin = mapReadViewIO
      (\ view (linkedObject,preambleLink) ->
         do
            (mmissPackageFolder,postMerge) 
               <- createMMiSSPackageFolder view linkedObject preambleLink
            doPostMerge postMerge
            return mmissPackageFolder
         )

instance HasLinkedObject MMiSSPackageFolder where
   toLinkedObject mmissPackageFolder = linkedObject mmissPackageFolder

-- ------------------------------------------------------------------------
-- Functions which need to go in the .boot.hs file.
-- ------------------------------------------------------------------------

toMMiSSPreambleLink :: MMiSSPackageFolder -> Link MMiSSPreamble
toMMiSSPreambleLink = preambleLink

linkToMMiSSPreambleLink 
   :: View -> Link MMiSSPackageFolder -> IO (Link MMiSSPreamble)
linkToMMiSSPreambleLink view link =
   do
      packageFolder <- readLink view link
      return (preambleLink packageFolder)


toMMiSSPackageFolderLinkedObject 
   :: MMiSSPackageFolder -> LinkManager.LinkedObject
toMMiSSPackageFolderLinkedObject = linkedObject

toMMiSSPackageFolder :: HasLinkedObject object => View -> object 
   -> SimpleSource (WithError MMiSSPackageFolder)
toMMiSSPackageFolder view object =
      toMMiSSPackageFolder1 (toLinkedObject object)
   where
      toMMiSSPackageFolder1 
         :: LinkedObject -> SimpleSource (WithError MMiSSPackageFolder)
      toMMiSSPackageFolder1 linkedObject =
         case toMMiSSPackageFolder2 linkedObject of
            Just packageFolderLink ->
               mkIOSimpleSource (
                 do
                    packageFolder <- readLink view packageFolderLink
                    return . return . return $ packageFolder
                 )
            Nothing ->
               do
                  insertionOpt <- toInsertion linkedObject
                  case insertionOpt of
                     Nothing -> 
                        mkIOSimpleSource (
                           do
                              fullName <- getFullName view object
                              return . return . fail $
                                 ("MMiSS object " ++ fullName 
                                    ++ " somehow not in an MMiSSPackageFolder")
                           )
                     Just insertion ->
                        let
                           (parentLinkedObject,_) = unmkInsertion insertion
                        in
                           toMMiSSPackageFolder1 parentLinkedObject

      toMMiSSPackageFolder2 :: LinkedObject -> Maybe (Link MMiSSPackageFolder)
      toMMiSSPackageFolder2 = unpackWrappedLink . toWrappedLink

getMMiSSPackageFolder :: HasLinkedObject object 
   => View -> object -> IO (WithError MMiSSPackageFolder)
getMMiSSPackageFolder view object = 
   do 
      resultWE <- getMMiSSPackageFolderAndName view object
      return (mapWithError fst resultWE)

getMMiSSPackageFolderAndName :: HasLinkedObject object
   => View -> object -> IO (WithError (MMiSSPackageFolder,EntityFullName))
getMMiSSPackageFolderAndName view object =
   do
      insertionOpt <- getCurrentInsertion (toLinkedObject object)
      case insertionOpt of
         Nothing -> return (hasError ("object mysteriously detached"))
         Just insertion ->
            let
               (parentLinkedObject,thisName) = unmkInsertion insertion
            in
               case splitLinkedObject parentLinkedObject of
                  MMiSSPackageFolderC packageFolderLink ->
                     do
                        packageFolder <- readLink view packageFolderLink 
                        return (hasValue 
                           (packageFolder,EntityFullName [thisName]))
                  FolderC folderLink ->
                     do
                        (folder :: Folder) <- readLink view folderLink
                        let
                           folderType :: FolderType
                           folderType = getObjectTypePrim folder

                        if objectTypeIdPrim folderType 
                              == objectTypeIdPrim mmissSubFolderType
                           then
                              do
                                 recurseWE <- getMMiSSPackageFolderAndName
                                    view folder
                                 return (mapWithError
                                    (\ (packageFolder,fullName0) ->
                                       (packageFolder,
                                          combineDirBase fullName0 thisName
                                          )
                                       )
                                    recurseWE
                                    )
                           else
                              notInFolder  
                  _ -> notInFolder
   where
      notInFolder = return (hasError ("object not in package folder"))


unpackWrappedLinkToMMiSSPackageFolder 
   :: WrappedLink -> Maybe (Link MMiSSPackageFolder)
unpackWrappedLinkToMMiSSPackageFolder = unpackWrappedLink

wrapMMiSSPackageFolderLink :: Link MMiSSPackageFolder -> WrappedLink
wrapMMiSSPackageFolderLink = WrappedLink

newEmptyLinkMMiSSPackageFolder :: View -> IO (Link MMiSSPackageFolder)
newEmptyLinkMMiSSPackageFolder = newEmptyLink

linkToLinkedObjectMMiSSPackageFolder
   :: View -> Link MMiSSPackageFolder -> IO LinkedObject
linkToLinkedObjectMMiSSPackageFolder view link =
   do
      object <- readLink view link
      return (toLinkedObject object)

          
-- ------------------------------------------------------------------------
-- Constructing the MMiSSPackageFolder
-- ------------------------------------------------------------------------

createMMiSSPackageFolder :: View -> LinkedObject -> Link MMiSSPreamble 
   -> IO (MMiSSPackageFolder,PostMerge)
createMMiSSPackageFolder view linkedObject preambleLink =
   do
      let
         -- Create blocker for links to contents.
         contentsSet :: VariableSetSource (EntityName,WrappedLink)
         contentsSet = objectContentsWithName linkedObject

      blocker2 <- newBlocker contentsSet

      let
         postMergeAct :: IO ()
         -- Create an action which is only to be done after the view has been
         -- fully initialised.  This will monitor changes to the preamble,
         -- and make corresponding changes to the import commands of the 
         -- linkedObject
         postMergeAct =
            do
               mmissPreamble <- readLink view preambleLink
               let
                  importCommands :: SimpleSource ImportCommands
                  importCommands = toImportCommands mmissPreamble              

                  setCommands' commands =
                     setCommands linkedObject commands

               sinkID <- newSinkID
               parallelX <- newParallelExec

               addNewSourceActions (toSource importCommands)
                  setCommands' setCommands' sinkID parallelX

               done


         preambleSet :: VariableSetSource WrappedLink
         preambleSet = singletonSetSource
            (staticSimpleSource (WrappedLink preambleLink))

      blocker3 <- newBlocker preambleSet

      let
         mkArcsHiddenSource :: IO (SimpleBroadcaster (Maybe NodeArcsHidden))
         mkArcsHiddenSource = newSimpleBroadcaster Nothing

      hideFolderArcs <- mkArcsHiddenSource

      return (MMiSSPackageFolder {
         linkedObject = linkedObject,
         blocker2 = blocker2,
         blocker3 = blocker3,
         hideFolderArcs = hideFolderArcs,
         preambleLink = preambleLink
         }, newPostMerge postMergeAct
         )

-- ------------------------------------------------------------------------
-- Merging
-- ------------------------------------------------------------------------

instance HasMerging MMiSSPackageFolder where

   getMergeLinks = 
      let
         -- need to process LinkedObject links to add on a link for the
         -- preamble.
         mergeLinks1 = getLinkedObjectMergeLinks
         mergeLinks2 = singletonMergeLinks 
            (\ mmissPackageFolder 
               -> WrappedMergeLink (preambleLink mmissPackageFolder))
      in
         pairMergeLinks mergeLinks1 mergeLinks2

   attemptMergeWithPostMerge linkReAssigner newView newLink vlos =
      addFallOutWE (\ break ->
         do
            -- compare with similar code in Folders.  But this is
            -- simple as we don't have attributes or more than one
            -- type.
            (vlosPruned @ ((view1,link1,folder1) : _)) <- mergePrune vlos

            let
               preambleLink1 = preambleLink folder1

               preambleLink2 = mapLink linkReAssigner view1 preambleLink1

            newLinkedObjectWE <- attemptLinkedObjectMerge
               linkReAssigner newView newLink
                  (map 
                     (\ (view,link,folder) -> (view,toLinkedObject folder))
                     vlos
                     )

            newLinkedObject <- coerceWithErrorOrBreakIO break newLinkedObjectWE

            isSame 
               <- linkedObjectsSame newLinkedObject (toLinkedObject folder1)
            if (isSame && preambleLink1 == preambleLink2)
               then
                  do
                     cloneLink view1 link1 newView newLink
                     return (newPostMerge done)
               else
                  do
                     (mmissPackageFolder,postMerge) <- createMMiSSPackageFolder 
                        newView newLinkedObject preambleLink2
                     setLink newView mmissPackageFolder newLink
                     return postMerge
      )

      
-- ------------------------------------------------------------------------
-- The instance of ObjectTypes.
-- ------------------------------------------------------------------------

instance ObjectType MMiSSPackageFolderType MMiSSPackageFolder where

   objectTypeTypeIdPrim _ = "MMiSSPackageFolder"

   objectTypeIdPrim _ = oneOffKey "MMiSSPackageFolder" ""

   objectTypeGlobalRegistry _ = globalRegistry

   extraObjectTypes = return [theMMiSSPackageFolderType]

   getObjectTypePrim _ = theMMiSSPackageFolderType

   createObjectMenuItemPrim _ = Just ("New Package",importMMiSSPackage)

   toLinkedObjectOpt object = Just (linkedObject object)

   nodeTitleSourcePrim object = 
      fmap toString (
         getLinkedObjectTitle (linkedObject object) 
            (fromString "UndefinedMMiSSPackageFolder")
         )

   getNodeDisplayData view wrappedDisplayType mmissPackageFolderType _ =
      do
         blockID <- newBlockID

         let
            nodeTypeParmsOpt = getNodeTypeParms wrappedDisplayType
               (displayParms mmissPackageFolderType)

            theNodeType = fromString ""

            openAction link =
               do
                  folder <- readLink view link
                  bracketForImportErrors view (
                     do
                        openBlocker (blocker2 folder) blockID
                        openBlocker (blocker3 folder) blockID
                     )

            closeAction link =
               do
                  folder <- readLink view link
                  delay view (
                     do
                        closeBlocker (blocker3 folder) blockID
                        closeBlocker (blocker2 folder) blockID
                     )

            menuOptions = [
               Button "Open Package" (\ link -> openAction link),
               Button "Close Package" (\ link -> closeAction link),
               Button "Reimport Package" (\ link 
                  -> reimportMMiSSPackage view link),
               Button "Hide Links" (\ link -> hideAction link True),
               Button "Reveal Links" (\ link -> hideAction link False)
               ]

            menu = LocalMenu (Menu Nothing menuOptions)

            getNodeLinks1 link =
               do 
                  folder <- readLink view link

                  entityNameOpt <- readContents (
                     getLinkedObjectTitleOpt (toLinkedObject folder))

                  entityName0 <- case entityNameOpt of
                     -- objects with this name, in particular the head object,
                     -- will be displayed with a different sort of edge.
                     Nothing -> -- this shouldn't really happen
                        do
                           putStrLn "Displaying folder with no name??"
                           return (EntityName "#BADNAME")
                     Just entityName0 -> return entityName0
                    

                  arcs2 <- toArcEndsForContents entityName0 (blocker2 folder) 
                     blockID 
                  arcs3 
                     <- toArcEnds (blocker3 folder) blockID thePreambleArcType
                  let
                     arcs = catVariableLists arcs2 arcs3

                  return arcs

         case nodeTypeParmsOpt of
            Nothing -> return Nothing
            Just nodeTypeParms0 ->
               let
                  nodeTypeParms1 =
                     menu $$$
                     (valueTitleSource view) $$$
                     (fontStyleSource view) $$$
                     (DoubleClickAction openAction) $$$
                     nodeTypeParms0

                  preambleArcTypeParms =
                     Color "blue" $$$
                     emptyArcTypeParms

                  headPackageArcTypeParms =
                     Double $$$
                     emptyArcTypeParms

                  nodeDisplayData = NodeDisplayData {
                     topLinks = [],
                     arcTypes = [
                        (theArcType,emptyArcTypeParms),
                        (thePackageHeadArcType,headPackageArcTypeParms),
                        (thePreambleArcType,preambleArcTypeParms)],
                     nodeTypes = [(theNodeType,nodeTypeParms1)],
                     getNodeType = const theNodeType,
                     getNodeLinks = getNodeLinks1,
                     closeDown = done,
                     specialNodeActions =
                        (\ object ->
                           fmap
                              (\ arcsHidden ->
                                 (\ graph node ->
                                    modify arcsHidden graph node
                                    )
                                 )
                              (toSimpleSource (hideFolderArcs object))
                           )
                 
                     }
               in
                  return (Just nodeDisplayData)
      where
         hideAction :: Link MMiSSPackageFolder -> Bool -> IO () 
         hideAction link bool =
            do
               folder <- readLink view link
               broadcast (hideFolderArcs folder) (Just (NodeArcsHidden bool))

-- ------------------------------------------------------------------------
-- The instance of HasBundleNodeWrite
-- ------------------------------------------------------------------------

instance HasBundleNodeWrite MMiSSPackageFolder where
   bundleNodeWrite1 view bundleNodeLocations thisLocation node 
         packageFolderLink =
      do
         isNew <- isEmptyLink view packageFolderLink
         if isNew
            then
               do
                  linkedObjectWE <- newLinkedPackageObject view 
                     (WrappedLink packageFolderLink) Nothing
                  linkedObject <- coerceImportExportIO linkedObjectWE

                  let
                     preambleLocation = preambleEntityName : thisLocation

                  preambleLink <- case lookupFM (fm bundleNodeLocations) 
                        preambleLocation of
                     Just bundleNodeExtraData -> 
                        let
                           preambleWrappedLink = location bundleNodeExtraData
                           Just preambleLink 
                              = unpackWrappedLink preambleWrappedLink
                        in
                           return preambleLink
                     Nothing -> 
                        do
                           warningMess 
                              "No preamble found; inserting an empty one"
                           createPreamble view emptyMMiSSLatexPreamble


                  (packageFolder,postMerge) 
                     <- createMMiSSPackageFolder view linkedObject preambleLink
                  writeLink view packageFolderLink packageFolder
                  return (doPostMerge postMerge)
            else
               return done

-- ------------------------------------------------------------------------
-- The global registry (currently unused)
-- ------------------------------------------------------------------------

globalRegistry :: GlobalRegistry MMiSSPackageFolderType
globalRegistry = System.IO.Unsafe.unsafePerformIO createGlobalRegistry
{-# NOINLINE globalRegistry #-}
 

-- ------------------------------------------------------------------------
-- Generating the ArcEnds
-- ------------------------------------------------------------------------

toArcEnds :: Blocker WrappedLink -> BlockID -> ArcType -> IO ArcEnds
toArcEnds blocker blockID arcType =
   toArcEndsGeneral blocker blockID 
      (\ wrappedLink -> toArcData wrappedLink arcType True)

toArcEndsForContents 
   :: EntityName -> Blocker (EntityName,WrappedLink) -> BlockID -> IO ArcEnds
toArcEndsForContents name0 blocker blockID =
   toArcEndsGeneral blocker blockID
      (\ (name1,wrappedLink) ->
         toArcData wrappedLink 
            (if name1 == name0 then thePackageHeadArcType else theArcType)
            True
         )

instance HasKey (EntityName,WrappedLink) (EntityName,Location) where
   toKey (name,wrappedLink) = (name,toKey wrappedLink)
    
theArcType :: ArcType
theArcType = fromString "T"

thePackageHeadArcType :: ArcType
thePackageHeadArcType = fromString "H"

theInvisibleArcType :: ArcType
theInvisibleArcType = fromString ""

thePreambleArcType :: ArcType
thePreambleArcType = fromString "B"

-- ------------------------------------------------------------------------
-- Importing a new package
-- ------------------------------------------------------------------------

importMMiSSPackage :: View -> LinkedObject -> IO Bool
importMMiSSPackage view linkedObject 
   = importMMiSSPackage1 view linkedObject Nothing

-- The last argument is the file path to import the file from.  If Nothing,
-- as for importMMiSSPackage, we will prompt the user.
importMMiSSPackage1 :: View -> LinkedObject -> Maybe String -> IO Bool
importMMiSSPackage1 view parentLinkedObject filePathOpt0 =
   displayImportExportErrors False (
      do
         folderLink <- case splitLinkedObject parentLinkedObject of
            FolderC folderLink -> return folderLink
            _ -> importExportError 
               "You may only import a package into a folder"
       
         (whereIsData :: Maybe (FilePath,Format)) <- 
            findData filePathOpt0
                    
         case whereIsData of
            Nothing -> return False
            Just (filePath,format) ->
               do
                  let
                     (dirPath,_) = splitName filePath

                  (bundle,packageId) 
                     <- parseBundle format standardFileSystem filePath
                  let
                     nameWE = guessName bundle packageId
                  name <- coerceImportExportIO nameWE
                  writeBundle bundle (Just packageId) (Just dirPath) view 
                     (Right (folderLink,name)) 
                  messageMess ("Import of " ++ toString name ++
                     " successful")
                  return True
      )

-- Some tiresome code to work out what to call the package in which we
-- insert a bundle.  We look for an element 
guessName :: Bundle -> PackageId -> WithError EntityName
guessName (Bundle packageBundles) packageId = 
   case List.lookup packageId packageBundles of
      Nothing -> fail ("Bug: missing packageId " ++ toString packageId)
      Just bundleNode -> guessNodeName bundleNode

guessNodeName :: BundleNode -> WithError EntityName
guessNodeName bundleNode = case bundleNodeData bundleNode of
   MMiSSBundle.Object _ ->
      do
         nameOpt <- nameFileLocOpt (fileLoc bundleNode)
         case nameOpt of
            Nothing -> fail 
               "Element has no name, and I don't know where to put it"
            Just name -> return name
   MMiSSBundle.Dir nodes -> 
      let
         nodesNotPreambles = 
            filter
               (\ node -> case base . objectType . fileLoc $ node of
                  MMiSSPreambleEnum -> False
                  _ -> True
                  )
               nodes
      in
         case nodesNotPreambles of
            [node] -> guessNodeName node
            _ -> fail (
              "Bug: bundleNodeData after parseBundle contains 0 or >1 "
              ++ "non-preamble nodes")
   NoData -> fail "Bug: bundleNodeData after parseBundle contains NoData"
 

-- Import a new version into an existing MMiSS package.
reimportMMiSSPackage :: View -> Link MMiSSPackageFolder -> IO ()
reimportMMiSSPackage view packageFolderLink = 
   reimportMMiSSPackage1 view packageFolderLink Nothing

reimportMMiSSPackage1 :: View -> Link MMiSSPackageFolder -> Maybe String 
   -> IO ()
reimportMMiSSPackage1 view packageFolderLink filePathOpt0 =
   displayImportExportErrors () (
      do
         packageFolder <- readLink view packageFolderLink

         (whereIsData :: Maybe (FilePath,Format)) <- 
            findData filePathOpt0
                    
         case whereIsData of
            Nothing -> done
            Just (filePath,format) ->
               do
                  let
                     (dirPath,_) = splitName filePath

                  (bundle,packageId) 
                     <- parseBundle format standardFileSystem filePath
                  writeBundle bundle (Just packageId) (Just dirPath) view 
                     (Left (toLinkedObject packageFolder))
                  fullName <- getFullName view packageFolder
                  messageMess ("Reimport of " ++ fullName ++ " successful")
      )

findData :: Maybe FilePath -> IO (Maybe (FilePath,Format))
findData filePathOpt0 =
   do
      filePathOpt <- case filePathOpt0 of
         Just _ -> return filePathOpt0
         Nothing ->
            do
               dialogEvent <- fileDialog "Import Sources" pathRef
               sync dialogEvent
      case filePathOpt of
         Nothing -> return Nothing
         Just filePath ->
            do
               formatOpt <- case splitExtension filePath of
                  Just (_,"tex") -> return (Just LaTeX)
                  Just (_,"xml") -> return (Just XML)
                  Just (_,"omdox") -> return (Just XML)
                  _ ->
                     doForm "Format of file" formatForm
               return (fmap
                  (\ format -> (filePath,format))
                  formatOpt
                  )


-- ------------------------------------------------------------------------
-- Miscellaneous Functions
-- ------------------------------------------------------------------------

lookupMMiSSPackageFolder :: View -> MMiSSPackageFolder -> EntitySearchName 
    -> IO (WithError (Maybe (Link MMiSSPackageFolder)))
lookupMMiSSPackageFolder view packageFolder searchName =
   do
      objectLinkOptWE <- lookupObject view 
         (toMMiSSPackageFolderLinkedObject packageFolder) searchName
      case fromWithError objectLinkOptWE of
         Left mess -> return (hasError (
            "Error looking for MMiSS package folder: " ++ mess))
         Right _ -> return objectLinkOptWE

lookupMMiSSObjectMustExist :: View -> MMiSSPackageFolder -> EntitySearchName 
    -> IO (WithError (Link MMiSSObject))
lookupMMiSSObjectMustExist view packageFolder searchName =
   do
      objectLinkOptWE 
         <- lookupMMiSSObject view packageFolder searchName
      return (mapWithError'
         (\ objectLinkOpt -> case objectLinkOpt of
            Just objectLink -> hasValue objectLink
            Nothing -> hasError
               ("Object " ++ toString searchName ++ " does not exist")
            )
         objectLinkOptWE
         )
