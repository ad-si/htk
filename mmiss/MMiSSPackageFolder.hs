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

import System.IO.Unsafe
import Control.Concurrent.MVar
import Data.FiniteMap

import Computation
import ExtendedPrelude
import FileNames
import Dynamics
import Sink
import Sources
import AtomString(fromString,toString)
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

import MMiSSSubFolder
import MMiSSFormat
import MMiSSSplitLink
import MMiSSFileSystemExamples
import MMiSSObjectType hiding (linkedObject)
import MMiSSObjectTypeType hiding (displayParms)
import MMiSSPreamble
import MMiSSImportExportErrors
import MMiSSBundleSimpleUtils
import MMiSSBundleNodeWriteClass
import MMiSSBundleConvert

import {-# SOURCE #-} MMiSSObjectTypeInstance
import {-# SOURCE #-} MMiSSBundleWrite

-- ------------------------------------------------------------------------
-- The MMiSSPackageFolderType type and its instance of HasCodedValue
-- ------------------------------------------------------------------------

newtype MMiSSPackageFolderType = MMiSSPackageFolderType {
   displayParms :: NodeTypes (Link MMiSSPackageFolder)
   }

theMMiSSPackageFolderType = MMiSSPackageFolderType {
   displayParms = readDisplay "white box"
   }

mmissPackageFolderType_tyRep
    = mkTyRep "MMiSSPackageFolder" "MMiSSPackageFolderType"

instance HasTyRep MMiSSPackageFolderType where
   tyRep _ = mmissPackageFolderType_tyRep

instance Monad m => HasBinary MMiSSPackageFolderType m where
   writeBin = mapWrite (\ _ -> ())
   readBin = mapRead (\ () -> theMMiSSPackageFolderType)

-- ------------------------------------------------------------------------
-- The MMiSSPackageFolder type and its instances of Eq, Ord and HasBinary
-- ------------------------------------------------------------------------

data MMiSSPackageFolder = MMiSSPackageFolder {
   linkedObject :: LinkedObject,
   blocker1 :: Blocker WrappedLink,
      -- blocker for link to head package.
   blocker2 :: Blocker WrappedLink,
      -- blocker for package contents.
   blocker3 :: Blocker WrappedLink,
      -- blocker for preamble link.
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
   let
      insertionOptSource :: SimpleSource (Maybe Insertion)
      insertionOptSource = toInsertion (toLinkedObject object)
   in
      mapIO
         (\ insertionOpt -> case insertionOpt of
            Nothing -> return (hasError 
               "MMiSS object somehow not contained anywhere")
            Just insertion -> 
               let
                  (parentLinkedObject,_) = unmkInsertion insertion
                  wrappedLink = toWrappedLink parentLinkedObject
               in
                  case unpackWrappedLink wrappedLink of
                     Nothing ->
                        return (hasError 
                            "MMiSS object somehow not in an MMiSSPackageFolder"
                            )
                     Just packageFolderLink ->
                        do
                           packageFolder <- readLink view packageFolderLink
                           return (hasValue packageFolder)
            )
         insertionOptSource

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
        -- Create blocker for link to head package
        packageLink :: SimpleSource (Maybe WrappedLink)
        packageLink = extractObjectSameName linkedObject

        packageLinks :: SimpleSource [WrappedLink]
        packageLinks  = fmap maybeToList packageLink

        packageLinkSet :: VariableSetSource WrappedLink
        packageLinkSet = listToSetSource packageLinks

     blocker1 
        <- newBlockerWithPreAction packageLinkSet (wrapPreFetchLinks view)

     let
        -- Create blocker for links to contents.
        contentsSet :: VariableSetSource WrappedLink
        contentsSet = objectContents linkedObject

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

     return (MMiSSPackageFolder {
        linkedObject = linkedObject,
        blocker1 = blocker1,
        blocker2 = blocker2,
        blocker3 = blocker3,
        preambleLink = preambleLink
        }, newPostMerge postMergeAct
        )

---
-- Extract from a LinkedObject the wrappedLink for the sub-object with the
-- same name, if any, so for an MMiSSPackageFolder the corresponding package
-- object
extractObjectSameName :: LinkedObject -> SimpleSource (Maybe WrappedLink)
extractObjectSameName linkedObject =
   do
      entityNameOpt <- getLinkedObjectTitleOpt linkedObject
      case entityNameOpt of
         Nothing -> return Nothing
         Just entityName -> lookupObjectContents linkedObject entityName

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
                  delay view (
                     do
                        openBlocker (blocker1 folder) blockID
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
                        closeBlocker (blocker1 folder) blockID
                     )

            menuOptions = [
               Button "Open Package" (\ link -> openAction link),
               Button "Close Package" (\ link -> closeAction link),
               Button "Reimport Package" (\ link 
                  -> reimportMMiSSPackage view link)
               ]

            menu = LocalMenu (Menu Nothing menuOptions)

            getNodeLinks1 link =
               do 
                  folder <- readLink view link
                  arcs1 <- toArcEnds (blocker1 folder) blockID theArcType
                  arcs2 
                     <- toArcEnds (blocker2 folder) blockID theInvisibleArcType
                  arcs3 
                     <- toArcEnds (blocker3 folder) blockID thePreambleArcType
                  let
                     arcs = catVariableLists arcs1 
                        (catVariableLists arcs2 arcs3)

                  return arcs

         case nodeTypeParmsOpt of
            Nothing -> return Nothing
            Just nodeTypeParms0 ->
               let
                  nodeTypeParms1 =
                     menu $$$
                     (valueTitleSource view) $$$
                     (DoubleClickAction openAction) $$$
                     nodeTypeParms0

                  preambleArcTypeParms =
                     Color "blue" $$$
                     emptyArcTypeParms
 

                  nodeDisplayData = NodeDisplayData {
                     topLinks = [],
                     arcTypes = [
                        (theArcType,Double $$$ emptyArcTypeParms),
                        (theInvisibleArcType,invisibleArcTypeParms),
                        (thePreambleArcType,preambleArcTypeParms)],
                     nodeTypes = [(theNodeType,nodeTypeParms1)],
                     getNodeType = const theNodeType,
                     getNodeLinks = getNodeLinks1,
                     closeDown = done,
                     specialNodeActions = const emptyNodeActions
                     }
               in
                  return (Just nodeDisplayData)   

-- ------------------------------------------------------------------------
-- The instance of HasBundleNodeWrite
-- ------------------------------------------------------------------------

instance HasBundleNodeWrite MMiSSPackageFolder where
   bundleNodeWrite1 view bundleNodeLocations thisLocation node 
         packageFolderLink =
      do
         linkedObjectWE <- newLinkedPackageObject view 
            (WrappedLink packageFolderLink) Nothing
         linkedObject <- coerceImportExportIO linkedObjectWE

         let
            preambleLocation = preambleEntityName : thisLocation
            Just bundleNodeExtraData 
               = lookupFM (fm bundleNodeLocations) preambleLocation
            preambleWrappedLink = location bundleNodeExtraData
            Just preambleLink = unpackWrappedLink preambleWrappedLink

         (packageFolder,postMerge) 
            <- createMMiSSPackageFolder view linkedObject preambleLink
         writeLink view packageFolderLink packageFolder
         return (doPostMerge postMerge)

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
   do
      (setSource1 :: VariableSetSource WrappedLink) 
         <- blockVariableSet blocker blockID
 
      let
         variableSet1 :: VariableList WrappedLink
         variableSet1 = newVariableListFromSet setSource1

         variableSet2 :: VariableList (ArcData WrappedLink ArcType)
         variableSet2 = fmap
            (\ wrappedLink -> toArcData wrappedLink arcType True)
            variableSet1

      return variableSet2

theArcType :: ArcType
theArcType = fromString "T"

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

                  bundle <- parseBundle format standardFileSystem filePath
                  writeBundle bundle Nothing (Just dirPath) view 
                     (Right folderLink) 
                  return True
      )

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

                  bundle <- parseBundle format standardFileSystem filePath
                  writeBundle bundle Nothing (Just dirPath) view 
                     (Left (toLinkedObject packageFolder))
      )

findData :: Maybe FilePath -> IO (Maybe (FilePath,Format))
findData filePathOpt0 =
   do
      filePathOpt <- case filePathOpt0 of
         Just _ -> return filePathOpt0
         Nothing ->
            do
               top <- getTOP 
               let
                  fullName = unbreakName [top,"mmiss","test","files"]
               dialogEvent <- fileDialog "Import Sources" fullName
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
