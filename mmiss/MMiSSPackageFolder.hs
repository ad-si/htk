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
      -- -> IO (WithError (MMiSSPackageFolder,EntityName))
   toMMiSSPackageFolderLinkedObject, 
     -- :: MMiSSPackageFolder -> LinkManager.LinkedObject

   lookupMMiSSObject,
      -- :: View -> MMiSSPackageFolder -> EntitySearchName 
      -- -> IO (WithError (Maybe (Link MMiSSObject)))
      -- Look up a particular object, starting from a folder.
   lookupMMiSSObjectMustExist,
      --  :: View -> MMiSSPackageFolder -> EntitySearchName 
      -- -> IO (WithError (Link MMiSSObject))
      -- Like lookupMMiSSObject, but returns an error if the object does not
      -- exist.
   lookupMMiSSPackageFolder,
      -- :: View -> MMiSSPackageFolder -> EntitySearchName 
      -- -> IO (WithError (Maybe (Link MMiSSPackageFolder)))
      -- Get a package-folder from another one by search name.

   importMMiSSPackage1,
      -- :: View -> LinkedObject -> Maybe String
      -- -> IO (Maybe (Link MMiSSPackageFolder))
      -- Import a new MMiSSPackage into a folder designated by a LinkedObject.
      -- The String, if supplied, is the file-path to read it from.

   reimportMMiSSPackage1,
      -- :: View -> Link MMiSSPackageFolder -> Maybe String -> IO ()
      -- Reimport an MMiSSPackage into its existing package folder.
      -- The String, if supplied, is the file-path to read it from.
      

   ) where

import Maybe

import System.IO.Unsafe
import Control.Concurrent.MVar

import Computation
import ExtendedPrelude
import Dynamics
import Sink
import Sources
import AtomString(fromString,toString)
import VariableSet
import VariableSetBlocker
import VariableList
import Delayer(delay)
import Messages

import MenuType

import GraphDisp
import GraphConfigure

import View
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

import MMiSSObjectType hiding (linkedObject)
import MMiSSObjectTypeType hiding (displayParms)
import MMiSSObjectTypeInstance
import MMiSSPreamble
import {-# SOURCE #-} MMiSSImportLaTeX

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
      packageFolderLinkOptWE <- toParentLink (toLinkedObject object)
      case fromWithError packageFolderLinkOptWE of
         Left mess -> return (hasError 
            "MMiSS object somehow not in an MMiSSPackageFolder")
         Right Nothing -> return (hasError 
            "MMiSS object somehow not contained anywhere")
         Right (Just packageFolderLink) ->
            do
               packageFolder <- readLink view packageFolderLink
               return (hasValue packageFolder)

getMMiSSPackageFolderAndName :: HasLinkedObject object
   => View -> object -> IO (WithError (MMiSSPackageFolder,EntityName))
getMMiSSPackageFolderAndName view object =
   do
      insertionOpt <- getCurrentInsertion (toLinkedObject object)
      case insertionOpt of
         Nothing -> return (hasError ("object mysteriously detached"))
         Just insertion ->
            let
               (parentLinkedObject,thisName) = unmkInsertion insertion
               wrappedLink = toWrappedLink parentLinkedObject     
            in
               case unpackWrappedLink wrappedLink of
                  Just (link :: (Link MMiSSPackageFolder)) ->
                     do
                        packageFolder <- readLink view link
                        return (hasValue (packageFolder,thisName))
                  Nothing -> return (hasError ("object not in package folder"))

          
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

importMMiSSPackage :: View -> LinkedObject 
   -> IO (Maybe (Link MMiSSPackageFolder))
importMMiSSPackage view linkedObject 
   = importMMiSSPackage1 view linkedObject Nothing

-- The last argument is the file path to import the file from.  If Nothing,
-- as for importMMiSSPackage, we will prompt the user.
importMMiSSPackage1 :: View -> LinkedObject -> Maybe String
   -> IO (Maybe (Link MMiSSPackageFolder))
importMMiSSPackage1 view parentLinkedObject filePathOpt =
   do
      -- We create the LinkedObject first of all, but without putting 
      -- anything in it, or putting it into anything, enabling us to 
      -- tie the knot.  However we do at least make sure that the link
      -- points to a complete MMiSSPackage. 
      --
      -- We also create a link for the preamble.
      -- Both links will get deleted if we fail.
      (link :: Link MMiSSPackageFolder) <- newEmptyLink view
      (preambleLink1 :: Link MMiSSPreamble) <- newEmptyLink view
     
      let
         error1 =
            do
               deleteLink view link
               deleteLink view preambleLink1
               return Nothing

      linkedObjectWE <- newLinkedPackageObject view (WrappedLink link) Nothing
      case fromWithError linkedObjectWE of
         Left mess -> 
            do
               errorMess mess
               error1
         Right linkedObject ->
--            (delay view) .
            (synchronizeView view) $ (
            do
               let
                  error2 =
                     do
                        moveObject linkedObject Nothing
                        error1
               resultWE <- addFallOutWE (\ break ->
                  do
                     (packageFolder,postMerge) <- createMMiSSPackageFolder 
                        view linkedObject preambleLink1
                     writeLink view link packageFolder

                     let
                        -- This is the function passed to importMMiSSLaTeX
                        getLinkedObject :: EntityName 
                           -> IO (WithError MMiSSPackageFolder)
                        getLinkedObject entityName =
                           do
                              successWE <- moveObject linkedObject (Just 
                                 (mkInsertion parentLinkedObject entityName))
                              return (mapWithError 
                                 (\ () -> packageFolder) successWE)

                        packageType = retrieveObjectType "package"

                     result <- importMMiSSLaTeX preambleLink1 packageType view 
                        getLinkedObject filePathOpt

                     if isJust result
                        then
                           doPostMerge postMerge
                        else
                           done

                     return result
                  )          

               case fromWithError resultWE of
                  Left mess -> 
                     do
                        errorMess mess
                        error2
                  Right Nothing -> error2
                  Right result -> return (Just link)
            )

-- Import a new version into an existing MMiSS package.
-- This function is implemented like a (very) stripped-down version of
-- importMMiSSPackage.
reimportMMiSSPackage :: View -> Link MMiSSPackageFolder -> IO ()
reimportMMiSSPackage view packageFolderLink = 
   reimportMMiSSPackage1 view packageFolderLink Nothing

reimportMMiSSPackage1 :: View -> Link MMiSSPackageFolder -> Maybe String 
   -> IO ()
reimportMMiSSPackage1 view packageFolderLink filePathOpt =
   do
      resultWE <- addFallOutWE (\ break ->
         do
            packageFolder <- readLink view packageFolderLink
            
            (nameOpt :: Maybe EntityName) 
               <- readContents (getLinkedObjectTitleOpt 
                  (toLinkedObject packageFolder))
            name <- case nameOpt of
               Nothing -> break 
                  "Can't import to package which hasn't been inserted"
               Just name -> return name
            let
               -- function for importMMiSSLaTeX
               getLinkedObject :: EntityName 
                  -> IO (WithError MMiSSPackageFolder)
               getLinkedObject name1 =
                  return (if name == name1 
                     then
                        hasValue packageFolder
                     else
                        hasError ("File contains wrong package: "
                           ++ toString name1 ++ ", not "
                           ++ toString name)
                     )

               packageType = retrieveObjectType "package"

            result <- importMMiSSLaTeX (preambleLink packageFolder)
               packageType view getLinkedObject filePathOpt
            -- don't care if errors, as any errors will already have been
            -- shown.
            done
         )

      case fromWithError resultWE of
         Right () -> done
         Left mess -> errorMess mess

-- ------------------------------------------------------------------------
-- Miscellaneous Functions
-- ------------------------------------------------------------------------

lookupMMiSSObject :: View -> MMiSSPackageFolder -> EntitySearchName 
    -> IO (WithError (Maybe (Link MMiSSObject)))
lookupMMiSSObject view packageFolder searchName =
   do
      objectLinkOptWE <- lookupObject view 
         (toMMiSSPackageFolderLinkedObject packageFolder) searchName
      case fromWithError objectLinkOptWE of
         Left mess -> return (hasError (
            "Error looking for MMiSS object: " ++ mess))
         Right _ -> return objectLinkOptWE

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
      objectLinkOptWE <- lookupMMiSSObject view packageFolder searchName
      return (mapWithError'
         (\ objectLinkOpt -> case objectLinkOpt of
            Just objectLink -> hasValue objectLink
            Nothing -> hasError
               ("Object " ++ toString searchName ++ " does not exist")
            )
         objectLinkOptWE
         )