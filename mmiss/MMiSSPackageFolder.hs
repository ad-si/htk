{- This module defines the MMiSSPackageFolder type.  An MMiSSPackageFolder
   is a special sort of folder containing exactly one package.
   -}
module MMiSSPackageFolder(
   MMiSSPackageFolder,
   MMiSSPackageFolderType,
   ) where

import Maybe

import System.IO.Unsafe
import Control.Concurrent.MVar

import Computation
import ExtendedPrelude
import Dynamics
import Sources
import AtomString(fromString,toString)
import VariableSet
import VariableSetBlocker
import VariableList
import Delayer(delay)

import MenuType
import DialogWin

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
import MMiSSImportLaTeX
import MMiSSObjectTypeInstance

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

instance HasCodedValue MMiSSPackageFolderType where
   encodeIO = mapEncodeIO (\ _ -> ())
   decodeIO = mapDecodeIO (\ () -> theMMiSSPackageFolderType)

-- ------------------------------------------------------------------------
-- The MMiSSPackageFolder type and its instance of HasCodedValue
-- ------------------------------------------------------------------------

data MMiSSPackageFolder = MMiSSPackageFolder {
   linkedObject :: LinkedObject,
   blocker1 :: Blocker WrappedLink,
      -- blocker for link to head package.
   blocker2 :: Blocker WrappedLink
      -- blocker for package contents.
   }

mmissPackageFolder_tyRep = mkTyRep "MMiSSPackageFolder" "MMiSSPackageFolder"

instance HasTyRep MMiSSPackageFolder where
   tyRep _ = mmissPackageFolder_tyRep

instance HasCodedValue MMiSSPackageFolder where
   encodeIO = mapEncodeIO (\ 
      (MMiSSPackageFolder {linkedObject = linkedObject})
         -> linkedObject
      )
   decodeIO codedValue0 view =
      do
         (linkedObject,codedValue1) <- decodeIO codedValue0 view
         mmissPackageFolder <- createMMiSSPackageFolder view linkedObject
         return (mmissPackageFolder,codedValue1)

instance HasLinkedObject MMiSSPackageFolder where
   toLinkedObject mmissPackageFolder = linkedObject mmissPackageFolder

-- ------------------------------------------------------------------------
-- Constructing the MMiSSPackageFolder
-- ------------------------------------------------------------------------

createMMiSSPackageFolder :: View -> LinkedObject -> IO MMiSSPackageFolder
createMMiSSPackageFolder view linkedObject =
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

     return (MMiSSPackageFolder {
        linkedObject = linkedObject,
        blocker1 = blocker1,
        blocker2 = blocker2
        })

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

   getMergeLinks = getLinkedObjectMergeLinks

   attemptMerge linkReAssigner newView newLink vlos =
      addFallOutWE (\ break ->
         do
            -- compare with similar code in Folders.  But this is
            -- simple as we don't have attributes or more than one
            -- type.
            (vlosPruned @ ((view1,link1,folder1) : _)) <- mergePrune vlos

            newLinkedObjectWE <- attemptLinkedObjectMerge
               linkReAssigner newView newLink
                  (map 
                     (\ (view,link,folder) -> (view,toLinkedObject folder))
                     vlos
                     )

            newLinkedObject <- coerceWithErrorOrBreakIO break newLinkedObjectWE

            isSame 
               <- linkedObjectsSame newLinkedObject (toLinkedObject folder1)
            if isSame 
               then
                  cloneLink view1 link1 newView newLink
               else
                  do
                     mmissPackageFolder 
                        <- createMMiSSPackageFolder newView newLinkedObject
                     setLink newView mmissPackageFolder newLink
                     done
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
            (fromString "Undefined MMiSSPackageFolder")
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
                     )

            closeAction link =
               do
                  folder <- readLink view link
                  delay view (
                     do
                        closeBlocker (blocker2 folder) blockID
                        closeBlocker (blocker1 folder) blockID
                     )

            menuOptions = [
               Button "Open Package" (\ link -> openAction link),
               Button "Close Package" (\ link -> closeAction link)
               ]

            menu = LocalMenu (Menu Nothing menuOptions)

            getNodeLinks1 link =
               do 
                  folder <- readLink view link
                  arcs1 <- toArcEnds (blocker1 folder) blockID theArcType
                  arcs2 
                     <- toArcEnds (blocker2 folder) blockID theInvisibleArcType

                  let
                     arcs = catVariableLists arcs1 arcs2

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

                  nodeDisplayData = NodeDisplayData {
                     topLinks = [],
                     arcTypes = [
                        (theArcType,Double $$$ emptyArcTypeParms),
                        (theInvisibleArcType,invisibleArcTypeParms)],
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

-- ------------------------------------------------------------------------
-- Importing a new package
-- ------------------------------------------------------------------------

importMMiSSPackage :: View -> LinkedObject 
   -> IO (Maybe (Link MMiSSPackageFolder))
importMMiSSPackage view parentLinkedObject =
   do
      -- We create the LinkedObject first of all, but without putting 
      -- anything in it, or putting it into anything, enabling us to 
      -- tie the knot.  However we do at least make sure that the link
      -- points to a complete MMiSSPackage.  In the event of failure we will
      -- delete this link.
      (link :: Link MMiSSPackageFolder) <- newEmptyLink view
      let
         error1 =
            do
               deleteLink view link
               return Nothing

      linkedObjectWE <- newLinkedObject view (WrappedLink link) Nothing
      case fromWithError linkedObjectWE of
         Left mess -> 
            do
               createErrorWin mess []
               error1
         Right linkedObject ->
            (delay view) .
            (synchronizeView view) $ (
            do
               let
                  error2 =
                     do
                        moveObject linkedObject Nothing
                        error1
               resultWE <- addFallOutWE (\ break ->
                  do
                     mmissPackageFolder 
                        <- createMMiSSPackageFolder view linkedObject
                     writeLink view link mmissPackageFolder

                     let
                        -- This is the function passed to importMMiSSLaTeX
                        getLinkedObject :: EntityName 
                           -> IO (WithError LinkedObject)
                        getLinkedObject entityName =
                           do
                              successWE <- moveObject linkedObject (Just 
                                 (mkInsertion parentLinkedObject entityName))
                              return (mapWithError 
                                 (\ () -> linkedObject) successWE)

                        packageType = retrieveObjectType "package"

                     importMMiSSLaTeX packageType view getLinkedObject
                  )          

               case fromWithError resultWE of
                  Left mess -> 
                     do
                        createErrorWin mess []
                        error2
                  Right Nothing -> error2
                  Right result -> return (Just link)
            )
