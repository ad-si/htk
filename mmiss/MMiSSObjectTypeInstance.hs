-- | This is the module that defines the ObjectTypes and HasMerging 
-- instance of MMiSSObjectType. 
-- 
-- (Really this should be two modules; it is one because GHC's rules of
-- separating recursive modules would make any alternative too tricky.) 
module MMiSSObjectTypeInstance(
   unpackWrappedLinkToMMiSSObject,
   newEmptyLinkMMiSSObject,
   wrapMMiSSObjectLink, -- :: Link MMiSSObject -> WrappedLink
   linkToLinkedObjectMMiSSObject,
      -- :: View -> Link MMiSSObject -> IO LinkedObject
   lookupMMiSSObject, 
      -- :: View -> MMiSSPackageFolder -> EntitySearchName
      -- -> IO (WithError (Maybe (Link MMiSSObject)))
   ) where

import Maybe

import System.IO.Unsafe

import Computation
import Thread
import ExtendedPrelude
import AtomString(fromString,toString,fromStringWE)
import Sources
import VariableSet(VariableSetSource,toKey)
import VariableList
import VariableSetBlocker
import Dynamics
import CompileFlags

import BSem

import MenuType

import Graph(ArcType,NodeType)
import GraphDisp
import GraphConfigure

import ObjectTypes
import GlobalRegistry
import LinkDrawer
import LinkManager
import ObjectTypes
import Link
import DisplayParms
import SpecialNodeActions
import MergeTypes
import MergePrune
import View
import LocalMenus

import Text.XML.HaXml.Types hiding (MarkupDecl(Element))

import EntityNames

import MMiSSDTDAssumptions
import MMiSSObjectTypeType
import MMiSSObjectType
import MMiSSVariant
import MMiSSVariantObject
import MMiSSPrint
import MMiSSCheck
import MMiSSActiveMath
import MMiSSEditFormatConverter(exportElement)
import MMiSSEditXml(toExportableXml)
import MMiSSFileType
import MMiSSBundle
import MMiSSPackageFolder
import MMiSSBundleSimpleUtils
import MMiSSImportExportErrors

import {-# SOURCE #-} MMiSSEmacsEdit
import {-# SOURCE #-} MMiSSEditAttributes
import {-# SOURCE #-} MMiSSExportLaTeX

-- -------------------------------------------------------------------------
-- The instance
-- -------------------------------------------------------------------------

instance ObjectType MMiSSObjectType MMiSSObject where
   objectTypeTypeIdPrim _ = "MMiSSObjects"

   objectTypeIdPrim objectType = typeId objectType

   objectTypeGlobalRegistry object = globalRegistry

   extraObjectTypes = return allObjectTypes

   getObjectTypePrim object = mmissObjectType object

   nodeTitleSourcePrim object = objectNameSource object

   toLinkedObjectOpt object = Just (toLinkedObject object)

   getNodeDisplayData view wrappedDisplayType objectType getDisplayedView =
      do
         blockID <- newBlockID

         let
            openAction link = 
               do
                  object <- readLink view link
                  openBlocker (extraNodes object) blockID

            closeAction link = 
               do
                  object <- readLink view link
                  closeBlocker (extraNodes object) blockID

            includedArcParms =
               Color "red" $$$
               Head "arrow" $$$
               Solid $$$
               emptyArcTypeParms

            -- The ArcType's are defined by MMiSSObjectType.
            referencedArcType = fromString "R"

            referencedArcParms =
               Color "red" $$$
               Dotted $$$
               Head "arrow" $$$
               emptyArcTypeParms

            linkedArcParms =
               Color "red" $$$
               Dashed $$$
               Head "arrow" $$$
               emptyArcTypeParms

            fileArcParms =
               Thick $$$
               emptyArcTypeParms

            toArcType :: LinkType -> ArcType
            toArcType IncludeLink = includedArcType
            toArcType LinkLink = linkedArcType
            toArcType ReferenceLink = referencedArcType

            theNodeType :: NodeType
            theNodeType = fromString ""

            newNodeTypeParms nodeTypeParms =
               let
                  editOptions = [
                     Button "Edit Object as LaTeX"
                        (\ link -> editMMiSSObjectLaTeX view link),
                     Button "Select Variants" 
                        (\ link -> editObjectAttributes view link),
                     Button "Display Variants"
                        (\ link ->
                           do
                              object <- readLink view link
                              displayObjectVariants (variantObject object)
                           ),
                     Button "Export Object as LaTeX"
                        (\ link -> exportMMiSSObjectLaTeX view link),
                     Button "Export Object as XML"
                        (\ link -> exportMMiSSObjectXML view link),
                     Button "Check consistency"
                        (\ link -> mmissCheck view link),
                     Button "Present in ActiveMath"
		        (\ link -> mmiss2AM view link),
                     Button "Print or Preview Object"
                        (\ link -> printMMiSSObject view link),
                     Button "Delete" (deleteObject view)
                     ] 

                  menu = LocalMenu (Menu Nothing editOptions)

                  borderSourceFn link =
                     do
                        object <- readLink view link
                        let
                           isEditedSource :: SimpleSource Bool
                           isEditedSource = toSimpleSource (
                              isEditedBroadcaster object)

                           borderSource :: SimpleSource Border
                           borderSource = fmap
                              (\ isEdited -> 
                                 if isEdited 
                                    then
                                       DoubleBorder
                                    else
                                       SingleBorder
                                 )
                              isEditedSource
                        return borderSource

               in
                  menu $$$
                  valueTitleSource view $$$
                  fontStyleSource view $$$
                  (BorderSource borderSourceFn) $$
                  nodeTypeParms

            getNodeLinks link =
               do
                  mmissObject <- readLink view link

                  let
                     objectLinks0 :: SimpleSource [(WrappedLink,ArcType)]
                     objectLinks0 =
                        do
                           cache <- toVariantObjectCache (
                              variantObject mmissObject)
                           mkArcEndsSource (cacheLinks cache) toArcType

                     objectLinks1 :: VariableList (WrappedLink,ArcType)
                     objectLinks1 = newVariableListFromList objectLinks0
                     
                     fileLinks0 :: SimpleSource [String]
                     fileLinks0 =
                        do
                           cache <- toVariantObjectCache 
                              (variantObject mmissObject)
                           return (getFiles (cacheElement cache))

                     fileLinks1 :: SimpleSource [EntityFullName]
                     fileLinks1 = fmap
                        (\ nameStrs -> 
                           mapMaybe
                              (\ nameStr -> 
                                 case fromWithError (fromStringWE nameStr) of
                                    Left _ -> Nothing
                                    Right name -> Just name
                                 )
                              nameStrs
                           )
                        fileLinks0

                     packageFolderLinkWE 
                        :: SimpleSource (WithError MMiSSPackageFolder)
                     packageFolderLinkWE 
                        = toMMiSSPackageFolder view mmissObject

                     fileLinks2 :: SimpleSource [(WrappedLink,ArcType)]
                     fileLinks2 =
                        mapIO
                           (\ (fileLinks,packageFolderWE) ->
                              case fromWithError packageFolderWE of
                                 Left mess ->
                                    do
                                       putStrLn ("?? " ++ mess)
                                       return []
                                 Right packageFolder ->
                                    do
                                       let
                                          packageLinkedObject =
                                             toMMiSSPackageFolderLinkedObject
                                                packageFolder

                                       (found :: [[(Link MMiSSFile,
                                             EntityFullName,
                                             String)]])
                                          <- mapM
                                             (\ fileLink ->
                                                findMMiSSFilesInRepository 
                                                   packageLinkedObject 
                                                   fileLink
                                                )
                                             fileLinks
                                       let
                                          allFound1 :: [Link MMiSSFile]
                                          allFound1 = map 
                                             (\ (link,_,_) -> link)
                                             (concat found)

                                          allFound2 :: [(WrappedLink,ArcType)]
                                          allFound2 = map 
                                             (\ link -> 
                                                (WrappedLink link,fileArcType))
                                             (uniqOrd allFound1)

                                       return allFound2
                              )
                           (pairSimpleSources fileLinks1 packageFolderLinkWE)


                     fileLinks3 :: VariableList (WrappedLink,ArcType)
                     fileLinks3 = newVariableListFromList fileLinks2

                     objectFileLinks1 :: VariableList (WrappedLink,ArcType)
                     objectFileLinks1 = catVariableLists
                        objectLinks1 fileLinks3

                     objectFileLinks2 
                        :: VariableList (ArcData WrappedLink ArcType)
                     objectFileLinks2 = fmap
                        (\ (wrappedLink,arcType) ->
                           toArcData wrappedLink arcType False)
                        objectFileLinks1

                  (extraLinks0 :: VariableSetSource
                     (ArcData WrappedLink ArcType))
                     <- blockVariableSet (extraNodes mmissObject) blockID

                  let
                     extraLinks1 :: VariableList (ArcData WrappedLink ArcType)
                     extraLinks1 = newVariableListFromSet extraLinks0

                     allLinks = catVariableLists objectFileLinks2 extraLinks1

                  return allLinks

         return (case getNodeTypeParms wrappedDisplayType 
               (displayParms objectType) of
            Nothing -> Nothing
            Just nodeTypeParms ->
               Just (NodeDisplayData {
                  topLinks = [],
                  arcTypes = [
                     (includedArcType,includedArcParms),
                     (referencedArcType,referencedArcParms),
                     (linkedArcType,linkedArcParms),
                     (fileArcType,fileArcParms)
                     ],
                  nodeTypes = [(theNodeType,newNodeTypeParms nodeTypeParms)],
                  getNodeType = (\ object -> theNodeType),
                  getNodeLinks = getNodeLinks,
                  specialNodeActions= emptySpecialNodeActions
                  })
            )


-- ------------------------------------------------------------------
-- The globalRegistry (currently unused).
-- ------------------------------------------------------------------

globalRegistry :: GlobalRegistry MMiSSObjectType
globalRegistry = System.IO.Unsafe.unsafePerformIO createGlobalRegistry
{-# NOINLINE globalRegistry #-}

-- ------------------------------------------------------------------
-- Merging
-- ------------------------------------------------------------------


instance HasMerging MMiSSObject where

   getMergeLinks = 
      let
         fn :: View -> Link MMiSSObject 
            -> IO (ObjectLinks (MMiSSVariants,CacheContentsMergeKey))
         fn view link =
            do
               object <- readLink view link
               variantObjectObjectLinks
                  (\ variable -> 
                     return (ObjectLinks [
                        (WrappedMergeLink (element variable),Element)])
                     )
                  (variantObject object)
      in
         MergeLinks fn

   attemptMerge = (\ linkReAssigner newView newLink vlos0 ->
      addFallOutWE (\ break ->
         do
            -- (0) Prune the objects list
            vlos1 <- mergePrune vlos0

            -- (1) Get the type of the first object, and check that the
            -- other objects also have this test.
            let
               ((headView,headLink,headObject):vlosRest) = vlos1

               tag = xmlTag . mmissObjectType

               thisTag = tag headObject

            mapM_
               (\ (_,_,object) ->
                  if tag object /= thisTag
                     then
                        do
                           objectTitle <- objectName headObject
                           break ("Type mismatch attempting to merge MMiSS "
                              ++ "object "++ objectTitle)
                     else
                        done
                  )
               vlosRest

            let
               mmissObjectType1 = mmissObjectType headObject

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
               reAssign :: View -> Variable -> IO Variable
               reAssign oldView variable =
                  do
                     let
                        element1 
                           = mapLink linkReAssigner oldView (element variable)
                     editLock1 <- newBSem
                     let
                        variable1 = Variable {
                           element = element1,
                           editLock = editLock1
                           }
                     return variable1

            variantObject1 <- attemptMergeVariantObject
               (converter newView linkedObject1)
               reAssign
               (map (\ (view,_,object) -> (view,variantObject object)) vlos1)

            canClone <- case vlosRest of
               [] ->
                  do
                     linkedsSame <- linkedObjectsSame linkedObject1 
                        (toLinkedObject headObject)
                     if linkedsSame
                        then
                           do
                              variantsSame <- variantObjectsSame
                                 variablesSame
                                 variantObject1 (variantObject headObject)
                              return variantsSame
                         else
                            return False
               _ -> return False 
                            

            if canClone
               then
                  cloneLink headView headLink newView newLink
               else
                  do
                     -- (4) Create the object, and put it in the view.
                     mmissObject1 <- createMMiSSObject 
                        mmissObjectType1 linkedObject1 variantObject1

                     setLink newView mmissObject1 newLink

                     done
         )
      )

   copyObject = Just (\ view0 object0 view1 getNewVersionInfo ->
      do
         let
            variantObject0 = variantObject object0
         variantObject1 <- copyVariantObject getNewVersionInfo variantObject0
         let
            object1 = object0 {variantObject = variantObject1}
         return object1
      )
-- We also need a (trivial) HasMerging instance for Element

instance HasMerging Element where

   getMergeLinks = emptyMergeLinks

   attemptMerge = (\ linkReAssigner newView newLink vlos ->
      do
         vlosPruned <- mergePrune vlos
         case vlosPruned of
            [(oldView,oldLink,element)] ->
               do
                  cloneLink oldView oldLink newView newLink
                  return (hasValue ())
            _ ->
               return (hasError "Unexpected merge required of Element")
      )

-- Key for the sake of merging that produces an intelligible backtrace
-- "Element".  (Now trivial, since the choice of Preamble has been
-- removed.)
data CacheContentsMergeKey = Element deriving (Eq,Ord,Show)

cacheContentsMergeKey_tyRep 
   = mkTyRep "MMiSSObjectTypeInstance" "CacheContentsMergeKey"
instance HasTyRep CacheContentsMergeKey where
   tyRep _ = cacheContentsMergeKey_tyRep

-- -------------------------------------------------------------------------
-- Exporting objects to bundles
-- -------------------------------------------------------------------------

instance HasBundleNodeData MMiSSObject where
   getBundleNodeData view mmissObject exportOpts =
      do
         (allObjectVariants :: [(MMiSSVariantSpec,Variable)])
            <- getAllVariants (variantObject mmissObject)
         (variants :: [(Maybe MMiSSVariantSpec,BundleText)]) <-
            mapMConcurrentExcep
               (\ (variantSpec,variable) ->
                  do
                     bundleText <- if getText exportOpts
                        then
                           do
                              element1 <- readLink view (element variable)
                              exportedStrWE <- exportElement1
                                 view (format exportOpts) False [] element1
                              exportedStr <- coerceImportExportIO exportedStrWE
                              return (BundleString {
                                 contents = fromString exportedStr,
                                 charType = Unicode
                                 })
                        else
                           return NoText
                     return (Just variantSpec,bundleText)
                  )
               allObjectVariants
         return (MMiSSBundle.Object variants)

   getBundleNodeDataForVariant = error 
      ("Don't use getBundleNodeDataForVariant for objects; use "
         ++ "exportMMiSSObjectVariant instead, which also handles ExportFiles")


-- -------------------------------------------------------------------------
-- Functions exported for .hi-boot file.
-- -------------------------------------------------------------------------

unpackWrappedLinkToMMiSSObject :: WrappedLink -> Maybe (Link MMiSSObject)
unpackWrappedLinkToMMiSSObject = unpackWrappedLink

newEmptyLinkMMiSSObject :: View -> IO (Link MMiSSObject)
newEmptyLinkMMiSSObject = newEmptyLink

wrapMMiSSObjectLink :: Link MMiSSObject -> WrappedLink
wrapMMiSSObjectLink = WrappedLink

linkToLinkedObjectMMiSSObject
   :: View -> Link MMiSSObject -> IO LinkedObject
linkToLinkedObjectMMiSSObject view link =
   do
      object <- readLink view link
      return (toLinkedObject object)


lookupMMiSSObject 
   :: View -> MMiSSPackageFolder -> EntitySearchName
   -> IO (WithError (Maybe (Link MMiSSObject)))
lookupMMiSSObject view packageFolder searchName 
   = lookupObject view (toLinkedObject packageFolder) searchName

