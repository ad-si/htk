{- This is the module that defines the ObjectTypes and HasMerging 
   instance of MMiSSObjectType. 

   (Really this should be two modules; it is one because GHC's rules of
   separating recursive modules would make any alternative too tricky.) 
   -}
module MMiSSObjectTypeInstance() where

import System.IO.Unsafe

import Computation
import ExtendedPrelude
import AtomString(fromString)
import Sources
import VariableSet(VariableSetSource)
import VariableList
import VariableSetBlocker

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

import Text.XML.HaXml.Types

import MMiSSObjectTypeType
import MMiSSObjectType
import MMiSSVariant
import MMiSSVariantObject
import MMiSSImportLaTeX
import MMiSSExportLaTeX
import MMiSSContent
import MMiSSPrint

import {-# SOURCE #-} MMiSSEmacsEdit
import {-# SOURCE #-} MMiSSEditAttributes

-- -------------------------------------------------------------------------
-- The instance
-- -------------------------------------------------------------------------

instance ObjectType MMiSSObjectType MMiSSObject where
   objectTypeTypeIdPrim _ = "MMiSSObjects"

   objectTypeIdPrim objectType = typeId objectType

   objectTypeGlobalRegistry object = globalRegistry

   extraObjectTypes = return allObjectTypes

   getObjectTypePrim object = mmissObjectType object

{- Now commented out.  The ONLY way of creating objects is via
   MMiSSPackageFolder.
   createObjectMenuItemPrim objectType = 
      if xmlTag objectType == "package"
         then
            Just (xmlTag objectType,\ view linkedObject
               -> importMMiSSLaTeX objectType view linkedObject)
         else
            Nothing
   -}

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
               Thick $$$
               emptyArcTypeParms

            -- The ArcType's are defined by MMiSSObjectType.
            referencedArcType = fromString "R"

            referencedArcParms =
               Color "red" $$$
               Dotted $$$
               emptyArcTypeParms

            linkedArcParms =
               Color "red" $$$
               Dashed $$$
               emptyArcTypeParms

            toArcType :: LinkType -> ArcType
            toArcType IncludeLink = includedArcType
            toArcType LinkLink = linkedArcType
            toArcType ReferenceLink = referencedArcType

            preambleArcParms =
               Color "blue" $$$
               emptyArcTypeParms

            theNodeType :: NodeType
            theNodeType = fromString ""

            newNodeTypeParms nodeTypeParms =
               let
                  editOptions = [
                     Button "Edit Object as LaTeX"
                        (\ link -> editMMiSSObjectLaTeX view link),
                     Button "Edit Attributes" 
                        (\ link -> editObjectAttributes view link),
                     Button "Export Object as LaTeX"
                        (\ link -> exportMMiSSObjectLaTeX view link),
#ifdef DEBUG
                     Button "Export Object as XML"
                        (\ link -> exportMMiSSObjectXML view link),
#endif
                     Button "Print or Preview Object"
                        (\ link -> printMMiSSObject view link),
                     Button "Show Preamble Object"
                        (\ link -> openAction link),
                     Button "Hide Preamble Object"
                        (\ link -> closeAction link),
                     Button "Delete" (deleteObject view)
                     ]
                  menu = LocalMenu (Menu Nothing editOptions)
               in
                  menu $$$
                  valueTitleSource view $$$
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

                     objectLinks2 :: VariableList (ArcData WrappedLink ArcType)
                     objectLinks2 = fmap
                        (\ (wrappedLink,arcType) ->
                           toArcData wrappedLink arcType False)
                        objectLinks1

                  (extraLinks0 :: VariableSetSource
                     (ArcData WrappedLink ArcType))
                     <- blockVariableSet (extraNodes mmissObject) blockID

                  let
                     extraLinks1 :: VariableList (ArcData WrappedLink ArcType)
                     extraLinks1 = newVariableListFromSet extraLinks0

                     allLinks = catVariableLists objectLinks2 extraLinks1

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
                     (preambleArcType,preambleArcParms)
                     ],
                  nodeTypes = [(theNodeType,newNodeTypeParms nodeTypeParms)],
                  getNodeType = (\ object -> theNodeType),
                  getNodeLinks = getNodeLinks,
                  closeDown = done,
                  specialNodeActions= (\ object
                     -> getNodeActions (nodeActions object)
                     )
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
            -> IO (ObjectLinks (MMiSSVariants,Bool))
         fn view link =
            do
               object <- readLink view link
               variantObjectObjectLinks
                  (\ variable -> 
                     return (ObjectLinks [
                        (WrappedMergeLink (element variable),False),
                        (WrappedMergeLink (preamble variable),True)])
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
               ((_,_,headObject):vlosRest) = vlos1

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
                        preamble1
                           = mapLink linkReAssigner oldView (preamble variable)
                     editLock1 <- newBSem
                     let
                        variable1 = Variable {
                           element = element1,
                           preamble = preamble1,
                           editLock = editLock1
                           }
                     return variable1

            variantObject1 <- attemptMergeVariantObject reAssign
               (map (\ (view,_,object) -> (view,variantObject object)) vlos1)

            -- (4) Create the object, and put it in the view.
            mmissObject1 <- createMMiSSObject 
               mmissObjectType1 linkedObject1 variantObject1

            setLink newView mmissObject1 newLink

            done
         )
      )

-- We also need a (trivial) HasMerging instance for Element

instance HasMerging Element where

   getMergeLinks = emptyMergeLinks

   attemptMerge = (\ linkReAssigner newView newLink viewLinks ->
      case viewLinks of
         [(oldView,oldLink,element)] 
            | oldLink == newLink ->
               do
                  cloneLink oldView newLink newView
                  return (hasValue ())
         _ -> return (hasError "Unexpected merge required of Element")
      )
   