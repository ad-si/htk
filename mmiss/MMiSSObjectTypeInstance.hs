{- This is the module that defines the instance of MMiSSObjectType. -}
module MMiSSObjectTypeInstance() where

import System.IO.Unsafe

import Computation(done)
import AtomString(fromString)
import Sources
import VariableSet(VariableSetSource)
import VariableList
import VariableSetBlocker

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

import MMiSSObjectTypeType
import MMiSSObjectType
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
                     Button "Print or Preview Object"
                        (\ link -> printMMiSSObject view link),
                     Button "Show Preamble Object"
                        (\ link -> openAction link),
                     Button "Hide Preamble Object"
                        (\ link -> closeAction link)
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

