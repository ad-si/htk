{- This module defines an object in MMiSS as part of the ObjectTypes 
   framework.
   -}
module MMiSSObjects(
   initialiseObjectTypes, -- :: View -> IO ()


   ) where

import Maybe

import FiniteMap
import Concurrent
import qualified IOExts(unsafePerformIO)

import XmlTypes
import XmlValidate

import Dynamics
import Sink
import VariableSet
import Computation
import AtomString

import DialogWin

import GraphDisp
import GraphConfigure
import Graph(NodeType,ArcType)

import CodedValue
import Link
import View
import ObjectTypes
import BasicObjects
import AttributesType
import DisplayParms
import GlobalRegistry
import DisplayView
import EmacsContent

import MMiSSPaths
import MMiSSObjectTypeList
import MMiSSVariant
import MMiSSContent
import MMiSSDTD

-- ------------------------------------------------------------------------
-- The MMiSSObjectType type, and its instance of HasCodedValue and 
--    HasAttributesType
-- ------------------------------------------------------------------------

data MMiSSObjectType = MMiSSObjectType {
   xmlTag :: String, 
      -- Describes the type.  This String should be identical with 
      -- corresponding XML Tag, eg "atom".
   typeId :: GlobalKey,
   attributesType :: AttributesType,
      -- This describes the attributes peculiar to this MMiSS object type.
   displayParms :: NodeTypes (String,Link MMiSSObject),
      -- Displays parameters for this object
   knownObjects :: VariableSet (Link MMiSSObject)
      -- Known elements of this type   
   -- Further information about what can be included in an MMiSSObjectType
   -- will probably be included here. 
   }

mmissObjectType_tyRep = mkTyRep "MMiSSObject" "MMiSSObjectType"
instance HasTyRep MMiSSObjectType where
   tyRep _ = mmissObjectType_tyRep

---
-- Because for now all the information is in MMiSSObjectTypeList,
-- we just represent the type by the xml tag.
instance HasCodedValue MMiSSObjectType where
   encodeIO = mapEncodeIO
      (\ (MMiSSObjectType {xmlTag = xmlTag}) -> xmlTag)
   decodeIO codedValue0 view =
      do
         (xmlTag,codedValue1) <- safeDecodeIO codedValue0 view
         let (Just mmissObjectTypeData) = lookupFM mmissObjectTypeMap xmlTag
         objectType <- createObjectType mmissObjectTypeData
         return (objectType,codedValue1)

instance HasAttributesType MMiSSObjectType where
   toAttributesType objectType = attributesType objectType

-- ------------------------------------------------------------------------
-- Creating object types
-- ------------------------------------------------------------------------

initialiseObjectTypes :: View -> IO ()
initialiseObjectTypes view =
   do
      let
         typeDataList = eltsFM mmissObjectTypeMap
      mapM_
         (\ typeData -> addToGlobalRegistryOpt globalRegistry view 
            (typeId' typeData) (createObjectType typeData))
         typeDataList

createObjectType :: MMiSSObjectTypeData -> IO MMiSSObjectType
createObjectType (MMiSSObjectTypeData {xmlTag' = xmlTag',typeId' = typeId',
      attributesType' = attributesType',
      displayParms' = displayParms'}) = 
   do
      knownObjects <- newEmptyVariableSet
      return (MMiSSObjectType {xmlTag = xmlTag',typeId = typeId',
         attributesType = attributesType',
         displayParms = displayParms',knownObjects = knownObjects})
         
         
-- ------------------------------------------------------------------------
-- The MMiSSObject type, and its instance of HasCodedValue
-- ------------------------------------------------------------------------

data MMiSSObject = MMiSSObject {
   name :: String, -- the user name for this.  NB - although this is
      -- currently fixed, we will probably change this.
   mmissObjectType :: MMiSSObjectType,
   variantAttributes :: Attributes, 
      -- Current variant attributes for this object, IE those according to
      -- which it is opened by default, or those taken from current contents.
      -- The object's path is only stored in attributes.
   objectContents :: MMiSSVariantDict (Link Element),
      -- This contains all known variants of this object.
      -- This should have no children.
   includedObjects :: VariableSet EntityName,
      -- Points to objects with True LinkStatus mention in Include's
      -- in the content last created (or null at the beginning).
   referencedObjects :: VariableSet EntityName
      -- Ditto Reference's.
   }
 
mmissObject_tyRep = mkTyRep "MMiSSObject" "MMiSSObject"
instance HasTyRep MMiSSObject where
   tyRep _ = mmissObject_tyRep

instance HasCodedValue MMiSSObject where
   encodeIO = mapEncodeIO 
      (\ (MMiSSObject {name = name,mmissObjectType = mmissObjectType,
         objectContents = objectContents}) ->
         (name,typeId mmissObjectType,objectContents)
         )
   decodeIO codedValue0 view =
      do
         ((name,tId,objectContents),codedValue1) <- decodeIO codedValue0 view
         mmissObjectType <- lookupInGlobalRegistry globalRegistry view tId
         includedObjects <- newEmptyVariableSet
         referencedObjects <- newEmptyVariableSet
         variantAttributes <- newEmptyAttributes view
         mkVariantAttributes variantAttributes
         return (MMiSSObject {name = name,mmissObjectType = mmissObjectType,
            variantAttributes = variantAttributes,
            objectContents = objectContents,
            includedObjects = includedObjects,
            referencedObjects = referencedObjects},codedValue1)

-- ------------------------------------------------------------------
-- The instance of HasAttributes
-- ------------------------------------------------------------------

instance HasAttributes MMiSSObject where
   readPrimAttributes object = variantAttributes object

-- ------------------------------------------------------------------
-- The instance of ObjectType
-- ------------------------------------------------------------------

instance ObjectType MMiSSObjectType MMiSSObject where
   objectTypeTypeIdPrim _ = "MMiSSObjects"

   objectTypeIdPrim objectType = typeId objectType

   objectTypeGlobalRegistry object = globalRegistry

   getObjectTypePrim object = mmissObjectType object

   nodeTitlePrim object = name object

   createObjectMenuItemPrim objectType = 
      Just (xmlTag objectType,\ view -> createMMiSSObject objectType view)

   getNodeDisplayData view wrappedDisplayType objectType getDisplayedView
         = return (
      let
         includedArcParms =
            Color "red" $$$
            Dotted $$$
            emptyArcTypeParms
         includedArcType = fromString "I"

         referencedArcParms =
            Color "red" $$$
            Dashed $$$
            emptyArcTypeParms
         referencedArcType = fromString "R"

         theNodeType = fromString ""


         focusAction (_,link) =
            do
               displayedView <- getDisplayedView
               focusLink displayedView link

         newNodeTypeParms nodeTypeParms =
            let
               editOptions = [
                  Button "Edit Object"
                     (\ (_,link) -> editMMiSSObject view link),
                  Button "Edit Attributes" 
                     (\ (_,link) -> editObjectAttributes view link),
                  Button "Export Object"
                     (\ (_,link) -> exportMMiSSObject view link)
                  ]
               menu = LocalMenu (Menu Nothing editOptions)
            in
               menu $$$
               ValueTitle (\ (str,_) -> return str) $$$
               (DoubleClickAction focusAction) $$$
               nodeTypeParms

         focus link =
            do
               updateSet (knownObjects objectType) (AddElement link)
               mmissObject <- readLink view link
               let
                  includedNames :: VariableSetSource EntityName
                  includedNames = SinkSource (includedObjects mmissObject)

                  referencedNames :: VariableSetSource EntityName
                  referencedNames = SinkSource (referencedObjects mmissObject)
 
                  arcEntityName :: ArcType -> EntityName 
                     -> IO (Maybe (WrappedLink,ArcType))
                  arcEntityName arcType entityName = 
                     do
                        linkOpt <- checkLookup 
                           (lookupByObject view mmissObject) entityName
                        return (fmap
                           (\ link -> (link,arcType))
                           linkOpt
                           )

                  includedLinks ::  VariableSetSource (WrappedLink,ArcType) 
                  includedLinks = mapVariableSetSourceIO'
                     (arcEntityName includedArcType)
                     includedNames
                  
                  referencedLinks ::  VariableSetSource (WrappedLink,ArcType) 
                  referencedLinks = mapVariableSetSourceIO'
                     (arcEntityName referencedArcType)
                     referencedNames

                  allLinks :: VariableSetSource (WrappedLink,ArcType)
                  allLinks = concatVariableSetSource includedLinks 
                     referencedLinks                  
               return (allLinks,staticSinkSource [])
      in
         case getNodeTypeParms wrappedDisplayType (displayParms objectType) of
            Nothing -> Nothing
            Just nodeTypeParms ->
               Just (NodeDisplayData {
                  topLinks = [],
                  arcTypes = [
                     (includedArcType,includedArcParms),
                     (referencedArcType,referencedArcParms)
                     ],
                  nodeTypes = [(theNodeType,newNodeTypeParms nodeTypeParms)],
                  getNodeType = (\ object -> theNodeType),
                  knownSet = SinkSource (knownObjects objectType),
                  mustFocus = (\ _ -> return False),
                  focus = focus,
                  closeDown = done
                  })
      )  

-- ------------------------------------------------------------------
-- Creating new MMiSSObjects
-- ------------------------------------------------------------------

createMMiSSObject :: MMiSSObjectType -> View -> IO (Maybe (Link MMiSSObject))
createMMiSSObject _ _ = return Nothing -- for now.

-- ------------------------------------------------------------------
-- Editing MMiSSObjects
-- ------------------------------------------------------------------

editMMiSSObject :: View -> Link MMiSSObject -> IO ()
editMMiSSObject view link = done

-- ------------------------------------------------------------------
-- Exporting MMiSS objects
-- ------------------------------------------------------------------

exportMMiSSObject :: View -> Link MMiSSObject -> IO ()
exportMMiSSObject view link = done

-- ------------------------------------------------------------------
-- The global registry and a permanently empty variable set
-- ------------------------------------------------------------------

globalRegistry :: GlobalRegistry MMiSSObjectType
globalRegistry = IOExts.unsafePerformIO createGlobalRegistry
{-# NOINLINE globalRegistry #-}


-- ------------------------------------------------------------------
-- Error messages
-- ------------------------------------------------------------------

errorMess :: String -> IO ()
errorMess mess = createErrorWin mess []

      
   

         

   