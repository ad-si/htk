{- This module defines an object in MMiSS as part of the ObjectTypes 
   framework.
   -}
module MMiSSObjects(
   registerMMiSSObjects, -- :: IO ()
   initialiseObjectTypes, -- :: View -> IO ()
   ) where

import Maybe
import List

import FiniteMap
import Concurrent
import qualified IOExts(unsafePerformIO)

import XmlTypes
import XmlValidate
import XmlParse

import Dynamics
import Sink
import VariableSet
import Computation
import AtomString
import ExtendedPrelude

import BSem

import DialogWin
import SimpleForm

import CopyFile

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
import Folders

import EmacsEdit

import MMiSSAttributes
import MMiSSPathsSimple
import MMiSSObjectTypeList
import MMiSSVariant
import MMiSSContent
import MMiSSDTD
import MMiSSEditXml

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
         
---
-- Retrieve an object type in a view given its xml tag.
retrieveObjectType :: View -> String -> IO MMiSSObjectType 
retrieveObjectType view xmlTag =
   lookupInGlobalRegistry globalRegistry view (constructKey xmlTag)

---
-- Register MMiSSObjectType.
registerMMiSSObjects :: IO ()
registerMMiSSObjects =
   registerObjectType (error "Unknown MMiSSObjectType" :: MMiSSObjectType)

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
   objectContents :: MMiSSVariantDict (Link Element),
      -- This contains all known variants of this object.
      -- This should have no children.
   includedObjects :: VariableSet EntityName,
      -- Points to objects with True LinkStatus mention in Include's
      -- in the content last created (or null at the beginning).
   referencedObjects :: VariableSet EntityName,
      -- Ditto Reference's.
   linkedObjects :: VariableSet EntityName,
      -- Ditto Link's.
   parentFolder :: Link Folder,
      -- Folder containing this object.  This is also where we search for
      -- other constituent objects.
   editLock :: BSem
      -- Set when we are editing this object.
   }
 
mmissObject_tyRep = mkTyRep "MMiSSObject" "MMiSSObject"
instance HasTyRep MMiSSObject where
   tyRep _ = mmissObject_tyRep

instance HasCodedValue MMiSSObject where
   encodeIO = mapEncodeIO 
      (\ (MMiSSObject {name = name,mmissObjectType = mmissObjectType,
         objectContents = objectContents,parentFolder = parentFolder}) ->
         (name,typeId mmissObjectType,objectContents,parentFolder)
         )
   decodeIO codedValue0 view =
      do
         ((name,tId,objectContents,parentFolder),codedValue1) 
            <- decodeIO codedValue0 view
         mmissObjectType <- lookupInGlobalRegistry globalRegistry view tId
         includedObjects <- newEmptyVariableSet
         referencedObjects <- newEmptyVariableSet
         linkedObjects <- newEmptyVariableSet
         variantAttributes <- newEmptyAttributes view
         mkVariantAttributes variantAttributes
         editLock <- newBSem
         return (MMiSSObject {name = name,mmissObjectType = mmissObjectType,
            variantAttributes = variantAttributes,
            objectContents = objectContents,
            includedObjects = includedObjects,
            referencedObjects = referencedObjects,
            linkedObjects = linkedObjects,
            parentFolder = parentFolder,
            editLock = editLock
            },codedValue1)

-- ------------------------------------------------------------------
-- The instances of HasAttributes and HasParents
-- ------------------------------------------------------------------

instance HasAttributes MMiSSObject where
   readPrimAttributes object = variantAttributes object

instance HasParent MMiSSObject where
   toParent mmissObject = Just (parentFolder mmissObject)

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
      Just (xmlTag objectType,\ view folder 
         -> createMMiSSObject objectType view folder)

   getNodeDisplayData view wrappedDisplayType objectType getDisplayedView
         = return (
      let
         includedArcParms =
            Color "red" $$$
            Thick $$$
            emptyArcTypeParms
         includedArcType = fromString "I"

         referencedArcParms =
            Color "red" $$$
            Dotted $$$
            emptyArcTypeParms
         referencedArcType = fromString "R"

         linkedArcParms =
            Color "red" $$$
            Dashed $$$
            emptyArcTypeParms
         linkedArcType = fromString "L"

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
                  folder = parentFolder mmissObject
               let
                  includedNames :: VariableSetSource EntityName
                  includedNames = SinkSource (includedObjects mmissObject)

                  referencedNames :: VariableSetSource EntityName
                  referencedNames = SinkSource (referencedObjects mmissObject)
 
                  linkedNames :: VariableSetSource EntityName
                  linkedNames = SinkSource (linkedObjects mmissObject)
 
                  arcEntityName :: ArcType -> EntityName 
                     -> IO (Maybe (WrappedLink,ArcType))
                  arcEntityName arcType name = 
                     do
                        linkOpt <- checkLookup 
                           (lookupByObject view mmissObject) name
{-
                     do
                        linkOpt <- getInFolder view folder (toString name)
-}
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
                  
                  linkedLinks ::  VariableSetSource (WrappedLink,ArcType) 
                  linkedLinks = mapVariableSetSourceIO'
                     (arcEntityName linkedArcType)
                     linkedNames

                  allLinks :: VariableSetSource (WrappedLink,ArcType)
                  allLinks =
                     concatVariableSetSource ( 
                        concatVariableSetSource includedLinks 
                           referencedLinks
                        ) linkedLinks
  
               return (allLinks,staticSinkSource [])
      in
         case getNodeTypeParms wrappedDisplayType (displayParms objectType) of
            Nothing -> Nothing
            Just nodeTypeParms ->
               Just (NodeDisplayData {
                  topLinks = [],
                  arcTypes = [
                     (includedArcType,includedArcParms),
                     (referencedArcType,referencedArcParms),
                     (linkedArcType,linkedArcParms)
                     ],
                  nodeTypes = [(theNodeType,newNodeTypeParms nodeTypeParms)],
                  getNodeType = (\ object -> theNodeType),
                  knownSet = SinkSource (knownObjects objectType),
                  mustFocus = (\ _ -> return True),
                  focus = focus,
                  closeDown = done
                  })
      )  

-- ------------------------------------------------------------------
-- Creating or updating an object from an Xml Element.
-- ------------------------------------------------------------------

---
-- Creates or update an object from an Xml Element.
-- If it fails it returns a String.
-- The MMiSSObjectType is the expected type of the object.
-- @param objectType
writeToMMiSSObject :: MMiSSObjectType -> View -> Link Folder -> 
   Maybe String -> Element -> IO (WithError (Link MMiSSObject))
writeToMMiSSObject objectType view folderLink expectedLabel element =
   addFallOutWE (\ break ->
      do
         -- (1) validate it.
         case validateElement (xmlTag objectType) element of
            (s@ (_:_)) -> break (unlines s)
            _ -> done

         -- (2) structure it
         let
            contentsWE = structureContents element

            contents = coerceWithErrorOrBreak break contentsWE

            -- This is the function we use for getting the children of
            -- a StructuredContent object.
            childs object = children (accContents object)

         -- (3) check the object's label
         let
            objectLabel = label contents

         case expectedLabel of
            Nothing -> done
            Just lab -> 
               if lab == objectLabel
                  then
                     done
                  else
                     break ("Expected label is "++lab++" but "++objectLabel++
                        " found")

         -- (4) Get a list of all objects paired with their labels in which
         -- children always occur before their parents.
         let
            (definedObjects :: [(String,StructuredContent)]) =
               treeFold
               (\ () list node -> 
                  ((),(label node,node) : list,childs node)
                  )
               ()
               []
               contents

         -- (5) Check that no new object is defined twice.
         let
            definedObjects2 = 
               sortBy (\ (n1,_) (n2,_) -> compare n1 n2) definedObjects
            split = groupBy (\ (n1,_) (n2,_) -> n1 == n2) definedObjects2
            definedTwice = 
               findJust
                  (\ same -> case same of
                     (n,_):_:_ -> Just n
                     [__] -> Nothing
                     )
                  split

         case definedTwice of
            Nothing -> done
            Just name -> break ("Name "++name++" is multiply defined")

         -- (6) Create map of all defined objects
         let
            definedMap = listToFM definedObjects

         -- (7) Create map to all defined objects already existing in
         --     the repository, also checking their types.
         (alreadyExisting :: [Maybe (String,(Link MMiSSObject,MMiSSObject))]) 
            <- mapM
               (\ (name,structuredContent) -> 
                     do
                        gotMMiSSObject <- getMMiSSObject view folderLink name
                        case gotMMiSSObject of
                           NoObject -> return Nothing
                           OtherObject ->
                              break ("Object you define "++name++
                                 " already exists, but isn't an MMiSS object")
                           Exists link ->
                              do
                                 object <- readLink view link
                                 let
                                    oldTag = xmlTag (mmissObjectType object)
                                    newTag = tag structuredContent
                                 unless (oldTag == newTag)
                                    (break ("Object you define "++name++
                                      "already exists, but with type "++
                                      oldTag++" rather than "++newTag))
                                 return (Just (name,(link,object)))    
                  )
               definedObjects
         let
            alreadyExistingMap 
               :: FiniteMap String (Link MMiSSObject,MMiSSObject)
            alreadyExistingMap = listToFM (catMaybes alreadyExisting)

         -- (8) Verify that all included, linked or referenced objects exist
         mapM_
            (\ (_,contents) ->
               mapM_
                  (\ reference ->
                     do
                        gotMMiSSObject 
                           <- getMMiSSObject view folderLink reference
                        case gotMMiSSObject of
                           NoObject ->
                              if elemFM reference definedMap
                                 then
                                    done
                                 else
                                    break ("Reference "++reference++" is "++
                                       "nowhere defined")
                           OtherObject ->
                              break("Object you reference "++reference++
                                 " already exists, but isn't an MMiSS object")
                           Exists _ -> done
                     )            
                  (includes (accContents contents) ++ 
                     references (accContents contents) ++
                     links (accContents contents))
               )
            definedObjects

         -- (9) For all defined objects verify that they can be found using
         -- the variant attributes inherited from the parent object,
         -- excepting the parent object. 
         treeFoldM
            (\ parentSearchObject () node ->
               do
                  let
                     thisLabel = label node
                     thisSearchObject = toMMiSSSearchObjectFromXml 
                        (attributes node)
                  if objectLabel == thisLabel
                     then 
                        done
                     else
                        case lookupFM alreadyExistingMap thisLabel of
                           Nothing -> done
                           Just (_,object) ->
                              do
                                 canInsert 
                                    <- queryInsert (objectContents object)
                                       parentSearchObject thisSearchObject
                                 if canInsert then done else
                                    break ("Defined object "++thisLabel++
                                      " has incompatible variant attributes")
                  let
                     newSearchObject = mergeMMiSSSearchObjects 
                        parentSearchObject thisSearchObject 
                  return (newSearchObject,(),childs node)
               )
            emptyMMiSSSearchObject
            ()
            contents

         -- (10) (Finally) add all the objects.  We take them in definedObjects
         -- order, since that means no parent is added before its children,
         -- so we never add an undefined reference
         newLinks <- mapM
            (\ (name,structuredContent) ->
               simpleWriteToMMiSSObject break view folderLink
                  (lookupFM alreadyExistingMap name) structuredContent
               )
            definedObjects

         return (last newLinks)
      )

data GotMMiSSObject =
      NoObject -- no object of this name in the folder
   |  OtherObject -- object exists but not with MMiSSObject type
   |  Exists (Link MMiSSObject) -- object exists

---
-- Looks for an MMiSS Object with a given name in a folder
getMMiSSObject :: View -> Link Folder -> String -> IO GotMMiSSObject
getMMiSSObject view folderLink name =
   do
      wrappedLinkOpt <- getInFolder view folderLink name
      case wrappedLinkOpt of
         Nothing -> return NoObject
         Just wrappedLink -> 
            case unpackWrappedLink wrappedLink of
               Nothing -> return OtherObject
               Just link -> return (Exists link)

---
-- Construct or update an MMiSSObject in a folder given its 
-- StructuredContents, assuming all references exist.
-- If a pair (link,MMiSSObject) is supplied this is the existing reference to 
-- the object and its link.
-- We also take a break function argument for indicating an error
simpleWriteToMMiSSObject :: BreakFn -> View -> Link Folder 
   -> Maybe (Link MMiSSObject,MMiSSObject) -> StructuredContent 
   -> IO (Link MMiSSObject)
simpleWriteToMMiSSObject break view folderLink maybeObject structuredContent =
   do
      let
         searchObject = toMMiSSSearchObjectFromXml 
            (attributes structuredContent)
      -- (1) construct and register the actual object, if it doesn't already 
      --     exist, and also get a place to put the new contents in.
      (versioned,objectLink,object) <- case maybeObject of
         Just (objectLink,object) ->
            do
               linkOpt <- variantDictSearchExact (objectContents object) 
                  searchObject
               versioned <- case linkOpt of
                  Just link -> fetchLink view link
                  Nothing -> newEmptyObject view
               -- Set the included and referenced objects.
               setVariableSet (includedObjects object) (map fromString
                  (includes (accContents structuredContent)))
               setVariableSet (referencedObjects object) (map fromString
                  (references (accContents structuredContent)))
               setVariableSet (linkedObjects object) (map fromString
                  (links (accContents structuredContent)))
               return (versioned,objectLink,object)          
         Nothing ->
            do
               mmissObjectType <- retrieveObjectType view 
                  (tag structuredContent)
               variantAttributesWE <- fromXmlAttributes view
                  (extractVariantAttributes (attributes structuredContent))
               objectContents <- newEmptyVariantDict
               includedObjects <- newVariableSet (map fromString
                  (includes (accContents structuredContent)))
               referencedObjects <- newVariableSet (map fromString
                  (references (accContents structuredContent)))
               linkedObjects <- newVariableSet (map fromString
                  (links (accContents structuredContent)))
               editLock <- newBSem
               let
                  name = label structuredContent

                  object = MMiSSObject {
                     name = name,
                     mmissObjectType = mmissObjectType,
                     variantAttributes = coerceWithError variantAttributesWE,
                     objectContents = objectContents,
                     includedObjects = includedObjects,
                     referencedObjects = referencedObjects,
                     linkedObjects = linkedObjects,
                     parentFolder = folderLink,
                     editLock = editLock
                     }
               objectVersioned <- createObject view object
               objectLink <- makeLink view objectVersioned
               success <- insertInFolder view folderLink 
                  (WrappedLink objectLink)
               unless success (break 
                 ("Insertion of object "++name++" has been anticipated"))
                 -- this is unlikely as writeToMMiSSObject has checked this,
                 -- but could happen if someone is trying to update the
                 -- object twice simultaneously.
               versioned <- newEmptyObject view
               return (versioned,objectLink,object)

      -- (2) Construct the new element and insert it in the variant
      --     dictionary
      let
         element = Elem (tag structuredContent) 
            (attributes structuredContent) 
            (contents (accContents structuredContent))

      updateObject view element versioned
      link <- makeLink view versioned
      let
         dict = objectContents object
      
      addToVariantDict dict searchObject link
      return objectLink

-- ------------------------------------------------------------------
-- Creating new MMiSSObjects
-- ------------------------------------------------------------------

createMMiSSObject :: MMiSSObjectType -> View -> Link Folder 
   -> IO (Maybe (Link MMiSSObject,Bool))
createMMiSSObject objectType view folder =
   do
      result <- addFallOut (\ break ->
         do
            let
               createMMiSSForm :: Form String
               createMMiSSForm = newFormEntry "Read Xml from" ""
            detailsOpt <- doForm
               ("Creating new "++xmlTag objectType)
               createMMiSSForm
            let
               xmlPath = case detailsOpt of
                  Nothing -> break "Creation cancelled"
                  Just path -> path
            xmlStringWE <- copyFileToStringCheck xmlPath
            let
               xmlString = coerceWithErrorOrBreak break xmlStringWE

            xmlElementWE <- xmlParseCheck xmlPath xmlString
            let
               xmlElement = coerceWithErrorOrBreak break xmlElementWE

            linkWE <- writeToMMiSSObject objectType view folder Nothing
               xmlElement

            let
               link = coerceWithErrorOrBreak break linkWE
            link `seq` return link
         )
      case result of
         Left str ->
            do
               createMessageWin str []
               return Nothing
         Right link -> return (Just (link,True))
            -- True because the object has already been entered

-- ------------------------------------------------------------------
-- Editing MMiSSObjects
-- ------------------------------------------------------------------

editMMiSSObject :: View -> Link MMiSSObject -> IO ()
editMMiSSObject view link =
   do
      object <- readLink view link
      let
         parent = parentFolder object
         variants = variantAttributes object

         editFS name =
            addFallOutWE (\ break -> 
               do
                  gotObject <- getMMiSSObject view parent name
                  mmissLink <- case gotObject of
                     NoObject -> break ("Object "++name++" does not exist")
                     OtherObject -> break 
                        ("Object "++name++" is not an MMiSS object")
                     Exists mmissLink -> return mmissLink

                  object <- readLink view mmissLink

                  let
                     lock = editLock object
 
                  isAvailable <- tryAcquire lock
                  if isAvailable
                     then
                        done
                     else 
                        break ("Object "++name++" is already being edited")
                  let
                     contents = objectContents object
                  -- For the time being we search using the top objects
                  -- variants, until we think of a better model.
                  searchObject <- toMMiSSSearchObject variants
                  elementLinkOpt <- variantDictSearch contents searchObject
                  elementLink <- case elementLinkOpt of
                     Nothing -> break ("Object "++name++
                        " has no matching variant")
                     Just elementLink -> return elementLink
                  element <- readLink view elementLink
                  let
                     content = toEditableXml name element

                  -- We now have to set up the EditedFile stuff 
                  let
                     writeData emacsContent =
                        addFallOutWE (\ break ->
                          do
                             elementWE <- fromEditableXml name emacsContent
                             let 
                                element = 
                                   coerceWithErrorOrBreak break elementWE
                             element `seq` done
                             linkWE <- writeToMMiSSObject 
                                (mmissObjectType object) view parent
                                (Just name) element
                             let
                                link = coerceWithErrorOrBreak break linkWE
                             link `seq` done
                          )

                     finishEdit = release lock

                     editedFile = EditedFile {
                        writeData = writeData,
                        finishEdit = finishEdit
                        }
                  return (content,editedFile)
               )

         existsFS name =   
            do
               gotObject <- getMMiSSObject view parent name
               return (case gotObject of
                  NoObject -> hasError ("Object "++name++" does not exist")
                  OtherObject -> hasError
                     ("Object "++name++" is not an MMiSS object")
                  Exists mmissLink -> hasValue ()
                  )

         emacsFS = EmacsFS {
            editFS = editFS,
            existsFS = existsFS
            }

      editEmacs emacsFS (name object)

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

      
   

         

   