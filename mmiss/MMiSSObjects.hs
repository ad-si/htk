{- This module defines an object in MMiSS as part of the ObjectTypes 
   framework.
   -}
module MMiSSObjects(
   registerMMiSSObjects, -- :: IO ()
   initialiseObjectTypes, -- :: View -> IO ()

   mkLaTeXString, -- :: EmacsContent TypedName -> String
   -- mkLaTeXString is exported solely for testing purposes.
   ) where

#include "config.h"

import Maybe
import List

import FiniteMap
import Concurrent
import qualified IOExts(unsafePerformIO)

import Dynamics
import Sink
import Source
import VariableSet
import Computation
import AtomString
import ExtendedPrelude
import Debug
import qualified Exception
import Registry

import BSem

import DialogWin
import SimpleForm

import WBFiles (getTOP)
import FileDialog
import Events

import CopyFile

import GraphDisp
import GraphConfigure
import Graph(NodeType,ArcType)

#if HAXMLINT
import Text.XML.HaXml.Types
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Validate
#else
import XmlParse
import XmlTypes

import XmlValidate
#endif

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
import SpecialNodeActions

import EmacsEdit
import EmacsContent

import LaTeXParser

import MMiSSDTDAssumptions(getMiniType,toIncludeStr)
import MMiSSAttributes
import MMiSSPathsSimple
import MMiSSObjectTypeList
import MMiSSVariant
import MMiSSContent
import MMiSSDTD
import MMiSSEditXml
import MMiSSReAssemble
import MMiSSLaTeX

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
   editLock :: BSem,
      -- Set when we are editing this object.
   nodeActions :: NodeActionSource
   }
 
mmissObject_tyRep = mkTyRep "MMiSSObject" "MMiSSObject"
instance HasTyRep MMiSSObject where
   tyRep _ = mmissObject_tyRep

instance HasCodedValue MMiSSObject where
   encodeIO = 
      -- We put some magic here to stop the MMiSS object's text display being
      -- italicised when we commit.  This assumes that encodeIO only happens
      -- on a commit.
      let
         encodeIO0 =
            mapEncodeIO 
               (\ (MMiSSObject {name = name,
                  mmissObjectType = mmissObjectType,
                  objectContents = objectContents,
                  parentFolder = parentFolder}) ->
                  (name,typeId mmissObjectType,objectContents,parentFolder)
                  )
            
         encodeIO1 mmissObject codedValue view =
            do
               setFontStyle (nodeActions mmissObject) def
               encodeIO0 mmissObject codedValue view
      in
         encodeIO1
              
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
         nodeActions <- newNodeActionSource
         return (MMiSSObject {name = name,mmissObjectType = mmissObjectType,
            variantAttributes = variantAttributes,
            objectContents = objectContents,
            includedObjects = includedObjects,
            referencedObjects = referencedObjects,
            linkedObjects = linkedObjects,
            parentFolder = parentFolder,
            editLock = editLock,
            nodeActions = nodeActions
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
                  Button "Edit Object as LaTeX"
                     (\ (_,link) -> editMMiSSObjectLaTeX view link),
                  Button "Edit Object as XML"
                     (\ (_,link) -> editMMiSSObjectXml view link),
                  Button "Edit Attributes" 
                     (\ (_,link) -> editObjectAttributes view link),
                  Button "Export Object To File"
                     (\ (_,link) -> exportMMiSSObject view link),
                  Button "Print or Preview Object"
                     (\ (_,link) -> printMMiSSObject view link)
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
                  closeDown = done,
                  specialNodeActions= (\ object
                     -> getNodeActions (nodeActions object)
                     )
                  })
      )  

-- ------------------------------------------------------------------
-- Creating or updating an object from an Xml Element.
-- ------------------------------------------------------------------

---
-- Creates or update an object from an Xml Element.
-- If it fails it returns a String.
-- The MMiSSObjectType is the expected type of the object.
--
-- If the bool checkThisEditLock is set we insist on getting the edit lock
-- of the top object, if it already exists.  In any case we get the edit lock
-- of all included objects.
writeToMMiSSObject :: MMiSSObjectType -> View -> Link Folder -> 
   Maybe String -> Element -> Bool -> IO (WithError (Link MMiSSObject))
writeToMMiSSObject objectType view folderLink expectedLabel element 
   checkThisEditLock =

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

         -- (10) Attempt to grab the editLock for every object that is already
         --     defined, unless (if checkThisEditLock) it is the top object.
         let
            objectsToCheck0 = 
               (map
                  (\ (_,mmissObject) -> mmissObject)
                  (eltsFM alreadyExistingMap)
                  )

            objectsToCheck1 =
               if checkThisEditLock
                  then
                     objectsToCheck0
                  else
                     filter 
                        (\ object -> objectLabel /= name object)
                        objectsToCheck0

         releaseActWE <- tryAcquireBSemsWithError
            editLock
            (\ mmissObject -> "Cannot write to "++name mmissObject
               ++" as it is already being edited")
            objectsToCheck1

         let
            releaseAct = coerceWithErrorOrBreak break releaseActWE
         seq releaseAct done

         -- (11) Add all the objects.  We take them in definedObjects
         -- order, since that means no parent is added before its children,
         -- so we never add an undefined reference
         newLinks <- 
            Exception.finally (
               mapM
                  (\ (name,structuredContent) ->
                     simpleWriteToMMiSSObject break view folderLink
                        (lookupFM alreadyExistingMap name) structuredContent
                     )
                  definedObjects
               )
               -- Release all the edit locks
              releaseAct
         
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
               nodeActions <- newNodeActionSource
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
                     editLock = editLock,
                     nodeActions = nodeActions
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
{-
            let
               createMMiSSForm :: Form (Format,String)
               createMMiSSForm =
                  fmap 
                     (\ ((),details) -> details)
                     (nullForm "Read" 
                        SimpleForm.\\ 
                        (formatForm 
                           SimpleForm.\\ 
                        newFormEntry "from" ""
                        ))
            detailsOpt <- doForm
               ("Creating new "++xmlTag objectType)
               createMMiSSForm
-}
	    top <- getTOP >>= return . (++ "/mmiss/test/files")
	    fpc <- fileDialog "Import LaTeX sources." top >>= sync
	    
            let
               (format,filePath) = case fpc of
                  Nothing -> break "Creation cancelled"
                  Just fp -> (LaTeX, fp)

            inputStringWE <- copyFileToStringCheck filePath
            let
               inputString = coerceWithErrorOrBreak break inputStringWE

            xmlElementWE <- 
               case format of
                  XML ->  xmlParseCheck filePath inputString
                  LaTeX -> return (parseMMiSSLatex Nothing inputString)
            let
               xmlElement = coerceWithErrorOrBreak break xmlElementWE

            linkWE <- writeToMMiSSObject objectType view folder Nothing
               xmlElement True

            let
               link = coerceWithErrorOrBreak break linkWE
            link `seq` return link
         )
      case result of
         Left str ->
            do
               createMessageWin str []
               return Nothing
         Right link -> 
            do
               createMessageWin "Import successful!" [] 
               return (Just (link,True))
            -- True because the object has already been entered

-- ------------------------------------------------------------------
-- Editing MMiSSObjects
-- ------------------------------------------------------------------


editMMiSSObjectXml :: View -> Link MMiSSObject -> IO ()
editMMiSSObjectXml = editMMiSSObjectGeneral XML

editMMiSSObjectLaTeX :: View -> Link MMiSSObject -> IO ()
editMMiSSObjectLaTeX = editMMiSSObjectGeneral LaTeX

editMMiSSObjectGeneral :: Format -> View -> Link MMiSSObject -> IO ()
editMMiSSObjectGeneral format
    = editMMiSSObjectInner (toEditFormatConverter format)

editMMiSSObjectInner :: WrappedEditFormatConverter -> View -> Link MMiSSObject
      -> IO ()
editMMiSSObjectInner 
      (WrappedEditFormatConverter (
         (EditFormatConverter {toEdit = toEdit,fromEdit = fromEdit}
            :: EditFormatConverter formatExtra)
         ))
      view link =
   do
      object <- readLink view link

      -- We store the formatExtra information so it can be got at during
      -- printing.
      (formatExtraStash :: Registry String formatExtra) <- newRegistry
 
      let
         parent = parentFolder object
         variants = variantAttributes object

         editFS (name,miniType) =
            addFallOutWE (\ break -> 
               do
                  gotObject <- getMMiSSObject view parent name
                  mmissLink <- case gotObject of
                     NoObject -> break ("Object "++name++" does not exist")
                     OtherObject -> break 
                        ("Object "++name++" is not an MMiSS object")
                     Exists mmissLink -> return mmissLink
                  
                  object <- readLink view mmissLink
                  if getObjectMiniType object == miniType
                     then
                        done
                     else
                        break ("Object "++name++" has wrong type")
                  let
                     lock = editLock object
 
                  isAvailable <- tryAcquire lock
                  if isAvailable
                     then
                        done
                     else 
                        break ("Object "++name++" is already being edited")
                  setBorder (nodeActions object) DoubleBorder

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
                     contentWE = toEdit name element
                     (content,formatExtra) 
                        = coerceWithErrorOrBreak break contentWE

                  setValue formatExtraStash name formatExtra

                  -- We now have to set up the EditedFile stuff 
                  let
                     writeData emacsContent =
                        addFallOutWE (\ break ->
                          do
                             elementWE 
                                <- fromEdit formatExtra name emacsContent
                             let 
                                element = 
                                   coerceWithErrorOrBreak break elementWE
                             element `seq` done
                             linkWE <- writeToMMiSSObject 
                                (mmissObjectType object) view parent
                                (Just name) element False
                             let
                                link = coerceWithErrorOrBreak break linkWE
                             link `seq` done
                             setFontStyle (nodeActions object) 
                                BoldItalicFontStyle
                             createMessageWin 
                                ("Commit of "++name++ " successful!") []
                          )

                     finishEdit =
                        do
                           release lock
                           setBorder (nodeActions object) def
                           deleteFromRegistry formatExtraStash name

                     editedFile = EditedFile {
                        writeData = writeData,
                        finishEdit = finishEdit
                        }
                  return (content,editedFile)
               )

         existsFS (name,miniType) =   
            do
               gotObject <- getMMiSSObject view parent name
               case gotObject of
                  NoObject -> return (hasError 
                     ("Object "++name++" does not exist"))
                  OtherObject -> return (hasError
                     ("Object "++name++" is not an MMiSS object"))
                  Exists mmissLink -> 
                     do
                        object <- readLink view mmissLink
                        if getObjectMiniType object == miniType
                           then
                              return (hasValue ())
                           else
                              return (hasError 
                                 ("Object "++name++" has wrong type"))

         emacsFS = EmacsFS {
            editFS = editFS,
            existsFS = existsFS
            }

         printAction topStr getContent =
            do
               let
                  -- Untidily, we pass down the information about whether to
                  -- expand particular children as a list of MVars.
                  reAssembleArg :: EntityName -> MVar [Bool] 
                     -> IO (WithError (Maybe (Element,MVar [Bool])))
                  reAssembleArg entityName mVar =
                     do
                        (doExpand:rest) <- takeMVar mVar
                        putMVar mVar rest
                        if doExpand
                           then
                              addFallOutWE (\ break ->
                                 do
                                    let
                                       name = toString entityName
                                    content0WE <- getContent name
                                    let
                                       content0 = coerceWithErrorOrBreak break 
                                          content0WE
                                    seq content0 done
                                    let
                                       content1 = fmap 
                                          (\ (b,typedName) -> typedName) 
                                          content0

                                       (EmacsContent dataItems) = content0
                                       expandList = mapMaybe
                                          (\ dataItem -> case dataItem of
                                             EmacsLink (b,typedName) -> Just b
                                             _ -> Nothing
                                             )
                                          dataItems

                                    (Just formatExtra) <- getValueOpt
                                       formatExtraStash name
                                    elementWE
                                       <- fromEdit formatExtra name content1
                                    let
                                       element = coerceWithErrorOrBreak break 
                                          elementWE
                                    seq element done

                                    nextMVar <- newMVar expandList   
                                    return (Just (element,nextMVar))
                                 )                                  
                           else
                              return (hasValue Nothing)

               topMVar <- newMVar [True]
               elementWE 
                  <- reAssemble reAssembleArg (fromString topStr) topMVar
               case fromWithError elementWE of
                  Left error -> createErrorWin error []
                  Right element ->
                     do
                        -- We do the actual printing in a separate thread,
                        -- so the user can continue editing.
                        forkIO (
                           do
                              let
                                 stringWE = exportElement LaTeX element
                              case fromWithError stringWE of
                                 Left error -> createErrorWin error []
                                 Right str -> mmissLaTeX topStr str
                           )
                        done

      editEmacs emacsFS (PrintAction printAction) 
         (name object,getObjectMiniType object)

getObjectMiniType :: MMiSSObject -> Char
getObjectMiniType object = getMiniType (xmlTag (mmissObjectType object))

-- ------------------------------------------------------------------
-- Exporting MMiSS objects
-- ------------------------------------------------------------------

exportMMiSSObject :: View -> Link MMiSSObject -> IO ()
exportMMiSSObject view link =
   do
      result <- addFallOut (\ break ->
         do
            object <- readLink view link
{-
            let
               exportMMiSSForm :: Form (Format,String)
               exportMMiSSForm =
                  fmap 
                     (\ ((),details) -> details)
                     (nullForm "Write" 
                        SimpleForm.\\ 
                        (formatForm 
                           SimpleForm.\\ 
                        newFormEntry "to" ""
                        ))
            detailsOpt <- doForm
               ("Exporting "++name object)
               exportMMiSSForm 
-}
            top <- getTOP >>= return . (++ "/mmiss/test/files")
	    fpc <- fileDialog "Export LaTeX sources." top >>= sync
	    
            let (format,filePath) = case fpc of
                  Nothing -> break "Export cancelled"
                  Just fp -> (LaTeX, fp)

               -- getElement is the function to be passed to
               -- MMiSSReAssemble.reAssembleNoRecursion.

            stringWE <- extractMMiSSObject view link format
            let
               string = coerceWithErrorOrBreak break stringWE

            -- Write to the file
            resultWE <- copyStringToFileCheck string filePath
            seq (coerceWithErrorOrBreak break resultWE) done
         )

      case result of
         Right () -> done
         Left mess -> createErrorWin mess []

-- ------------------------------------------------------------------
-- Sending an MMiSS object through LaTeX, and then doing things with the
-- DVI file. 
-- ------------------------------------------------------------------

printMMiSSObject :: View -> Link MMiSSObject -> IO ()
printMMiSSObject view link =
   do
      result <- addFallOut (\ break ->
         do
            stringWE <- extractMMiSSObject view link LaTeX
            let
               string = coerceWithErrorOrBreak break stringWE
            seq string done

            object <- readLink view link

            mmissLaTeX (name object) string
         )
      case result of
         Right () -> done
         Left mess -> createErrorWin mess []
        
-- ------------------------------------------------------------------
-- Extracting an object in a given format.
-- ------------------------------------------------------------------

extractMMiSSObject :: View -> Link MMiSSObject -> Format 
   -> IO (WithError String)
extractMMiSSObject view link format =
   addFallOutWE (\ break ->
      do
         object <- readLink view link

         let
            -- getElement is the function to be passed to
            -- MMiSSReAssemble.reAssembleNoRecursion.
            getElement :: EntityName -> MMiSSSearchObject
               -> IO (WithError (Maybe (Element,MMiSSSearchObject)))
            getElement entityName searchObject = 
               addFallOutWE (\ break ->
                  do
                     wrappedLinkWE
                        <- lookupByObjectWithError view object entityName
                     let
                        wrappedLink 
                           = coerceWithErrorOrBreak break wrappedLinkWE
                        link = case unpackWrappedLink wrappedLink of
                           Nothing -> break ("Object "++toString entityName
                              ++ " is not an MMiSS object")
                           Just link -> link
                     object <- readLink view link
                     elementLinkOpt <- variantDictSearch 
                        (objectContents object) searchObject
                     let
                        elementLink = case elementLinkOpt of
                           Nothing -> break ("Object "++toString entityName
                              ++ " has no version with matching attributes")
                           Just elementLink -> elementLink
                     (element @ (Elem _ attributes _)) 
                        <- readLink view elementLink
                     let
                        searchObject2 =
                           mergeMMiSSSearchObjects
                              searchObject 
                              (toMMiSSSearchObjectFromXml attributes) 
                     return (Just (element,searchObject2))
                  )

         initialSearchObject 
            <- toMMiSSSearchObject (variantAttributes object)

         completeElementWE <- reAssembleNoRecursion getElement 
            (fromString (name object)) initialSearchObject

         let
            completeElement = coerceWithErrorOrBreak break completeElementWE

            stringWE = exportElement format completeElement
            string = coerceWithErrorOrBreak break stringWE

         -- Catch any errors in the string, then return it.
         seq string (return string)
      )
          
-- ------------------------------------------------------------------
-- Selecting the format of files
-- ------------------------------------------------------------------

data Format = LaTeX | XML

formatForm :: Form Format
formatForm = newFormOptionMenu2 [("LaTeX",LaTeX),("XML",XML)]


-- ------------------------------------------------------------------
-- Turning a Format into conversion functions for editing
-- ------------------------------------------------------------------

---
-- For EditFormatConvert, the String's are the file name (made available
-- for error messages).
--
-- The extra data formatExtra is available so whoever is managing the format
-- can stash extra information (in particular, the LaTeX preamble).
data EditFormatConverter formatExtra = EditFormatConverter {
   toEdit :: String -> Element 
      -> WithError (EmacsContent TypedName,formatExtra),
   fromEdit :: formatExtra -> String -> EmacsContent TypedName 
      -> IO (WithError Element)
   }

data WrappedEditFormatConverter = forall formatExtra .
   WrappedEditFormatConverter (EditFormatConverter formatExtra)

toEditFormatConverter :: Format -> WrappedEditFormatConverter
toEditFormatConverter XML = 
   WrappedEditFormatConverter (
      EditFormatConverter {
         -- formatExtra is ()
      toEdit = (\ str elem -> hasValue (toEditableXml str elem,())),
      fromEdit = (\ () -> fromEditableXml)
         }
      )
toEditFormatConverter LaTeX =
   WrappedEditFormatConverter (
      EditFormatConverter {
      toEdit = (\ _ element -> makeMMiSSLatex (element,False)),
      fromEdit = (\ formatExtra string content 
         ->
            do
               let 
                  str = mkLaTeXString content
               debugString ("START|"++str++"|END")
               return (parseMMiSSLatex (Just formatExtra) str)
            )
         }
      )

mkLaTeXString :: EmacsContent TypedName -> String
mkLaTeXString (EmacsContent dataItems) =
   concatMap
      (\ dataItem -> case dataItem of
         EditableText str -> str
         EmacsLink (included,ch) -> 
            "\\Include"
            ++ toIncludeStr ch
            ++ "{" ++ included ++ "}{status=present}"
         )     
      dataItems
   
-- ------------------------------------------------------------------
-- Turning a Format into conversion functions for exporting
-- ------------------------------------------------------------------

exportElement :: Format -> Element -> WithError String
exportElement XML element = hasValue (toExportableXml element)
exportElement LaTeX element =
   mapWithError 
      (\ (content,_) -> mkLaTeXString content)
      (makeMMiSSLatex (element,True))

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

      
   

         

   
