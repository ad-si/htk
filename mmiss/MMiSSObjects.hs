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

import Control.Exception
import FiniteMap
import Concurrent
import qualified IOExts(unsafePerformIO)

import Dynamics
import Sink
import Sources
import VariableSet
import VariableSetBlocker
import Computation
import AtomString
import ExtendedPrelude
import Debug
import qualified Exception
import Registry
import Delayer(delay)

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
import EntityNames
import LinkManager hiding (objectContents)
import Folders
import SpecialNodeActions

import EmacsEdit
import EmacsContent

import LaTeXParser

import MMiSSDTDAssumptions(getMiniType,toIncludeStr,getPath)
import MMiSSAttributes
-- import MMiSSObjectTypeList
import MMiSSVariant
import MMiSSContent
import MMiSSDTD
import MMiSSEditXml
import MMiSSReAssemble
import MMiSSLaTeX
import MMiSSPreamble
import MMiSSVariantObject

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
   displayParms :: NodeTypes (Link MMiSSObject)
      -- Displays parameters for this object
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

      initialiseMMiSSPreambles view

createObjectType :: MMiSSObjectTypeData -> IO MMiSSObjectType
createObjectType (MMiSSObjectTypeData {xmlTag' = xmlTag',typeId' = typeId',
      attributesType' = attributesType',
      displayParms' = displayParms'}) = 
   do
      return (MMiSSObjectType {xmlTag = xmlTag',typeId = typeId',
         attributesType = attributesType',
         displayParms = displayParms'})
         
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
   mmissObjectType :: MMiSSObjectType,
   linkedObject :: LinkedObject,
      -- contains name and containing folder information
   variantAttributes :: Attributes, 
      -- Current variant attributes for this object, IE those according to
      -- which it is opened by default, or those taken from current contents.
   objectContents :: MMiSSVariantDict (Link Element),
      -- This contains all known variants of this object.
      -- This should have no children.
   mmissPreambleLink :: Link MMiSSPreamble,
      -- Link to the object's preamble. 
   linkEnvironment :: LinkEnvironment,
      -- contains path information
   linkSource :: LinkSource LinkType,
      -- Nodes out of this object.
   editLock :: BSem,
      -- Set when we are editing this object.
   nodeActions :: NodeActionSource,
      -- Special actions for the node
   extraNodes :: Blocker (ArcData WrappedLink ArcType)
      -- Nodes which connect to this one but are not normally shown
      -- (Preamble, security manager and so on).
   }
 
mmissObject_tyRep = mkTyRep "MMiSSObject" "MMiSSObject"
instance HasTyRep MMiSSObject where
   tyRep _ = mmissObject_tyRep

instance HasCodedValue MMiSSObject where
   encodeIO =
      let
         encodeIO0 = 
            mapEncodeIO
               (\ (MMiSSObject {
                     mmissObjectType = mmissObjectType,
                     linkedObject = linkedObject,
                     objectContents = objectContents,
                     mmissPreambleLink = mmissPreambleLink,
                     linkEnvironment = linkEnvironment,
                     linkSource = linkSource}) ->
                  (typeId mmissObjectType,linkedObject,objectContents,
                     mmissPreambleLink,
                     LinkSourceSet linkEnvironment [linkSource])
                  )
         encodeIO1 mmissObject codedValue view =
            do
               setFontStyle (nodeActions mmissObject) def
               encodeIO0 mmissObject codedValue view
      in
         encodeIO1
              
   decodeIO codedValue0 view =
      do
         ((tId,linkedObject,objectContents,
            mmissPreambleLink,LinkSourceSet linkEnvironment [linkSource]),
               codedValue1) 
            <- safeDecodeIO codedValue0 view
         mmissObjectType <- lookupInGlobalRegistry globalRegistry view tId
         variantAttributes <- newEmptyAttributes view
         mkVariantAttributes variantAttributes
         editLock <- newBSem
         nodeActions <- newNodeActionSource
         extraNodes <- newExtraNodes mmissPreambleLink

         return (MMiSSObject {
            mmissObjectType = mmissObjectType,
            linkedObject = linkedObject,
            variantAttributes = variantAttributes,
            objectContents = objectContents,
            mmissPreambleLink = mmissPreambleLink,
            linkEnvironment = linkEnvironment,
            linkSource = linkSource,
            editLock = editLock,
            nodeActions = nodeActions,
            extraNodes = extraNodes
            },codedValue1)

-- ------------------------------------------------------------------
-- Instances of HasAttributes, HasLinkedObject, Eq and Ord
-- ------------------------------------------------------------------

instance HasAttributes MMiSSObject where
   readPrimAttributes object = variantAttributes object

instance HasLinkedObject MMiSSObject where
   toLinkedObject = linkedObject

instance Eq MMiSSObject where
   (==) obj1 obj2 = linkedObject obj1 == linkedObject obj2

instance Ord MMiSSObject where
   compare obj1 obj2 = compare (linkedObject obj1) (linkedObject obj2)

-- ------------------------------------------------------------------
-- The instance of ObjectType
-- ------------------------------------------------------------------

instance ObjectType MMiSSObjectType MMiSSObject where
   objectTypeTypeIdPrim _ = "MMiSSObjects"

   objectTypeIdPrim objectType = typeId objectType

   objectTypeGlobalRegistry object = globalRegistry

   getObjectTypePrim object = mmissObjectType object

   createObjectMenuItemPrim objectType = 
      Just (xmlTag objectType,\ view folder 
         -> createMMiSSObject objectType view folder)

   nodeTitleSourcePrim object = fmap toString
      (mmissNodeTitleSource object)

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

            includedArcType :: ArcType
            includedArcType = fromString "I"

            includedArcParms =
               Color "red" $$$
               Thick $$$
               emptyArcTypeParms

            referencedArcType :: ArcType
            referencedArcType = fromString "R"

            referencedArcParms =
               Color "red" $$$
               Dotted $$$
               emptyArcTypeParms

            toArcType :: LinkType -> ArcType
            toArcType IncludeLink = includedArcType
            toArcType LinkLink = linkedArcType
            toArcType ReferenceLink = referencedArcType

            linkedArcType :: ArcType
            linkedArcType = fromString "L"

            linkedArcParms =
               Color "red" $$$
               Dashed $$$
               emptyArcTypeParms


            -- preambleArcType is defined later and globally.

            preambleArcParms =
               Color "blue" $$$
               emptyArcTypeParms

            theNodeType = fromString ""

            newNodeTypeParms nodeTypeParms =
               let
                  editOptions = [
                     Button "Edit Object as LaTeX"
                        (\ link -> editMMiSSObjectLaTeX view link),
                     Button "Edit Attributes" 
                        (\ link -> editObjectAttributes view link),
                     Button "Export Object To File"
                        (\ link -> exportMMiSSObject view link),
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
                     mmissLinks1
                        = mkArcEnds False (linkSource mmissObject) toArcType

                  mmissLinks2 
                     <- blockVariableSet (extraNodes mmissObject) blockID 

                  return (catVariableLists mmissLinks1 mmissLinks2)
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
-- Creating or updating an object from an Xml Element.
-- ------------------------------------------------------------------


---
-- PreObject is a type synonym used in writeToMMiSSObject and 
-- simpleWriteToMMiSSObject.
-- It is interpreted as follows.  It represents an object that is to be
-- written.
--
-- The first component is 
--    (Left (folder,name)) if the object does not exist, when 
--       it is the place to insert it.
--    (Right object) if the object does exist (and is the object).
-- The second component is the corresponding StructuredContent.
type PreObject = 
   (Either (Link Folder,EntityName) (Link MMiSSObject,MMiSSObject),
      StructuredContent)

---
-- Creates or update an object from an Xml Element.
-- If it fails it returns a String.
-- The MMiSSObjectType is the expected type of the object.
--
-- If the bool checkThisEditLock is set we insist on getting the edit lock
-- of the top object, if it already exists.  In any case we get the edit lock
-- of all included objects.
writeToMMiSSObject :: MMiSSObjectType -> View -> Link Folder -> 
   Maybe EntityFullName -> Element -> Bool -> IO (WithError (Link MMiSSObject))
writeToMMiSSObject objectType view folderLink expectedLabel element 
   checkThisEditLock =

   addFallOutWE (\ break ->
      (delay view) (do
         -- (0) get folder
         folder <- readLink view folderLink

         -- (1) validate it.
         case validateElement (xmlTag objectType) element of
            (s@ (_:_)) -> break (unlines s)
            _ -> done

         -- (2) structure it
         let
            (contentsWE :: WithError StructuredContent) 
               = structureContents element

         (contents :: StructuredContent) 
            <- coerceWithErrorOrBreakIO break contentsWE

         let
            -- This is the function we use for getting the children of
            -- a StructuredContent object.
            childs object = children (accContents object)

         -- (3) check the object's label
            objectLabel = label contents

         case expectedLabel of
            Nothing -> done
            Just lab -> 
               if lab == objectLabel
                  then
                     done
                  else
                     break ("Expected label is "++toString lab++" but "
                        ++toString objectLabel++" found")

         -- (4) Construct a list of type
         -- [PreObject]
         -- with one element for each structuredContent element.
         -- In this list, children always appear before their parents.
         -- In particular, we will assume that the top object comes last.

         -- We also (since it is now easy to do) check that the objects
         -- being written to have the right XML tag.
         let
            -- We use treeFoldM.  In its type,
            -- ancestorInfo is the (LinkedObject,EntityPath) with respect to
            -- which we look up the object, using lookupObjectByPath.
            --   (N.B. That means that for a path belonging to an object,
            --   the path needs to be raised if the corresponding object
            --   is passed, since for example the path "." attached to an 
            --   MMiSSObject means "Start from the *parent* of the object".
            -- state is the above list
            -- node is the structured contents.

            -- So we need the following function.
            treeFoldFn :: 
               (LinkedObject,EntityPath) -> [PreObject] -> StructuredContent
               -> IO ((LinkedObject,EntityPath),[PreObject],
                  [StructuredContent])
            treeFoldFn (linkedObject,entityPath) elementsSoFar 
               structuredContent =
               do
                  let
                     name = label structuredContent
                  -- We need to find the object.  Our strategy is as follows
                  -- (1) we attempt to find the object.
                  -- (2) if it doesn't exist, we look for the containing 
                  --     folder (where we will have to construct the object).
                  (objectLinkOptWE :: WithError (Maybe (Link MMiSSObject)))
                     <- lookupObjectByPath linkedObject entityPath name
                  let
                     objectLinkOpt = coerceWithErrorOrBreak break 
                        objectLinkOptWE

                     thisPath = path structuredContent
 
                  -- Obtain the first element of PreObject, and an object and
                  -- path to look up the children.
                  --
                  -- If the object does not yet exist we use the parent.
                  (firstItem,linkedObject,path) <- case objectLinkOpt of
                     Nothing ->
                        do
                           let
                              folderNameOpt = entityDir name
                              folderName = fromMaybe 
                                 (break "MMiSSObjects bug 1") folderNameOpt
                              baseNameOpt = entityBase name
                              baseName = fromMaybe 
                                 (break "MMiSSObjects bug 2") baseNameOpt

                           (folderLinkOptWE :: WithError (Maybe (Link Folder)))
                              <- lookupObjectByPath linkedObject entityPath 
                                 folderName
                           let
                              folderLinkOpt 
                                 = coerceWithErrorOrBreak break folderLinkOptWE
                              folderLink = fromMaybe
                                 (break ("Can't find folder to insert "
                                    ++toString name))
                                 folderLinkOpt
                           seq folderLink done

                           -- To construct a suitable linkEnvironment we
                           -- apply the hack of specifying the folder as
                           -- LinkedObject
                           folder <- readLink view folderLink 

                           return (Left (folderLink,baseName),
                              toLinkedObject folder,thisPath)
                     Just objectLink ->
                        do
                           object <- readLink view objectLink

                           let
                              oldTag = xmlTag (mmissObjectType object)
                              newTag = tag structuredContent
                           unless (oldTag == newTag)
                              (break ("Object you define "++toString name++
                                "already exists, but with type "++
                                oldTag++" rather than "++newTag))
    
                           linkEnvironment <- newParentLinkEnvironment
                              (toLinkedObject object) thisPath
                           return (Right (objectLink,object),
                              toLinkedObject object,raiseEntityPath thisPath)
                  let
                     element = (firstItem,structuredContent)
                  return ((linkedObject,path),element:elementsSoFar,
                     childs structuredContent)

         -- Now run treeFoldM.
         (elements :: [PreObject]) <- treeFoldM treeFoldFn 
            (toLinkedObject folder,trivialPath)
            [] contents

         -- (5) Check that no new object is defined twice.
         let
            elementsSorted = 
               sortBy (\ (n1,_) (n2,_) -> compare n1 n2) elements
            split = groupBy (\ (n1,_) (n2,_) -> n1 == n2) elementsSorted
            definedTwice = 
               findJust
                  (\ same -> case same of
                     (n,_):_:_ -> Just n
                     [__] -> Nothing
                     )
                  split

         case definedTwice of
            Nothing -> done
            Just loc -> 
               do
                  desc <- case loc of
                     Left (_,entityName) -> return (toString entityName)
                     Right (_,object) -> nodeTitleIOPrim object
                  break ("Name "++desc++" is multiply defined")

         -- (6) Attempt to grab the editLock for every object that is already
         --     defined, unless (if checkThisEditLock) it is the top object.
         let
            (objectsToCheck0 :: [(MMiSSObject,EntityFullName)]) =
               mapMaybe
                  (\ (loc,structuredContent) -> case loc of
                        Right (_,mmissObject) 
                           -> Just (mmissObject,label structuredContent)
                        Left _ -> Nothing
                     )
                  elements

            objectsToCheck1 =
               if checkThisEditLock
                  then
                     objectsToCheck0
                  else
                     fromMaybe
                        (break "MMiSSObjects bug 1")
                        (chop 1 objectsToCheck0)

         releaseActWE <- tryAcquireBSemsWithError
            (\ (object,_) -> editLock object)
            (\ (_,fullName) -> "Cannot write to "++toString fullName
               ++" as it is already being edited")
            objectsToCheck1

         releaseAct <- coerceWithErrorOrBreakIO break releaseActWE

         -- (7) Add all the objects.  We take them in elements order, which 
         -- means the top element is last.
         newLinks <- 
            synchronizeView view (
               Exception.finally (
                  mapM (simpleWriteToMMiSSObject view break) elements)
                  -- Release all the edit locks
                  releaseAct
                  )
         return (last newLinks)
      ))

---
-- Construct or update an MMiSSObject in a folder given its 
-- PreObject.
-- We also take a break function argument for indicating an error
simpleWriteToMMiSSObject :: View -> BreakFn -> PreObject 
   -> IO (Link MMiSSObject)
simpleWriteToMMiSSObject view break (loc,structuredContent) =
   do
      let
         searchObject = toMMiSSSearchObjectFromXml 
            (attributes structuredContent)
      -- (1) construct and register the actual object, if it doesn't already 
      --     exist, and also get a place (a Versioned item) to put the new 
      --     contents in.
      (versioned,objectLink,object) <- case loc of
         Right (objectLink,object) ->
            do
               linkOpt <- variantDictSearchExact (objectContents object) 
                  searchObject
               versioned <- case linkOpt of
                  Just link -> fetchLink view link
                  Nothing -> newEmptyObject view

               -- Set the path and included and referenced objects.
               setPath (linkEnvironment object) (path structuredContent)
               setLinks (linkSource object) 
                  (links (accContents structuredContent))
               return (versioned,objectLink,object) 
         Left (folderLink,entityName) ->
            do
               mmissObjectType <- retrieveObjectType view 
                  (tag structuredContent)
               variantAttributesWE <- fromXmlAttributes view
                  (extractVariantAttributes (attributes structuredContent))
               variantAttributes 
                  <- coerceWithErrorOrBreakIO break variantAttributesWE
               objectContents <- newEmptyVariantDict
               editLock <- newBSem
               nodeActions <- newNodeActionSource
               extraNodes <- newExtraNodes mmissPreambleLink 

               objectLinkOpt <- createWithLinkedObjectIO 
                  view folderLink entityName 
                  (\ linkedObject ->
                     do
                        linkEnvironment 
                           <- newParentLinkEnvironment 
                              linkedObject 
                              (path structuredContent)
                        linkSource 
                           <- newLinkSource 
                              linkEnvironment 
                              (links (accContents structuredContent))
                        return (MMiSSObject {
                           mmissObjectType = mmissObjectType,
                           linkedObject = linkedObject,
                           variantAttributes = variantAttributes,
                           objectContents = objectContents,
                           linkEnvironment = linkEnvironment,
                           linkSource = linkSource,
                           editLock = editLock,
                           nodeActions = nodeActions,
                           extraNodes = extraNodes
                           })
                     )    

               let
                  objectLink = fromMaybe (break ("Couldn't write to "++
                     toString (label structuredContent))) objectLinkOpt

               seq objectLink done
               object <- readLink view objectLink
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
   -> IO (Maybe (Link MMiSSObject))
createMMiSSObject objectType view folder =
   do
      result <- addFallOut (\ break ->
         do
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
               return (Just link)

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
editMMiSSObjectInner formatConverter view link =
   do
      object <- readLink view link
      variants <- toMMiSSSearchObject (variantAttributes object)

      objectTitle <- readContents (
         getLinkedObjectTitle (linkedObject object) (fromString "UNNAMED")
         )

      let
         emacsFS = mkEmacsFS view formatConverter 
         printAction = mkPrintAction view formatConverter 
         topEditRef = EditRef {
            objectLink = link,
            variants = variants,
            miniType = getObjectMiniType object,
            description = EntityFullName [objectTitle]
            }
            
      editEmacs emacsFS printAction topEditRef

-- ------------------------------------------------------------------
-- Values needed for EmacsEdit
-- ------------------------------------------------------------------

---
-- EditRef is the "ref" type passed to EmacsEdit. 
-- We expect it to be (possibly) made more 
-- complicated later when we handle variant attributes properly (allowing
-- the same object to be edited in multiple variants simultaneously), hence
-- the need to make it abstract.
--
-- The extra fields (apart from the link and variants) mean we can get what's 
-- needed to do the Emacs buttons, on the basis of what's in the referencing 
-- object, without having to dereference the link.
data EditRef = EditRef {
   objectLink :: Link MMiSSObject,
   variants :: MMiSSSearchObject,
   miniType :: Char,
   description :: EntityFullName
   }

instance Eq EditRef where
   (==) ref1 ref2 = (objectLink ref1) == (objectLink ref2)

instance Ord EditRef where
   compare ref1 ref2 = compare (objectLink ref1) (objectLink ref2)

mkEmacsFS :: View -> WrappedEditFormatConverter -> EmacsFS EditRef
mkEmacsFS view 
      (WrappedEditFormatConverter (EditFormatConverter {
         toEdit = toEdit,fromEdit = fromEdit})) =
   let
      toDescription :: EditRef -> String
      toDescription = toString. description

      -- Now for the difficult one.
      editFS :: EditRef 
         -> IO (WithError (EmacsContent EditRef,EditedFile EditRef))
      editFS (EditRef {objectLink = objectLink,variants = variants,
            miniType = miniType0,description = description0}) =
         addFallOutWE (\ break -> 
            do
               let
                  name = toString description0

               object <- readLink view objectLink
               if getObjectMiniType object == miniType0
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
               let
                  -- redefine break so that it releases the lock, if something
                  -- goes wrong during the rest of this 
                  break2 mess =
                     do
                        release lock
                        break mess
               let
                  contents = objectContents object

               elementLinkOpt <- variantDictSearch contents variants
               elementLink <- case elementLinkOpt of
                  Nothing -> break2 ("Object "++name++
                     " has no matching variant")
                  Just elementLink -> return elementLink
               (element @ (Elem _ attributes _)) 
                  <- readLink view elementLink

               -- Get hold of the object's parent.
               parentLinkOptWE <- toParentLink object 
 
               (parentLink :: Link Folder) <-
                  case fromWithError parentLinkOptWE of
                     Left mess -> break2 mess
                     Right Nothing -> break2 "Object has no current parent!"
                     Right (Just parentLink) -> return parentLink

               let
                  entityPath = coerceWithErrorOrBreak break
                     (getPath element)
               
               -- Create variants used for searching in this object.
               -- We merge the parent variants with the variants in
               -- this object.
               let
                  innerVariants0 = toMMiSSSearchObjectFromXml attributes
                  innerVariants 
                     = mergeMMiSSSearchObjects variants innerVariants0

                  contentWE = toEdit name element


               (content0 :: EmacsContent (String,Char),formatExtra)
                  <- case fromWithError contentWE of
                     Left mess -> break2 mess
                     Right content0 -> return content0

               -- convert content0 into EmacsContent EditRef.
               content <- mapMonadic
                  (\ (string,miniType) ->
                     do
                        description 
                           <- case fromWithError (fromStringWE string) of
                              Left mess -> break2 mess
                              Right description -> return description

                        objectLinkOptWE <- lookupObjectByPath 
                           (toLinkedObject object) (raiseEntityPath entityPath)
                           description
                        
                        objectLink <- case fromWithError objectLinkOptWE of
                           Left mess -> break2 mess
                           Right Nothing -> break2 ("Object "++string
                              ++" has the wrong type")
                           Right (Just objectLink) -> return objectLink

                        let
                           editRef = EditRef {
                              objectLink = objectLink,
                              variants = innerVariants,
                              miniType = miniType,
                              description =  description
                              }

                        return editRef
                     )
                  content0
               
               -- We now have to set up the EditedFile stuff 
               let
                  writeData (emacsContent0 :: EmacsContent EditRef) =
                     addFallOutWE (\ break ->
                       do
                          let
                             (emacsContent1 :: EmacsContent (String,Char)) =
                                fmap
                                   (\ editRef -> 
                                      (toString (description editRef),
                                      miniType editRef))
                                   emacsContent0
                          elementWE <- fromEdit name emacsContent1
                          let 
                             element = 
                                coerceWithErrorOrBreak break elementWE
                          element `seq` done
                          linkWE <- writeToMMiSSObject 
                             (mmissObjectType object) view parentLink
                             (Just description0) element False
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

                  (editedFile :: EditedFile EditRef) = EditedFile {
                     writeData = writeData,
                     finishEdit = finishEdit
                     }

               setBorder (nodeActions object) DoubleBorder

               return (content,editedFile)
            )

      emacsFS = EmacsFS {
         editFS = editFS,
         toMiniType = miniType,
         toDescription = toDescription
         }
   in
      emacsFS


mkPrintAction :: View -> WrappedEditFormatConverter -> PrintAction EditRef
mkPrintAction view
      (WrappedEditFormatConverter (
         (EditFormatConverter {fromEdit = fromEdit}
            :: EditFormatConverter formatExtra)
         )) =
   let
      printAction :: EditRef 
         -> (EditRef -> IO (WithError (EmacsContent (Bool,EditRef)))) 
         -> IO ()
      printAction topRef 
            (getContent :: EditRef 
               -> IO (WithError (EmacsContent (Bool,EditRef)))) =
         do
            let
               -- Unfortunately reAssemble doesn't do exactly what we work;
               -- it gives us the Element, but what we want is the
               -- (Bool,EditRef).  To work around this we *assume* that
               -- reAssembleArg visits the children of each node in order,
               -- and pass as search data an MVar containing the 
               -- (Bool,EditRef)'s.  The EntityFullName it passes then becomes
               -- irrelevant . . .
               reAssembleArg :: EntityFullName -> MVar [(Bool,EditRef)] 
                  -> IO (WithError (Maybe (Element,MVar [(Bool,EditRef)])))
               reAssembleArg entityFullName mVar =
                  do
                     ((doExpand,editRef):rest) <- takeMVar mVar
                     putMVar mVar rest
                     assert (entityFullName == description editRef) done

                     if doExpand
                        then
                           addFallOutWE (\ break ->
                              do
                                 content0WE <- getContent editRef
                                 let
                                    content0 = coerceWithErrorOrBreak break 
                                       content0WE
                                 seq content0 done

                                 nextMVar <- newMVar (toEmacsLinks content0)

                                 let
                                    content1 = fmap 
                                       (\ (b,editRef) -> 
                                          (toString (description editRef),
                                             miniType editRef)
                                          ) 
                                       content0

                                    name = toString entityFullName

                                 elementWE <- fromEdit name content1

                                 let
                                    element = coerceWithErrorOrBreak break 
                                       elementWE

                                 seq element done

                                 return (Just (element,nextMVar))
                              )                                  
                        else
                           return (hasValue Nothing)

            topMVar <- newMVar [(True,topRef)]

            elementWE 
               <- reAssemble reAssembleArg (description topRef) topMVar
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
                              Right str -> mmissLaTeX (
                                 toString (description topRef)) str
                        )
                     done
   in
      PrintAction printAction

-- ------------------------------------------------------------------
-- Exporting MMiSS objects
-- ------------------------------------------------------------------

exportMMiSSObject :: View -> Link MMiSSObject -> IO ()
exportMMiSSObject view link =
   do
      result <- addFallOut (\ break ->
         do
            object <- readLink view link

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
            objectTitle <- nodeTitleIOPrim object

            mmissLaTeX objectTitle string
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
            getElement :: EntityFullName 
               -> (LinkedObject,EntityPath,MMiSSSearchObject)
               -> IO (WithError (Maybe (Element,
                     (LinkedObject,EntityPath,MMiSSSearchObject))))
            getElement entityFullName (linkedObject,entityPath,searchObject) = 
               addFallOutWE (\ break ->
                  do
                     linkOptWE <- lookupObjectByPath linkedObject entityPath
                        entityFullName
                     let
                        link = 
                           fromMaybe
                              (break ("Object "++toString entityFullName
                                 ++ " is not an MMiSS object"))
                           (coerceWithErrorOrBreak break linkOptWE)

                     seq link done
                     object <- readLink view link
                     elementLinkOpt <- variantDictSearch 
                        (objectContents object) searchObject
                     let
                        elementLink = case elementLinkOpt of
                           Nothing -> break ("Object "
                              ++ toString entityFullName
                              ++ " has no version with matching attributes")
                           Just elementLink -> elementLink
                     (element @ (Elem _ attributes _)) 
                        <- readLink view elementLink
       
                     let
                        entityPath0 = coerceWithErrorOrBreak break
                           (getPath element)
                        searchObject2 =
                           mergeMMiSSSearchObjects
                              searchObject 
                              (toMMiSSSearchObjectFromXml attributes) 
                     seq entityPath0 done
                     let
                        entityPath1 = raiseEntityPath entityPath0

                     return (Just (element,
                        (toLinkedObject object,entityPath1,searchObject2)))
                  )

         initialSearchObject 
            <- toMMiSSSearchObject (variantAttributes object)

         title <- mmissNodeTitleIO object

         completeElementWE <- reAssembleNoRecursion getElement 
            (EntityFullName [title])
            (toLinkedObject object,raiseEntityPath trivialPath,
               initialSearchObject)

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
data EditFormatConverter formatExtra = EditFormatConverter {
   toEdit :: String -> Element 
      -> WithError (EmacsContent (String,Char),formatExtra),
   fromEdit :: String -> EmacsContent (String,Char) 
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
      fromEdit = fromEditableXml
         }
      )
toEditFormatConverter LaTeX =
   WrappedEditFormatConverter (
      EditFormatConverter {
      toEdit = (\ _ element -> makeMMiSSLatex (element,False)),
      fromEdit = (\ string content 
         ->
            do
               let 
                  str = mkLaTeXString content
               debugString ("START|"++str++"|END")
               return (parseMMiSSLatex Nothing str)
            )
         }
      )

mkLaTeXString :: EmacsContent (String,Char) -> String
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
-- Miscellaneous utilities.
-- ------------------------------------------------------------------

errorMess :: String -> IO ()
errorMess mess = createErrorWin mess []

getObjectMiniType :: MMiSSObject -> Char
getObjectMiniType object = getMiniType (xmlTag (mmissObjectType object))

mmissNodeTitleSource :: MMiSSObject -> SimpleSource EntityName
mmissNodeTitleSource object =
   getLinkedObjectTitle (linkedObject object) (fromString "UNNAMED")

mmissNodeTitleIO :: MMiSSObject -> IO EntityName
mmissNodeTitleIO object = readContents (mmissNodeTitleSource object)

-- ------------------------------------------------------------------
-- extraNodes
-- ------------------------------------------------------------------

preambleArcType :: ArcType
preambleArcType = fromString "B"

newExtraNodes :: Link MMiSSPreamble 
   -> IO (Blocker (ArcData WrappedLink ArcType))
newExtraNodes mmissPreambleLink =
   do
      let
         setSource = staticSource [
            mkArcData (WrappedLink mmissPreambleLink) preambleArcType True]
      newBlocker setSource

   
-- ------------------------------------------------------------------
-- Type arguments for the VariableObject   
-- ------------------------------------------------------------------

---
-- This is what varies with the variant attributes.
data Variable = Variable {
   element :: Link Element,
   preamble :: Link MMiSSPreamble,
   editLock :: BSem
   }

---
-- This is what is cached, and is needed to display the node's links with
-- the current set of attributes.
data Cache = Cache {
   cacheElement :: Element,
   cacheLinkEnvironment :: LinkEnvironment,
   cacheLinks :: LinkSource LinkType,
   cachePreamble :: Link MMiSSPreamble,
   }

-- ------------------------------------------------------------------
-- Instances of HasCodedValue for them.
-- ------------------------------------------------------------------

variable_tyRep = mkTyRep "MMiSSObjects" "Variable"
instance HasTyRep Variable where
   tyRep _ = variable_tyRep

cache_tyRep = mkTyRep "MMiSSObjects" "Cache"
instance HasTyRep Cache where
   tyRep _ = cache_tyRep

instance HasCodedValue Variable where
   encodeIO = mapEncodeIO (\
      (Variable {
         element = element,
         preamble = preamble
         })
      ->
      (element,preamble)
      )
   decodeIO codedValue0 view =
      do
         ((element,preamble),codedValue1) <- decodeIO codedValue0 view
         editLock <- newBSem
         let
            variable = Variable {
               element = element,
               preamble = preamble,
               editLock = editLock
               }
         return variable

instance HasCodedValue Object where
   encodeIO = mapEncodeIO (\ 
      (Object {
         cacheElement = cacheElement,
         cacheLinkEnvironment = cacheLinkEnvironment,
         cacheLinks = cacheLinks,
         cachePreamble = cachePreamble
         })
      ->
      (cacheElement,LinkSourceSet cacheLinkEnvironment [cacheLinks],
         cachePreamble)
      )

   decodeIO = mapDecodeIO (\
      (cacheElement,LinkSourceSet cacheLinkEnvironment [cacheLinks],
         cachePreamble)
      ->
      (Object {
         cacheElement = cacheElement,
         cacheLinkEnvironment = cacheLinkEnvironment,
         cacheLinks = cacheLinks,
         cachePreamble = cachePreamble
         })
      )

---
-- Converter function.  This also needs to know the view
converter :: view -> Variable -> IO Object
converter view variable =
   do
&&&&&&


   
   
   