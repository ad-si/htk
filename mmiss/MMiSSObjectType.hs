-- |
-- Description: Defines 'MMiSSObjectType.MMiSSObject'.
-- 
-- This file contains the *definition* of the MMiSSObject
-- type, and its instances of HasCodedValue and other trivial types.
-- (Not alas ObjectType, the instance of that will come in 
-- MMiSSObjectTypeInstance.hs).
module MMiSSObjectType(
   MMiSSObject(..),
   Variable(..),
   Cache(..),

   -- The various ArcType values for different arcs out of MMiSSObject
   includedArcType,
   referencedArcType,
   linkedArcType,
   preambleArcType,
   fileArcType,

   -- Functions for extracting an object's current title.
   objectNameSource,
   objectName,

   -- Function to be passed to functions for creating new variant
   -- objects.
   converter,

   createMMiSSObject, -- all MMiSSObjects are created by this function.

   variablesSame,
   ) where

import Maybe

import System.IO.Unsafe(unsafeInterleaveIO)

import Computation(coerceWithErrorIO,fromWithError)
import Sources
import VariableSet
import VariableSetBlocker
import Dynamics
import AtomString (toString,fromString,fromStringWE)
import ReferenceCount
import Messages
import Broadcaster

import BSem

import Graph(ArcType)

import Text.XML.HaXml.Types

import XmlExtras


import VersionDB (Location)
import ViewType
import Link
import ObjectTypes
import CodedValue
import LinkDrawer
import LinkManager
import EntityNames

import MMiSSDTDAssumptions
import MMiSSVariant
import MMiSSVariantObject
import MMiSSObjectTypeType
import MMiSSElementInfo
import MMiSSElementInstances
import {-# SOURCE #-} MMiSSPackageFolder

-- ---------------------------------------------------------------------
-- The types
-- ---------------------------------------------------------------------

-- | There are three sorts of things stored in an MMiSSObject.
-- (1) Things which don\'t depend on the variant under consideration
-- (mmissObjectType,linkedObject,nodeActions,extraNodes).
-- (2) Things which \*do\* depend on the variant under consideration, and are
-- therefore stored via the Variable type as part of an VariantObject
-- (everything in a Variable).
-- (3) A cached version of a Variable, which is stored in a VariantObject
-- as the \"current object\".
-- (everything in an Object). 
data MMiSSObject = MMiSSObject {
   mmissObjectType :: MMiSSObjectType,
   linkedObject :: LinkedObject,
   extraNodes :: Blocker (ArcData WrappedLink ArcType),
      -- Nodes which connect to this one but are not normally shown
      -- (Preamble, security manager and so on).
      -- (currently defunct)
   variantObject :: VariantObject Variable Cache,
   editCount :: RefCount,
      -- This counts the number of times variants of this objects
      -- are being edited, which we need for knowing whether to display it
      -- with a double border or not.
   isEditedBroadcaster :: SimpleBroadcaster Bool
      -- This is Bool if the object is being edited.
   } deriving (Typeable)

-- | This is what varies with the variant attributes.
data Variable = Variable {
   element :: Link Element,
      -- For now we adopt the convention that the name of the element as given
      -- by its label attribute is always the last component of the name of 
      -- the MMiSS object
   editLock :: BSem
   } deriving (Typeable)

-- | This is what is cached, and is needed to display the node\'s links with
-- the current set of attributes.
data Cache = Cache {
   cacheElement :: Element,
   cacheLinks :: LinkSource LinkType,
   cacheEditLock :: BSem
   }

-- ---------------------------------------------------------------------
-- Instances of Typeable, HasCodedValue, HasLinkedObject for MMiSSObject
-- ---------------------------------------------------------------------

instance HasLinkedObject MMiSSObject where
   toLinkedObject mmissObject = linkedObject mmissObject

instance HasBinary MMiSSObject CodingMonad where
   writeBin = mapWriteIO
      (\ (MMiSSObject {mmissObjectType = mmissObjectType,
            linkedObject = linkedObject,variantObject = variantObject}) ->
         do
            frozenVariantObject <- freezeVariantObject variantObject
            return (mmissObjectType,linkedObject,frozenVariantObject)
         )
   readBin = mapReadViewIO
      (\ view (mmissObjectType,linkedObject,frozenVariantObject) ->
         do 
            variantObject <- unfreezeVariantObject 
               (converter view linkedObject) frozenVariantObject
            mmissObject 
               <- createMMiSSObject mmissObjectType linkedObject variantObject
            return mmissObject
        )

-- Also used during merging.
createMMiSSObject :: MMiSSObjectType -> LinkedObject 
   -> VariantObject Variable Cache
   -> IO MMiSSObject
createMMiSSObject mmissObjectType linkedObject variantObject =
   do
      (extraNodes :: Blocker (ArcData WrappedLink ArcType))
         <- newBlocker emptyVariableSetSource

      editCount <- newRefCount
      isEditedBroadcaster <- newSimpleBroadcaster False

      let
         mmissObject = MMiSSObject {
            mmissObjectType = mmissObjectType,
            linkedObject = linkedObject,
            extraNodes = extraNodes,
            variantObject = variantObject,
            editCount = editCount,
            isEditedBroadcaster = isEditedBroadcaster
            }

      return mmissObject

-- ---------------------------------------------------------------------
-- Instances of HasCodedValue for Variable.
-- ---------------------------------------------------------------------

instance HasBinary Variable CodingMonad where
   writeBin = mapWrite
      (\ (Variable {
         element = element
         }) 
      ->
      element
      )
   readBin = mapReadIO
      (\ element ->
         do
            editLock <- newBSem
            let
               variable = Variable {
                  element = element,
                  editLock = editLock
                  }
            return variable
         )

-- ---------------------------------------------------------------------
-- Converting a Variable to a Cache
-- ---------------------------------------------------------------------

-- | Converter function.  This also needs to know the view and LinkedObject
-- for the containing object.
converter :: View -> LinkedObject -> MMiSSVariantSpec -> Variable -> IO Cache
converter view linkedObject variantSpec variable =
   -- we wrap the operation in unsafeInterleaveIO so that we can
   -- assume (or at least hope) that the function won't be called
   -- until it is actually needed.
   unsafeInterleaveIO (
      do
         cacheElement0 <- readLink view (element variable)

         packageFolderAndNameWE 
            <- getMMiSSPackageFolderAndName view linkedObject
         (packageFolder,name) <- coerceWithErrorIO packageFolderAndNameWE

         let
            cacheElement1WE = setElementInfoStrict cacheElement0
               (ElementInfo {
                  packageIdOpt = Nothing,
                  packagePathOpt1 = Just name,
                  packageNameOpt = Nothing,
                  labelOpt = Just (FromHere name),
                  variants = variantSpec
                  })
         cacheElement1 <- case fromWithError cacheElement1WE of
            Left mess ->
               do
                  alertMess ("Unable to set variants: " ++ mess)
                  return cacheElement0
            Right cacheElement1 -> return cacheElement1

         -- Get the LinkedObject for the containing MMiSSPackageFolder.
         let
            packageLinkedObject 
               = toMMiSSPackageFolderLinkedObject packageFolder

         -- Get the links for the element.
         let
            allSubElements = getAllElements1 cacheElement1

            links0 :: [(LinkType,String)]
            links0 = mapMaybe classifyLink allSubElements

         (links1 :: [(EntitySearchName,LinkType)]) <-
            mapM
               (\ (linkType,searchNameStr) ->
                  do
                     searchName 
                        <- coerceWithErrorIO (fromStringWE searchNameStr)
                     return (searchName,linkType)
                  )
               links0

         cacheLinks2 <- newLinkSource view packageLinkedObject links1
         let
            cache = Cache {
               cacheElement = cacheElement1,
               cacheLinks = cacheLinks2,
               cacheEditLock = editLock variable 
               }

         return cache
      )
            
-- ---------------------------------------------------------------------
-- The ArcType values for the arcs out of an MMiSSObject.
-- ---------------------------------------------------------------------

-- | The first three correspond to the various LinkType constructors in
-- MMiSSContent.
includedArcType :: ArcType
includedArcType = fromString "I"

referencedArcType :: ArcType
referencedArcType = fromString "R"

linkedArcType :: ArcType
linkedArcType = fromString "L"

-- | Link to the preamble arc type.
preambleArcType :: ArcType
preambleArcType = fromString "B"

-- | Link to the file arc type.
fileArcType :: ArcType
fileArcType = fromString "B"

-- ---------------------------------------------------------------------
-- We make ArcData WrappedLink ArcType 
-- instance HasKey, so we can use VariableSetBlocker.newBlock
-- ---------------------------------------------------------------------

instance HasKey (ArcData WrappedLink ArcType) Location where
   toKey arcData = toKey (destination arcData)

-- ---------------------------------------------------------------------
-- Common utility functions
-- ---------------------------------------------------------------------

objectNameSource :: MMiSSObject -> SimpleSource String
objectNameSource mmissObject =
   fmap toString (getLinkedObjectTitle (linkedObject mmissObject)
      (fromString "Untitled MMiSSObject"))
-- | Extract an object\'s current name.  (Mainly needed for error messages.)
objectName :: MMiSSObject -> IO String
objectName mmissObject = readContents (objectNameSource mmissObject)

-- | Compare if two Variables are the same for the purposes of merging.
variablesSame :: Variable -> Variable -> Bool
variablesSame variable1 variable2 =
   (element variable1 == element variable2)

