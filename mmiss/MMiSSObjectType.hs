{- This file contains the *definition* of the MMiSSObject
   type, and its instances of HasCodedValue and other trivial types.
   (Not alas ObjectType, the instance of that will come in 
   MMiSSObjectTypeInstance.hs).
   -}
module MMiSSObjectType(
   MMiSSObject(..),
   Variable(..),
   Cache(..),

   -- The various ArcType values for different arcs out of MMiSSObject
   includedArcType,
   referencedArcType,
   linkedArcType,
   preambleArcType,

   -- Functions for extracting an object's current title.
   objectNameSource,
   objectName,

   -- Function to be passed to functions for creating new variant
   -- objects.
   converter,

   createMMiSSObject, -- all MMiSSObjects are created by this function.
  
   ) where

#include "config.h"

import Computation(coerceWithErrorIO)
import Sources
import VariableSet
import VariableSetBlocker
import Dynamics
import AtomString (fromString,toString)
import ReferenceCount

import BSem

import Graph(ArcType)

#if HAXMLINT
import Text.XML.HaXml.Types
#else
import XmlTypes
#endif

import VersionDB (Location)
import ViewType
import Link
import ObjectTypes
import CodedValue
import LinkDrawer
import LinkManager
import EntityNames
import SpecialNodeActions

import MMiSSVariantObject
import MMiSSContent
import MMiSSPreamble
import MMiSSObjectTypeType

-- ---------------------------------------------------------------------
-- The types
-- ---------------------------------------------------------------------

---
-- There are three sorts of things stored in an MMiSSObject.
-- (1) Things which don't depend on the variant under consideration
-- (mmissObjectType,linkedObject,nodeActions,extraNodes).
-- (2) Things which *do* depend on the variant under consideration, and are
-- therefore stored via the Variable type as part of an VariantObject
-- (everything in a Variable).
-- (3) A cached version of a Variable, which is stored in a VariantObject
-- as the "current object".
-- (everything in an Object). 
data MMiSSObject = MMiSSObject {
   mmissObjectType :: MMiSSObjectType,
   linkedObject :: LinkedObject,
   nodeActions :: NodeActionSource,
      -- Special actions for the node, which is passed to the node display
      -- functions.
   extraNodes :: Blocker (ArcData WrappedLink ArcType),
      -- Nodes which connect to this one but are not normally shown
      -- (Preamble, security manager and so on).
   variantObject :: VariantObject Variable Cache,
   editCount :: RefCount
      -- This counts the number of times variants of this objects
      -- are being edited, which we need for knowing whether to display it
      -- with a double border or not.
   }

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
   cachePreamble :: Link MMiSSPreamble
   }

-- ---------------------------------------------------------------------
-- Instances of Typeable, HasCodedValue, HasLinkedObject for MMiSSObject
-- ---------------------------------------------------------------------

instance HasLinkedObject MMiSSObject where
   toLinkedObject mmissObject = linkedObject mmissObject

mmissObject_tyRep = mkTyRep "MMiSSObjectType" "MMiSSObject"
instance HasTyRep MMiSSObject where
   tyRep _ = mmissObject_tyRep

instance HasCodedValue MMiSSObject where
   encodeIO (MMiSSObject {mmissObjectType = mmissObjectType,
      linkedObject = linkedObject,variantObject = variantObject}) 
         codedValue0 view =
      do
         frozenVariantObject <- freezeVariantObject variantObject
         encodeIO (mmissObjectType,linkedObject,frozenVariantObject)
            codedValue0 view

   decodeIO codedValue0 view =
      do
         ((mmissObjectType,linkedObject,frozenVariantObject),codedValue1) 
            <- decodeIO codedValue0 view
         variantObject <- unfreezeVariantObject (converter view linkedObject)
            frozenVariantObject
         mmissObject 
            <- createMMiSSObject mmissObjectType linkedObject variantObject
         return (mmissObject,codedValue1)

-- Also used during merging.
createMMiSSObject :: MMiSSObjectType -> LinkedObject 
   -> VariantObject Variable Cache
   -> IO MMiSSObject
createMMiSSObject mmissObjectType linkedObject variantObject =
   do
      let
         extraNodeSource :: SimpleSource (ArcData WrappedLink ArcType)
         extraNodeSource =
            fmap
               (\ cache ->
                  toArcData (WrappedLink (cachePreamble cache))
                     preambleArcType True
                  )
               (toVariantObjectCache variantObject)
                  
      (extraNodes :: Blocker (ArcData WrappedLink ArcType))
         <- newBlocker (singletonSetSource extraNodeSource)

      nodeActions <- newNodeActionSource
      editCount <- newRefCount

      let
         mmissObject = MMiSSObject {
            mmissObjectType = mmissObjectType,
            linkedObject = linkedObject,
            nodeActions = nodeActions,
            extraNodes = extraNodes,
            variantObject = variantObject,
            editCount = editCount
            }

      return mmissObject

-- ---------------------------------------------------------------------
-- Instances of HasCodedValue for Variable and Cache.
-- ---------------------------------------------------------------------

variable_tyRep = mkTyRep "MMiSSObjects" "Variable"
instance HasTyRep Variable where
   tyRep _ = variable_tyRep

cache_tyRep = mkTyRep "MMiSSObjects" "Cache"
instance HasTyRep Cache where
   tyRep _ = cache_tyRep

instance HasCodedValue Variable where
   encodeIO = mapEncodeIO (\ --
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
         return (variable,codedValue1)

instance HasCodedValue Cache where

   encodeIO = mapEncodeIO (\ 
      (Cache {
         cacheElement = cacheElement,
         cacheLinkEnvironment = cacheLinkEnvironment,
         cacheLinks = cacheLinks,
         cachePreamble = cachePreamble
         })
      ->
      (cacheElement,LinkSourceSet cacheLinkEnvironment [cacheLinks],
         cachePreamble)
      )

   decodeIO = mapDecodeIO (\ --
      (cacheElement,LinkSourceSet cacheLinkEnvironment [cacheLinks],
         cachePreamble)
      ->
      (Cache {
         cacheElement = cacheElement,
         cacheLinkEnvironment = cacheLinkEnvironment,
         cacheLinks = cacheLinks,
         cachePreamble = cachePreamble
         })
      )

-- ---------------------------------------------------------------------
-- Converting a Variable to a Cache
-- ---------------------------------------------------------------------

---
-- Converter function.  This also needs to know the view and LinkedObject
-- for the containing object.
converter :: View -> LinkedObject -> Variable -> IO Cache
converter view linkedObject variable =
   do
      cacheElement <- readLink view (element variable)
      cacheLinkEnvironment <- newLinkEnvironment linkedObject
         (raiseEntityPath trivialPath)
      let
         structureContentsWE = structureContents cacheElement
      structureContents <- coerceWithErrorIO structureContentsWE
      let
         cacheLinks0 = links (accContents structureContents)
         cacheLinks1 = map
            (\ (fullName,variantSearch,linkType) -> (fullName,linkType))
            cacheLinks0

      cacheLinks2 <- newLinkSource cacheLinkEnvironment cacheLinks1
      let
         cachePreamble = preamble variable

         cache = Cache {
            cacheElement = cacheElement,
            cacheLinkEnvironment = cacheLinkEnvironment,
            cacheLinks = cacheLinks2,
            cachePreamble = cachePreamble
            }

      return cache
            
-- ---------------------------------------------------------------------
-- The ArcType values for the arcs out of an MMiSSObject.
-- ---------------------------------------------------------------------

---
-- The first three correspond to the various LinkType constructors in
-- MMiSSContent.
includedArcType :: ArcType
includedArcType = fromString "I"

referencedArcType :: ArcType
referencedArcType = fromString "R"

linkedArcType :: ArcType
linkedArcType = fromString "L"

---
-- Link to the preamble arc type.
preambleArcType :: ArcType
preambleArcType = fromString "B"

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
---
-- Extract an object's current name.  (Mainly needed for error messages.)
objectName :: MMiSSObject -> IO String
objectName mmissObject = readContents (objectNameSource mmissObject)
