{- This module contains the definition of the MMiSSObjectType type,
   and its instances of HasCodedValue and HasAttributesType.
   -}
module MMiSSObjectTypeType(
   MMiSSObjectType(..),
   retrieveObjectType,
   allObjectTypes,
   constructKey,
   ) where

import Maybe

import Data.FiniteMap

import Dynamics

import GraphConfigure

import CodedValue
import AttributesType
import DisplayParms
import ObjectTypes
import GlobalRegistry
import Link

import MMiSSDTDAssumptions
import MMiSSVariant
import MMiSSDTD
import {-# SOURCE #-} MMiSSObjectType

-- ------------------------------------------------------------------------
-- The definition
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
   }

-- ------------------------------------------------------------------------
-- The instance of HasAttributesType
-- ------------------------------------------------------------------------

instance HasAttributesType MMiSSObjectType where
   toAttributesType objectType = attributesType objectType

-- ------------------------------------------------------------------------
-- The instances of Dynamics and HasCodedValue
-- 
-- Since the necessary information for defining an MMiSSObjectType is
-- actually defined by the DTD, we do not need to write out such things to
-- the AttributesType.  Instead we just represent it by the xmlTag.
-- ------------------------------------------------------------------------

mmissObjectType_tyRep = mkTyRep "MMiSSObject" "MMiSSObjectType"
instance HasTyRep MMiSSObjectType where
   tyRep _ = mmissObjectType_tyRep

instance HasBinary MMiSSObjectType CodingMonad where
   writeBin = mapWrite
      (\ (MMiSSObjectType {xmlTag = xmlTag}) -> xmlTag)
   readBin = mapRead
      (\ xmlTag ->
         let 
            (Just objectType) = lookupFM mmissObjectTypeMap xmlTag
         in
            objectType
         )

-- ------------------------------------------------------------------------
-- mmissObjectTypeMap contains all object types read from the DTD.
-- ------------------------------------------------------------------------

mmissObjectTypeMap :: FiniteMap String MMiSSObjectType
mmissObjectTypeMap =
   listToFM
      (map
         (\ xmlTag ->
            (xmlTag,MMiSSObjectType {
               xmlTag = xmlTag,
               typeId = constructKey xmlTag,
               attributesType = variantAttributesType,
               displayParms = getDisplayInstruction xmlTag
               })
            )
         allLabelledElements
         )

---
-- Return the key in the global registry for objects with this tag
constructKey :: String -> GlobalKey
constructKey xmlTag = oneOffKey "MMiSSObjectTypeList" xmlTag

---
-- Returns the object type for a given Xml tag.
retrieveObjectType :: String -> MMiSSObjectType
retrieveObjectType xmlTag =
   lookupWithDefaultFM mmissObjectTypeMap
      (error ("MMiSSObjectTypeType: unknown Xml tag "++xmlTag))
      xmlTag

allObjectTypes :: [MMiSSObjectType]
allObjectTypes = eltsFM mmissObjectTypeMap