{- This file processes the object type data in MMiSSDTD to extract information
   about all the required object types.
   -}
module MMiSSObjectTypeList(
   MMiSSObjectTypeData(..), -- data describing an object type
   mmissObjectTypeMap, -- :: FiniteMap String MMiSSObjectTypeData
   constructKey, -- :: String -> GlobalKey
      -- Return the key in the global registry for objects with this tag
   ) where

import Maybe

import FiniteMap

import GraphConfigure

import AttributesType
import DisplayParms
import GlobalRegistry
import Link

import MMiSSDTDAssumptions
import MMiSSVariant
import MMiSSDTD
import {-# SOURCE #-} MMiSSObjects

---
-- Fields are primed to distinguish them from the similar field names
-- in the type MMiSSObjects.MMiSSObjectType 
data MMiSSObjectTypeData = MMiSSObjectTypeData {
   xmlTag' :: String,
   typeId' :: GlobalKey,
   attributesType' :: AttributesType,
   displayParms' :: NodeTypes (String,Link MMiSSObject)
   }


mmissObjectTypeMap :: FiniteMap String MMiSSObjectTypeData
mmissObjectTypeMap =
   listToFM
      (map
         (\ xmlTag ->
            (xmlTag,MMiSSObjectTypeData {
               xmlTag' = xmlTag,
               typeId' = constructKey xmlTag,
               attributesType' = variantAttributesType,
               displayParms' = getDisplayInstruction xmlTag
               })
            )
         allLabelledElements
         )

---
-- Return the key in the global registry for objects with this tag
constructKey :: String -> GlobalKey
constructKey xmlTag = oneOffKey "MMiSSObjectTypeList" xmlTag

