{- This file describes all the MMiSSObjectTypes.
   NB.  It is HIGHLY unsatisfactory that this should be hardcoded
   into the repository code like this; it should be in the DTD somehow.
   However for the time being something like this is forced on us anyway
   because of the MMiSSVeriy.DTDItem mechanism.
   -}
module MMiSSObjectTypeList(
   MMiSSObjectTypeData(..), -- data describing an object type
   mmissObjectTypeMap, -- :: FiniteMap String MMiSSObjectTypeData
   ) where

import Maybe

import FiniteMap

import GraphConfigure

import AttributesType
import DisplayParms
import GlobalRegistry
import Link

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
               typeId' = oneOffKey "MMiSSObjectTypeList" xmlTag,
               attributesType' = allAttributes,
               displayParms' = getDisplayInstruction xmlTag
               })
            )
         allElements
         )

-- All the attributes we provide are the same.
extraAttributes :: [String]
extraAttributes = 
   ["version","data","comment","previous-version","authors","prior-authors"]

allAttributes :: AttributesType
allAttributes =
   foldl
      (\ attType name -> needs (mkAttributeKey name) "" attType)
      variantAttributesType
      extraAttributes
