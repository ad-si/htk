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

import FiniteMap

import GraphConfigure

import AttributesType
import DisplayParms
import GlobalRegistry
import Link

import MMiSSVariant
import DTD_MMiSS
import MMiSSVerify
import {-# SOURCE #-} MMiSSObjects

---
-- Fields are primed to distinguish them from the similar field names
-- in the type MMiSSObjects.MMiSSObjectType 
data MMiSSObjectTypeData = MMiSSObjectTypeData {
   xmlTag' :: String,
   typeId' :: GlobalKey,
   dtdItem' :: DTDItem,
   attributesType' :: AttributesType,
   displayParms' :: NodeTypes (String,Link MMiSSObject)
   }


mmissObjectTypeMap :: FiniteMap String MMiSSObjectTypeData
mmissObjectTypeMap =
   listToFM
      (map
         (\ (xmlTag,dtdItem,attributesType,displayParms) ->
            (xmlTag,MMiSSObjectTypeData {
               xmlTag' = xmlTag,
               typeId' = oneOffKey "MMiSSObjectTypeList" xmlTag,
               dtdItem' = dtdItem,
               attributesType' = attributesType,
               displayParms' = displayParms
               }))        
         mmissObjectTypeList
         )

-- The string is the xml tag.  The GlobalKey is deduced from this.
mmissObjectTypeList :: [(String,DTDItem,AttributesType,
   NodeTypes (String,Link MMiSSObject))]
mmissObjectTypeList = [
   ("textfragment",toDTDItem (e :: Textfragment),allAttributes,
      simpleNodeTypes "white" Triangle),
   ("paragraph",toDTDItem (e :: Paragraph),allAttributes,
      simpleNodeTypes "yellow" Triangle),
   ("atom",toDTDItem (e :: Atom),allAttributes,simpleNodeTypes "green" Box),
   ("section",toDTDItem (e :: Section),allAttributes,
      simpleNodeTypes "red" Box),
   ("package",toDTDItem (e :: Package),allAttributes,
      simpleNodeTypes "blue" Box)
   ]
   where 
      e = error "MMiSSObjectTypeList 1"

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

simpleNodeTypes :: String -> Shape (String,Link MMiSSObject) 
    -> NodeTypes (String,Link MMiSSObject)
simpleNodeTypes colorName shape = 
   addNodeRule
      AllDisplays
      (SimpleNodeAttributes {nodeColor = Just (Color colorName),
         shape = Just shape})
      emptyNodeTypes