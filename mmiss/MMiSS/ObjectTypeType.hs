-- | This module contains the definition of the MMiSSObjectType type,
-- and its instances of HasCodedValue and HasAttributesType.
module MMiSS.ObjectTypeType(
   MMiSSObjectType(..),
   retrieveObjectType,
   allObjectTypes,
   constructKey,
   ) where

import Data.Maybe

import Util.DeprecatedFiniteMap

import Util.Dynamics

import Types.CodedValue
import Types.DisplayParms
import Types.GlobalRegistry
import Types.Link

import MMiSS.DTD
import {-# SOURCE #-} MMiSS.ObjectType

-- ------------------------------------------------------------------------
-- The definition
-- ------------------------------------------------------------------------

data MMiSSObjectType = MMiSSObjectType {
   xmlTag :: String,
      -- Describes the type.  This String should be identical with
      -- corresponding XML Tag, eg "atom".
   typeId :: GlobalKey,
   displayParms :: NodeTypes (Link MMiSSObject)
      -- Displays parameters for this object
   } deriving (Typeable)

-- ------------------------------------------------------------------------
-- The instances of Dynamics and HasCodedValue
--
-- Since the necessary information for defining an MMiSSObjectType is
-- actually defined by the DTD, we do not need to write out such things to
-- the AttributesType.  Instead we just represent it by the xmlTag.
-- ------------------------------------------------------------------------

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
               displayParms = getDisplayInstruction xmlTag
               })
            )
         allDisplayedElements
         )

-- | Return the key in the global registry for objects with this tag
constructKey :: String -> GlobalKey
constructKey xmlTag = oneOffKey "MMiSSObjectTypeList" xmlTag

-- | Returns the object type for a given Xml tag.
retrieveObjectType :: String -> MMiSSObjectType
retrieveObjectType xmlTag =
   lookupWithDefaultFM mmissObjectTypeMap
      (error ("MMiSSObjectTypeType: unknown Xml tag "++xmlTag))
      xmlTag

allObjectTypes :: [MMiSSObjectType]
allObjectTypes = eltsFM mmissObjectTypeMap
