{- This module defines an object in MMiSS as part of the ObjectTypes framework.
   -}
module MMiSSObject(
   ) where

import Concurrent
import qualified IOExts(unsafePerformIO)

import Dynamics
import VariableSet

import CodedValue
import Link
import ObjectTypes
import BasicObjects
import AttributesType
import DisplayParms
import GlobalRegistry

import EmacsContent

import MMiSSPaths
import MMiSSGeneric

-- ------------------------------------------------------------------------
-- The MMiSSObjectType type, and its instance of HasCodedValue
-- ------------------------------------------------------------------------

data MMiSSObjectType = MMiSSObjectType {
   xmlTag :: String, 
      -- Describes the type.  This String should be identical with 
      -- corresponding XML Tag, eg "atom".
   typeId :: GlobalKey,
   extraAttributes :: AttributesType,
      -- This describes the attributes peculiar to this MMiSS object type.
      -- All MMiSS objects additionally have the attributes
      -- (pathNameKey,EntityPath) and (autoExpand,Bool)
      -- which should not be included here.
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

instance HasCodedValue MMiSSObjectType where
   encodeIO = mapEncodeIO
      (\ (MMiSSObjectType {xmlTag = xmlTag,typeId = typeId,
         extraAttributes = extraAttributes,displayParms = displayParms}) -> 
         (xmlTag,typeId,extraAttributes,displayParms)
         )
   decodeIO codedValue0 view =
      do
         ((xmlTag,typeId,extraAttributes,displayParms),codedValue1) 
            <- safeDecodeIO codedValue0 view
         knownObjects <- newEmptyVariableSet
         return (MMiSSObjectType {xmlTag = xmlTag,typeId = typeId,
            extraAttributes = extraAttributes,displayParms = displayParms,
            knownObjects = knownObjects},codedValue1)
         
-- ------------------------------------------------------------------------
-- The MMiSSObject type, and its instance of HasCodedValue
-- ------------------------------------------------------------------------

data MMiSSObject = MMiSSObject {
   name :: String, -- the user name for this.  NB - although this is
      -- currently fixed, we will probably change this.
   mmissObjectType :: MMiSSObjectType,
   attributes :: Attributes,
      -- The object's path is only stored in attributes.
   content :: Link [MMiSSText],
      -- The content of the object (including strings and references)
   includedObjects :: VariableSet EntityName,
      -- Points to objects with True LinkStatus mention in Include's
      -- in the content.
   referencedObjects :: VariableSet EntityName
      -- Ditto Reference's.
   }
 
mmissObject_tyRep = mkTyRep "MMiSSObject" "MMiSSObject"
instance HasTyRep MMiSSObject where
   tyRep _ = mmissObject_tyRep

instance HasCodedValue MMiSSObject where
   encodeIO = mapEncodeIO 
      (\ (MMiSSObject {name = name,mmissObjectType = mmissObjectType,
         attributes = attributes,content = content}) ->
         (name,typeId mmissObjectType,attributes,content)
         )
   decodeIO codedValue0 view =
      do
         ((name,tId,attributes,content),codedValue1) <-
            decodeIO codedValue0 view
         mmissObjectType <- lookupInGlobalRegistry globalRegistry view tId
         includedObjects <- newEmptyVariableSet
         referencedObjects <- newEmptyVariableSet
         return (MMiSSObject {name = name,mmissObjectType = mmissObjectType,
            attributes = attributes,content = content,
            includedObjects = includedObjects,
            referencedObjects = referencedObjects},codedValue1)
         

-- ------------------------------------------------------------------
-- The global registry and a permanently empty variable set
-- ------------------------------------------------------------------

globalRegistry :: GlobalRegistry MMiSSObjectType
globalRegistry = IOExts.unsafePerformIO createGlobalRegistry
{-# NOINLINE globalRegistry #-}



      
   

         

   