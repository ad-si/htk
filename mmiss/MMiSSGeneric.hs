{- MMiSSGeneric contains the classes describing what is expected of
   MMiSS objects. -}
module MMiSSGeneric(
   EditData(..),
   EditableObject(..),

   MMiSSText(..),
   LinkDescriptor,
   LinkStatus,

   autoExpandKey,
   ) where

import ExtendedPrelude
import VariableSet
import Dynamics
import Computation

import SimpleForm

import EmacsContent

import CodedValue
import ObjectTypes
import BasicObjects
import AttributesType

import MMiSSPaths
import MMiSSContent


---
-- The interface the object provides to something that is (emacs)-editing
-- it.
-- emacsContent is the initial contents to be edited.
-- saveEmacsContent is an action to be performed to save new 
--    (presumably edited) content
-- stopEdit is an action to be performed to stop editing.  
data EditData = EditData {
   emacsContent :: [MMiSSText],
   saveEmacsContent :: ([MMiSSText] -> IO ()),
   stopEdit :: IO ()
   }

---
-- This class describes those types objects need to instance to
-- be editable by MMiSSEmacs.
--
-- The path in the object's "Import Path" attribute will be used
-- for locating imported entities.
--
-- The following 
class (ObjectType objectType object,HasAttributes object) =>
      EditableObject objectType object 
      | objectType -> object,object -> objectType where

   edit :: object -> IO (Maybe EditData)
   includedEntities :: object -> VariableSetSource EntityName
   referencedEntities :: object -> VariableSetSource EntityName

-- ------------------------------------------------------------------   
-- The types describing the content of objects in MMiSS.
-- ------------------------------------------------------------------   
   
---
-- This is quite similar to the StructuredContents
data MMiSSText = MMiSSText {
   
   
   

-- ------------------------------------------------------------------   
-- MMiSSText is an instance of HasCodedValue
-- ------------------------------------------------------------------   

mmissText_tyRep = mkTyRep "MMiSSGeneric" "MMiSSText"
instance HasTyRep MMiSSText where
   tyRep _ = mmissText_tyRep
instance HasCodedValue MMiSSText where
   encodeIO (Chars str) codedValue repository =
      encode2IO 'C' str codedValue repository
   encodeIO (Include e d s) codedValue repository =
      encode2IO 'I' (e,d,s) codedValue repository
   encodeIO (Reference e d s) codedValue repository =
      encode2IO 'R' (e,d,s) codedValue repository

   decodeIO codedValue0 repository =
      do
         (letter,codedValue1) <- safeDecodeIO codedValue0 repository
         case letter of
            'C' -> 
               do
                  (str,codedValue2) <- safeDecodeIO codedValue1 repository
                  return (Chars str,codedValue2)
            'I' ->
               do
                  ((e,d,s),codedValue2) <- safeDecodeIO codedValue1 repository
                  return (Include e d s,codedValue2)
            'R' ->
               do
                  ((e,d,s),codedValue2) <- safeDecodeIO codedValue1 repository
                  return (Reference e d s,codedValue2)

-- ------------------------------------------------------------------   
-- The AutoExpand attribute key.
-- This should have type Bool.
-- ------------------------------------------------------------------   


autoExpandKey :: AttributeKey
autoExpandKey = mkAttributeKey "AutoExpand"
