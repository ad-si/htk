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
   

data MMiSSText =
      Chars String
   |  Include EntityName LinkDescriptor LinkStatus
         -- Link to some other MMiSS object to be included as part of the 
         -- containing document.
   |  Reference EntityName LinkDescriptor LinkStatus
         -- Link to something not included specifically as part of the
         -- document.

type LinkDescriptor = String -- describes user-appearance of the link.
   -- It would be a good idea if this corresponded to the name field of
   -- the linked-to object.
   -- (Currently, the name of the corresponding button in Emacs)
type LinkStatus = Bool 
   -- indicates whether the entity is supposed to have already been defined.
   -- This will initially be set by checking the current path.  If initially
   -- False, upgrading it to True will require explicit action by the user.
   -- (As part of the menu attached to the link in Emacs).  
   -- Dependencies will only appear in the includedObjects set 
   -- (and the corresponding objects displayed) when this becomes true.


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