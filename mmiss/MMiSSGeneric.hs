{- MMiSSGeneric contains the classes describing what is expected of
   MMiSS objects. -}
module MMiSSGeneric(
   EditData(..),
   MMiSSObject(..),
   ) where

import ExtendedPrelude
import VariableSet

import EmacsContent

import ObjectTypes
import BasicObjects

import MMiSSPaths

---
-- The interface the object provides to something that is (emacs)-editing
-- it.
-- emacsContent is the initial contents to be edited.
-- saveEmacsContent is an action to be performed to save new 
--    (presumably edited) content
-- stopEdit is an action to be performed to stop editing.  
data EditData = EditData {
   emacsContent :: EmacsContent EntityName,
   saveEmacsContent :: (EmacsContent EntityName -> IO ()),
   stopEdit :: IO ()
   }

---
-- This class describes those types objects need to instance to
-- be editable by MMiSSEmacs.
--
-- The path in the object's "Import Path" attribute will be used
-- for locating imported entities.
class (ObjectType objectType object,HasAttributes object) =>
      MMiSSObject objectType object 
      | objectType -> object,object -> objectType where

   edit :: object -> IO (Maybe EditData)
   containedEntities :: object -> VariableSetSource EntityName
   
   