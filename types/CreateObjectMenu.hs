{- CreateObjectMenu.createObjectMenu puts up a menu which selects an object 
   type and creates it. -}
module CreateObjectMenu(
   createObjectMenu,
   ) where

import Maybe

import Computation

import MenuType
import HTkMenu

import SimpleForm

import View
import Link
import LinkManager
import ObjectTypes

---
-- puts up a menu which selects an object type and creates an object in
-- the given folder.
createObjectMenu :: (HasLinkedObject parent,ObjectType parentType parent)
   => View -> Link parent -> IO Bool
createObjectMenu view (parentLink :: Link parent) =
   do
      allObjectTypes <- getAllObjectTypes view
      let
         (allObjectTypeMenuItems :: 
            [(String,View -> LinkedObject -> IO Bool)]) =
            mapMaybe
               createObjectMenuItem
               allObjectTypes

         (form :: Form (View -> LinkedObject -> IO Bool)) = 
            case allObjectTypes of 
               [] -> error "No available object types" 
                  -- unlikely, Folder should be available at least.
               _ -> newFormOptionMenu2 allObjectTypeMenuItems
      createFnOpt <- doForm "Object Type selection" form
      case createFnOpt of
         Nothing -> return False
         Just createFn -> 
            do
               parent <- readLink view parentLink
               bracketForImportErrors view (
                  createFn view (toLinkedObject parent)) 