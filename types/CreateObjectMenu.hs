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
import ObjectTypes
import {-# SOURCE #-} Folders

---
-- puts up a menu which selects an object type and creates an object in
-- the given folder.
-- 
-- If the Bool is True on return that means the object has been actually
-- inserted in the folder, otherwise that still has to be done.
createObjectMenu :: View -> Link Folder -> IO (Maybe (WrappedLink,Bool))
createObjectMenu view folderLink =
   do
      allObjectTypes <- getAllObjectTypes view
      let
         (allObjectTypeMenuItems :: 
            [(String,View -> Link Folder -> IO (Maybe (WrappedLink,Bool)))]) =
            mapMaybe
               createObjectMenuItem
               allObjectTypes

         (form :: Form (View -> Link Folder 
            -> IO (Maybe (WrappedLink,Bool)))) = 
            case allObjectTypes of 
               [] -> error "No available object types" 
                  -- unlikely, Folder should be available at least.
               _ -> newFormOptionMenu2 allObjectTypeMenuItems
      createFnOpt <- doForm "Object Type selection" form
      case createFnOpt of
         Nothing -> return Nothing
         Just createFn -> createFn view folderLink
     