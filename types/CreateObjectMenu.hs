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
         (menu :: HTkMenu (View -> Link Folder 
            -> IO (Maybe (WrappedLink,Bool)))) =
            HTkMenu (
               Menu "Object Types available"
                  (map (\ (str,fn) -> Button str fn) allObjectTypeMenuItems)
               )
         (form1 :: Form (Maybe (View -> Link Folder 
            -> IO (Maybe (WrappedLink,Bool))))) =
            newFormMenu EmptyLabel menu
         (form2 :: Form (View -> Link Folder 
            -> IO (Maybe (WrappedLink,Bool)))) =
            mapForm (\ createFnOpt -> case createFnOpt of
               Nothing -> hasError "Object type must be specified"
               Just createFn -> hasValue createFn
               ) form1
      createFnOpt <- doForm "Object Type selection" form2
      case createFnOpt of
         Nothing -> return Nothing
         Just createFn -> createFn view folderLink
     