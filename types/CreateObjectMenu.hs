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
import ObjectTypes

---
-- puts up a menu which selects an object type and creates it.
createObjectMenu :: View -> IO (Maybe WrappedLink)
createObjectMenu view =
   do
      allObjectTypes <- getAllObjectTypes view
      let
         (allObjectTypeMenuItems :: [(String,View -> IO (Maybe WrappedLink))]) 
               =
            mapMaybe
               createObjectMenuItem
               allObjectTypes
         (menu :: HTkMenu (View -> IO (Maybe WrappedLink))) =
            HTkMenu (
               Menu "Object Types available"
                  (map (\ (str,fn) -> Button str fn) allObjectTypeMenuItems)
               )
         (form1 :: Form (Maybe (View -> IO (Maybe WrappedLink)))) =
            newFormMenu EmptyLabel menu
         (form2 :: Form (View -> IO (Maybe WrappedLink))) =
            mapForm (\ createFnOpt -> case createFnOpt of
               Nothing -> hasError "Object type must be specified"
               Just createFn -> hasValue createFn
               ) form1
      createFnOpt <- doForm "Object Type selection" form2
      case createFnOpt of
         Nothing -> return Nothing
         Just createFn -> createFn view
     