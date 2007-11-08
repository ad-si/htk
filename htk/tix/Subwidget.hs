module Subwidget (

  CanBeSubwidget (..),
  createSubwidget

) where

import Core
  (ObjectKind (..),createGUIObject,ObjectName,ObjectID,TclScript,
   ConfigOption, Methods (..), GUIOBJECT,getObjectName,getParentObject)
import BaseClasses (Widget)

-- | Using the function createSubwidget, instantiating the
-- @class CanBeSubwidget@
-- should be easy, compare as an example instantiation of the @Entry@ widget.
createSubwidget :: ObjectKind -> Methods -> GUIOBJECT -> IO GUIOBJECT
createSubwidget kind meths megawidget
    = do mwName <- getObjectName megawidget
         Just parent <- getParentObject megawidget
         let megaName = show mwName
         createGUIObject parent (SUBWIDGET kind megaName)
              (meths  { createCmd = tkDoNothing })

-- ---------------------------------------------------------
-- TkDoNothing: There is nothing to do to create Subwidgets.
-- ---------------------------------------------------------

tkDoNothing::ObjectName->ObjectKind->ObjectName->ObjectID->[ConfigOption]
           ->TclScript
tkDoNothing _ _ _ _ _ = []
{-# INLINE tkDoNothing #-}

-- | Tix mega widgets are composed of several subwidgets.
-- As it is sometimes important to access these subwidgets, there
-- is a way in Htk of creating widgets as subwidgets by instanciating the
-- @class CanBeSubwidget@.

class Widget w => CanBeSubwidget w  where
-- | Use createAsSubwidget instead of the normal constructor new[WidgetName].
  createAsSubwidget :: GUIOBJECT
  -- ^ The only parameter is a reference to the mega widget the subwidget
  -- is part of.
                    -> IO w
