{- #########################################################################

MODULE        : GUIObjectName
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Defines the structure of object names, corresponding
                to Tk's pathnames, canvas item names etc.


   ######################################################################### -}


module GUIObjectName (
        ObjectName(..),
        TextItemName(..),
        WidgetName(..),
        CanvasTagOrID(..),
        toWidgetName
        ) where

import Object(ObjectID(..))
import Resources
import Debug(debug)


-- --------------------------------------------------------------------------
--  Object Name
-- --------------------------------------------------------------------------

data ObjectName = 
          ObjectName String                             -- widget
        | MenuItemName ObjectName Int                   -- menu item
        | CanvasItemName ObjectName CanvasTagOrID       -- canvas item
        | TextPaneItemName ObjectName TextItemName      -- text item

data TextItemName =
          TextTagID ObjectID
        | TextItemPosition GUIVALUE             -- Point actually
        | EmbeddedWindowName ObjectName


data CanvasTagOrID = CanvasTagOrID ObjectID 


toWidgetName :: ObjectName -> WidgetName
toWidgetName (ObjectName s) = WidgetName s
{-# INLINE toWidgetName #-}


-- --------------------------------------------------------------------------
--  Instances 
-- --------------------------------------------------------------------------

instance Show ObjectName where
   showsPrec d p r = 
      (case p of 
                (ObjectName s) -> s
                (MenuItemName s _) -> show s
                (TextPaneItemName s _) -> show s
                (CanvasItemName s _) -> show s
        ) ++ r

instance Show TextItemName where
   showsPrec d p r = 
      (case p of 
                (TextTagID (ObjectID i)) -> "tag" ++ show i
                (TextItemPosition p) -> show p
                (EmbeddedWindowName p) -> show p
        ) ++ r


instance Show CanvasTagOrID where
   showsPrec d (CanvasTagOrID i) r = "$v" ++ show i ++ r



-- --------------------------------------------------------------------------
--  Widget Path Names 
-- --------------------------------------------------------------------------

data WidgetName = WidgetName String 


-- --------------------------------------------------------------------------
--  Instances
-- --------------------------------------------------------------------------

instance GUIValue WidgetName where
        cdefault = WidgetName "."

instance Read WidgetName where
   readsPrec p b = [(WidgetName b,[])]

instance Show WidgetName where
   showsPrec d (WidgetName p) r =  p ++  r
