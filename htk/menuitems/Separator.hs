{- #########################################################################

MODULE        : Separator
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Separators for menu's and widgets. As a widget it is just
                a frame with a given relief and borderwidth etc. As a 
                menu item it will be turned into a real separator and
                the illegal config options will be filtered away.


TO BE DONE    : A separator widget that is packed horizontally, should 
                always have fill X set. Vice versa for vertical separators!



   ######################################################################### -}


module Separator (
        Separator,
        newSeparator
        
        ) where

import Concurrency
import GUICore
import Packer
import Menu
import MenuItem
import Debug(debug)
                
-- --------------------------------------------------------------------------
-- Separator
-- --------------------------------------------------------------------------   

data Separator = Separator GUIOBJECT deriving Eq

newSeparator :: [Config Separator] -> IO Separator
newSeparator confs = do {
        w <- createWidget FRAME; 
        configure (Separator w) (defaults ++ confs)
} where defaults = [relief Sunken, orient Horizontal, borderwidth 1]


                
-- --------------------------------------------------------------------------
-- Instances
-- --------------------------------------------------------------------------   

instance GUIObject Separator where 
        toGUIObject (Separator w) = w
        cname w = "Separator"

instance Destructible Separator where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive Separator
        
instance Widget Separator

instance ChildWidget Separator

instance Synchronized Separator where
        synchronize w = synchronize (toGUIObject w)

instance HasBorder Separator

instance ParentWidget (Menu a) Separator where
        parent mn item = synchronize item (do {
                setObjectKind (toGUIObject item) SEPARATOR;
                packMenuItem (toGUIObject mn) (toGUIObject item) (Just passiveEntryMethods);
                return item
                })

instance HasOrientation Separator where
        orient Horizontal s = do {
                configure s [height 2];
                return s
                }
        orient Vertical s = do {
                configure s [width 2];
                return s
                }

instance HasSize Separator


