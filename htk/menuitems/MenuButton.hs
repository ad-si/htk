{- #########################################################################

MODULE        : MenuButton
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Menu Button Widget 

   ######################################################################### -}


module MenuButton (
        HasIndicator(..),
        HasAccelerator(..),

        MenuButton,
        newMenuButton

        ) where

import SIM
import GUICore
import Button
import Image
import BitMap
import Packer
import Menu
import MenuItem
import Indicator
import Debug(debug)

-- --------------------------------------------------------------------------
--  Menu Button Type 
-- --------------------------------------------------------------------------

data MenuButton a = MenuButton GUIOBJECT (PVar (IO (IA a)))


-- --------------------------------------------------------------------------
-- Commands
-- --------------------------------------------------------------------------

newMenuButton :: [Config (MenuButton a)] -> IO (MenuButton a)
newMenuButton ol = do {
        w <- createWidget MENUBUTTON;
        pv <- newPVar (return inaction);
        configure (MenuButton w pv) ol
}


-- --------------------------------------------------------------------------
-- Instances
-- --------------------------------------------------------------------------

instance Eq (MenuButton a) where 
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (MenuButton a) where 
        toGUIObject (MenuButton w _) = w
        cname _ = "MenuButton"

instance Destructible (MenuButton a) where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive (MenuButton a)

instance Widget (MenuButton a)

instance ChildWidget (MenuButton a) where 
        packW bt = setMethods (toGUIObject bt) buttonMethods

instance ParentWidget (Menu a) (MenuButton a) where  -- cascade menu's
        parent mn item @ (MenuButton wid _) = do {
                addMenuItem mn wid passiveEntryMethods (getTrigger item);
                return item
                }

instance ParentWidget (MenuButton a) (Menu a) where   -- container for menu's
        parent mb @ (MenuButton wid pv) mn = do {
                setVar pv (getTrigger mn);
                packMenu (toGUIObject mb) (toGUIObject mn);
                return mn
                }
                        
instance Synchronized (MenuButton a) where
        synchronize w = synchronize (toGUIObject w)

instance HasAccelerator (MenuButton a)

instance HasBitMap (MenuButton a)

instance HasBorder (MenuButton a)

instance HasColour (MenuButton a) where 
        legalColourID = buttonColours

instance HasEnable (MenuButton a)

instance HasFont (MenuButton a)

instance HasJustify (MenuButton a)

instance HasIndicator (MenuButton a)

instance HasPadding (MenuButton a)

instance HasPhoto (MenuButton a)

instance HasSize (MenuButton a)

instance GUIValue v => HasText (MenuButton a) v

instance HasUnderline (MenuButton a)

-- --------------------------------------------------------------------------
-- Events
-- --------------------------------------------------------------------------   

instance HasTrigger MenuButton a where
        getTrigger (MenuButton w pv) = do {cmd <- getVar pv; cmd}

instance HasMapTrigger MenuButton where
        mapTrigger f mb @ (MenuButton w pv) = do {
                pv' <- newPVar (newTrigger mb); 
                return (MenuButton w pv')
                } where newTrigger mb = do {
                                ev <- getTrigger mb;
                                return (ev >>>= f)
                                }                                                               
