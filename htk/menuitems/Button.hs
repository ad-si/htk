{- #########################################################################

MODULE        : Button
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Conventional click button

TO BE DONE    : Accelerator configs should be ignored for button widgets!


   ######################################################################### -}


module Button (
        ButtonWidget(..),
        HasAccelerator(..),

        Button,
        button,
        newButton

        ) where

import SIM
import GUICore
import Image
import BitMap
import Packer
import MenuItem
import Menu(Menu,addMenuItem)
import Debug(debug)


-- --------------------------------------------------------------------------
--  Type
-- --------------------------------------------------------------------------

data Button a = Button GUIOBJECT (MVar (() -> IO a))


-- --------------------------------------------------------------------------
--  Creation
-- --------------------------------------------------------------------------

button :: [Config (Button ())] -> IO (Button ())
button ol = do
        bt <- createClickButton return
        configure bt ol


newButton :: [Config (Button a)] -> IO (Button a)
newButton ol = do
        bt <- createClickButton (\_ -> raise undefinedEventHandler)
        configure bt ol


createClickButton :: (() -> IO a) -> IO (Button a)
createClickButton f = do 
        b <- createGUIObject CLICKBUTTON buttonMethods
        mv <- newMVar (f)
        return (Button b mv)



-- --------------------------------------------------------------------------
--  Instances
-- --------------------------------------------------------------------------

instance Eq (Button a) where 
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (Button a) where 
        toGUIObject (Button w _) = w
        cname _ = "Button"

instance Destructible (Button a) where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive (Button a)

instance Widget (Button a)

instance ChildWidget (Button a) where 
        packW bt = setMethods (toGUIObject bt) buttonMethods
 
instance ButtonWidget (Button a)

instance ParentWidget (Menu a) (Button a) where
        parent mn w = do {
                addMenuItem mn (toGUIObject w) activeEntryMethods (getTrigger w);
                return w
                }

instance Synchronized (Button a) where
        synchronize w = synchronize (toGUIObject w)


instance HasAccelerator (Button a)

instance HasBitMap (Button a)

instance HasBorder (Button a)

instance HasColour (Button a) where 
        legalColourID = buttonColours

instance HasEnable (Button a)

instance HasFont (Button a)

instance HasJustify (Button a)

instance HasPadding (Button a)

instance HasPhoto (Button a)

instance HasSize (Button a)

instance GUIValue v => HasText (Button a) v

instance HasUnderline (Button a)


-- --------------------------------------------------------------------------
--  Events & Triggers
-- --------------------------------------------------------------------------

instance Reactive Button a where
        triggered bt @ (Button _ mv) = 
                userinteraction bt ButtonClicked Request >>> do
                        f <- getVar mv
                        f ()

instance HasTrigger Button a where
        getTrigger bt = return (triggered bt)

instance HasCommand Button () a where
        command f bt@(Button w mv) = do {setVar mv f;return bt}

instance HasMapTrigger Button where
        mapTrigger f (Button w mv1) = do
                g <- getVar mv1
                mv2 <- newMVar (\x -> g x >>= f)
                return (Button w mv2)

