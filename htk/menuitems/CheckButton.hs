{- #########################################################################

MODULE        : CheckButton
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Check Button Widgets


   ######################################################################### -}


module CheckButton (
        ButtonWidget(..),
        SelectButton(..),
        ToggleButton(..),

        Indicator(..),
        HasIndicator(..),

        Toggle(..),

        CheckButton(..),
        checkbutton,
        newCheckButton

        ) where

import SIM
import GUICore
import Image
import BitMap
import Packer
import Button
import Menu
import MenuItem
import Indicator
import Debug(debug)


-- --------------------------------------------------------------------------
--  Check Button Type
-- --------------------------------------------------------------------------

data CheckButton a = CheckButton GUIOBJECT (MVar (Toggle -> IO a))


-- --------------------------------------------------------------------------
--  Commands
-- --------------------------------------------------------------------------

checkbutton :: [Config (CheckButton Toggle)] -> IO (CheckButton Toggle)
checkbutton ol = do
        bt <- createCheckButton return
        configure bt ol


newCheckButton :: [Config (CheckButton a)] -> IO (CheckButton a)
newCheckButton ol = do
        bt <- createCheckButton (\t -> raise undefinedEventHandler)
        configure bt ol


createCheckButton :: (Toggle -> IO a) -> IO (CheckButton a)
createCheckButton f = do
        b <- createGUIObject (CHECKBUTTON Off) checkButtonMethods
        cset b "variable" (tkButtonVar (objectID(toGUIObject b)))
        mv <- newMVar f
        return (CheckButton b mv)


-- --------------------------------------------------------------------------
--  Instances
-- --------------------------------------------------------------------------

instance Eq (CheckButton a) where 
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (CheckButton a) where 
        toGUIObject (CheckButton wid _) = wid
        cname _ = "CheckButton"

instance Destructible (CheckButton a) where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive (CheckButton a)

instance Widget (CheckButton a)

instance ChildWidget (CheckButton a) where 
        packW bt = setMethods (toGUIObject bt) buttonMethods

instance ButtonWidget (CheckButton a)

instance SelectButton (CheckButton a) where
    selectionState On bt =  synchronize bt (do {
                setObjectKind (toGUIObject bt) (CHECKBUTTON On);
                selectionState On (toGUIObject bt);
                return bt
                }) 
    selectionState Off bt = synchronize bt (do {
                setObjectKind  (toGUIObject bt) (CHECKBUTTON Off);
                selectionState Off (toGUIObject bt);
                return bt
                })
    getSelectionState bt = do {
                kind <- getObjectKind (toGUIObject bt);
                case kind of {(RADIOBUTTON st) -> return st}
                }


instance ToggleButton (CheckButton a) where
   toggleButton w = synchronize w (do {
                updState w toggle;                      
                toggleButton (toGUIObject w);
                })

instance ParentWidget (Menu a) (CheckButton a) where
        parent mn w = do {
                addMenuItem mn (toGUIObject w) checkItemMethods (getTrigger w);
                return w
                }

instance Synchronized (CheckButton a) where
        synchronize w = synchronize (toGUIObject w)

instance HasBitMap (CheckButton a)

instance HasBorder (CheckButton a)

instance HasColour (CheckButton a) where 
        legalColourID = buttonColours

instance HasEnable (CheckButton a)

instance HasFont (CheckButton a)

instance HasIndicator (CheckButton a)

instance HasPadding (CheckButton a)

instance HasSize (CheckButton a)

instance GUIValue v => HasText (CheckButton a) v

instance HasUnderline (CheckButton a)

instance HasPhoto (CheckButton a)

instance HasJustify (CheckButton a)


-- --------------------------------------------------------------------------
--  Triggers and Events
-- --------------------------------------------------------------------------

instance Reactive CheckButton a where
        triggered w @ (CheckButton wid mv) = 
                userinteraction w ButtonClicked Request >>> do {
                        state <- updState w toggle;
                        f <- getVar mv;
                        f state
                        }

instance HasTrigger CheckButton a where
        getTrigger bt = return (triggered bt)
        
instance HasMapTrigger CheckButton where
        mapTrigger f (CheckButton w mv) = do
                g <- getVar mv
                mv2 <- newMVar (\x -> g x >>= f)
                return (CheckButton w mv2)

instance HasCommand CheckButton Toggle a where
        command f bt@(CheckButton w mv) = do {setVar mv f; return bt} 

instance HasCommand CheckButton () a where
        command f bt@(CheckButton w mv) = do {setVar mv (\_ -> f ()); return bt}


-- --------------------------------------------------------------------------
-- Aux
-- --------------------------------------------------------------------------

updState :: CheckButton a -> (Toggle -> Toggle) -> IO Toggle
updState w f = do {
        kind <- updObjectKind (toGUIObject w) f';
        case kind of {(CHECKBUTTON st) -> return st}
} where f' (CHECKBUTTON st) = CHECKBUTTON (f st) 
        


-- --------------------------------------------------------------------------
-- Methods (when appearing as menu item or as widget)
-- --------------------------------------------------------------------------

checkItemMethods :: Methods
checkItemMethods = activeEntryMethods{createCmd = tkCreateCheckButtonItem}

checkButtonMethods :: Methods
checkButtonMethods = buttonMethods{createCmd = tkCreateCheckButtonWidget}

tkCreateCheckButtonWidget kind@(CHECKBUTTON t) name oid args = 
        (createCmd buttonMethods) kind name oid args  ++
        tkSetButtonVar oid t

tkCreateCheckButtonItem kind@(CHECKBUTTON t) name oid args = 
        (createCmd activeEntryMethods) kind name oid args  ++
        tkSetButtonVar oid t


-- --------------------------------------------------------------------------
-- Tk Commands
-- --------------------------------------------------------------------------

tkSetButtonVar :: ObjectID -> Toggle -> TclScript
tkSetButtonVar oid val = ["global " ++ show var, "set " ++ show var ++ " " ++ show val]
 where var = tkButtonVar oid
{-# INLINE tkSetButtonVar #-}

tkButtonVar :: ObjectID -> WidgetName
tkButtonVar oid = WidgetName ("v" ++ (show oid))
{-# INLINE tkButtonVar #-}
