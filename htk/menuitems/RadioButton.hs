{- #########################################################################

MODULE        : RadioButton
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Radio Button Widgets

   ######################################################################### -}


module RadioButton (
        ButtonWidget(..),
        SelectButton(..),

        Indicator(..),
        HasIndicator(..),

        Toggle(..),

        RadioButton,
        radiobutton,
        newRadioButton

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
--  Radio Button Type
-- --------------------------------------------------------------------------

data RadioButton a = RadioButton GUIOBJECT (MVar (Toggle -> IO a))


-- --------------------------------------------------------------------------
-- Commands
-- --------------------------------------------------------------------------

radiobutton :: [Config (RadioButton Toggle)] -> IO (RadioButton Toggle)
radiobutton ol = do
        bt <- createRadioButton return 
        configure bt ol

newRadioButton :: [Config (RadioButton a)] -> IO (RadioButton a)
newRadioButton ol = do
        bt <- createRadioButton (\t -> raise undefinedEventHandler)
        configure bt ol


createRadioButton :: (Toggle -> IO a) -> IO (RadioButton a)
createRadioButton f = do
        bt <- createGUIObject (RADIOBUTTON Off) radioButtonMethods
        cset bt "variable" (tkButtonVar (objectID(toGUIObject bt)))
        cset bt "value" (tkButtonValue (toGUIObject bt))
        mv <- newMVar f
        return (RadioButton bt mv)


-- --------------------------------------------------------------------------
-- Instances
-- --------------------------------------------------------------------------

instance Eq (RadioButton a) where 
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (RadioButton a) where 
        toGUIObject (RadioButton w _) = w
        cname _ = "RadioButton"

instance Destructible (RadioButton a) where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive (RadioButton a)

instance Widget (RadioButton a)

instance ChildWidget (RadioButton a) where 
        packW bt = setMethods (toGUIObject bt) buttonMethods

instance ParentWidget (Menu a) (RadioButton a) where
        parent mn w = do {
                addMenuItem mn (toGUIObject w) radioItemMethods (getTrigger w);
                return w
                }

instance ButtonWidget (RadioButton a)

instance SelectButton (RadioButton a) where
    selectionState On bt =  synchronize bt (do {
                setObjectKind (toGUIObject bt) (RADIOBUTTON On);
                selectionState On (toGUIObject bt);
                return bt
                }) 
    selectionState Off bt = synchronize bt (do {
                setObjectKind  (toGUIObject bt) (RADIOBUTTON Off);
                selectionState Off (toGUIObject bt);
                return bt
                })
    getSelectionState bt = do {
                kind <- getObjectKind (toGUIObject bt);
                case kind of {(RADIOBUTTON st) -> return st}    
                }


instance Synchronized (RadioButton a) where
        synchronize w = synchronize (toGUIObject w)

instance HasBitMap (RadioButton a)

instance HasBorder (RadioButton a)

instance HasColour (RadioButton a) where 
        legalColourID = buttonColours

instance HasEnable (RadioButton a) 

instance HasFont (RadioButton a)

instance HasJustify (RadioButton a)

instance HasIndicator (RadioButton a)

instance HasPadding (RadioButton a)

instance HasPhoto (RadioButton a)

instance HasSize (RadioButton a)

instance GUIValue v => HasText (RadioButton a) v

instance HasUnderline (RadioButton a)


-- --------------------------------------------------------------------------
-- Events
-- --------------------------------------------------------------------------

instance Reactive RadioButton a where
        triggered bt @ (RadioButton w mv) = 
                userinteraction bt ButtonClicked Request >>> do {
                        kind <- updObjectKind w toggle';
                        f <- getVar mv;
                        let (RADIOBUTTON state) = kind in f state
                        } 
                        where toggle' (RADIOBUTTON t) = RADIOBUTTON (toggle t)


instance HasTrigger RadioButton a where
        getTrigger bt = return (triggered bt)
        
instance HasMapTrigger RadioButton where
        mapTrigger f (RadioButton w mv) = do
                g <- getVar mv
                mv2 <- newMVar (\x -> g x >>= f)
                return (RadioButton w mv2)

instance HasCommand RadioButton Toggle a where
        command f bt@(RadioButton w mv) = do {setVar mv f; return bt} 

instance HasCommand RadioButton () a where
        command f bt@(RadioButton w mv) = do {setVar mv (\_ -> f ()); return bt}



-- --------------------------------------------------------------------------
-- Methods (when appearing as menu item or widget)
-- --------------------------------------------------------------------------

radioItemMethods :: Methods
radioItemMethods = activeEntryMethods{createCmd = tkCreateRadioButtonItem}

radioButtonMethods :: Methods
radioButtonMethods = buttonMethods{createCmd = tkCreateRadioButtonWidget}

tkCreateRadioButtonWidget kind@(RADIOBUTTON t) name oid args = 
        (createCmd buttonMethods) kind name oid args  ++
        tkSetButtonVar oid t

tkCreateRadioButtonItem kind@(RADIOBUTTON t) name oid args = 
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

tkButtonValue :: GUIOBJECT -> Int
tkButtonValue wid = getObjectNo wid
{-# INLINE tkButtonValue #-}
