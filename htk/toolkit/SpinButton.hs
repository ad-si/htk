{- #########################################################################

MODULE        : SpinButton
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : alpha
DESCRIPTION   : The SpinButton widget as it is found in e.g. tkGofer.


   ######################################################################### -}


module SpinButton (
        Spin(..),

        SpinButton,
        spinbutton,
        newSpinButton

        ) 
where

import HTk
import Button
import BitMap
import Interaction()

import WBFiles
import Debug(debug)


-- --------------------------------------------------------------------------
-- SpinButton Type 
-- --------------------------------------------------------------------------           
data SpinButton a = 
        SpinButton {
                fContainer :: Box,
                fButtonUp :: Button Spin,
                fButtonDown :: Button Spin,
                fCommand :: (Spin -> IO a)
        }

data Spin = Down | Up deriving (Eq,Ord)


-- --------------------------------------------------------------------------
-- Constructor 
-- --------------------------------------------------------------------------           
spinbutton :: [Config (SpinButton Spin)] -> IO (SpinButton Spin)
spinbutton ol = do {
        bt <- createSpinButton; 
        configure bt ol
        }


newSpinButton :: [Config (SpinButton a)] -> IO (SpinButton a)
newSpinButton ol = do {
        bt <- createSpinButton; 
        bt' <- mapTrigger (\_ -> raise undefinedEventHandler) bt;
        configure bt' ol
        }

createSpinButton :: IO (SpinButton Spin)
createSpinButton = do {
        b <- newVFBox [];
        bup <- newButton [
                        htkbitmap "ms_up_arrow.bm", 
                        command (\() -> return Up),
                        parent b];
        bdown <- newButton [
                        htkbitmap "ms_down_arrow.bm", 
                        command (\() -> return Down),
                        parent b];
        return (SpinButton b bup bdown return)
}
                
-- --------------------------------------------------------------------------
-- SpinButton Instances 
-- --------------------------------------------------------------------------           
instance Eq (SpinButton a) where 
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (SpinButton a) where 
        toGUIObject sb = toGUIObject (fContainer sb)
        cname _ = "SpinButton"

instance Destructible (SpinButton a) where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive (SpinButton a)

instance Widget (SpinButton a)
        
instance ChildWidget (SpinButton a)

instance Synchronized (SpinButton a) where
        synchronize w = synchronize (toGUIObject w)     

instance HasColour (SpinButton a) where 
        legalColourID _ _ = True
        setColour sb cid col = do {
                setColour (fContainer sb) cid col;
                setColour (fButtonUp sb) cid col;
                setColour (fButtonDown sb) cid col;
                return sb
                }

instance HasBorder (SpinButton a)

instance HasEnable (SpinButton a) where 
        state s sb = 
                synchronize sb (do {
                        foreach [(fButtonUp sb),(fButtonDown sb)] (state s);
                        return sb
                        })
        getState sb = getState (fButtonUp sb)

instance HasFont (SpinButton a) where
        font f sb  = 
                synchronize sb (do {
                        foreach [(fButtonUp sb),(fButtonDown sb)] (font f);
                        return sb
                        })
        getFont sb = getFont (fButtonUp sb)


instance HasSize (SpinButton a)

        
-- --------------------------------------------------------------------------
-- Interactions
-- --------------------------------------------------------------------------           
instance Reactive SpinButton a where
        triggered w = 
                        triggered (fButtonUp w)  >>>= (fCommand w)
                +>      triggered (fButtonDown w) >>>= (fCommand w)

instance HasTrigger SpinButton a where
        getTrigger = return . triggered 

instance Functor SpinButton where
        fmap f (SpinButton b bup bdown g) = 
                (SpinButton b bup bdown ((fmap f) . g))

instance HasMapTrigger SpinButton where
        mapTrigger f (SpinButton b bup bdown g) = 
                return (SpinButton b bup bdown (\x -> g x >>= f))

instance HasCommand SpinButton Spin a where
        command f w =  return (w {fCommand = f})

        
-- --------------------------------------------------------------------------
-- The bitmaps
-- --------------------------------------------------------------------------           

htkbitmap :: HasBitMap w => String -> Config w
htkbitmap fnm w = do {
        path <- getWBImageFilePath fnm;
        configure w [bitmap path];
        return w
}

