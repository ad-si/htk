{- #########################################################################

MODULE        : LabelBox
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : A generic packer for associating a label with a widget.
                Ideally, we allow to place the text according to some sort
                of side or anchor specification rather than just
                according to the orientation which is presently the case.

   ######################################################################### -}


module LabelBox (
        LabelBox(..),
        newLabelBox,

        getLabelledWidget

        ) where

import Concurrency
import HTk
import Label
import Box
import Debug(debug)

                
-- --------------------------------------------------------------------------
--  Type
-- --------------------------------------------------------------------------

data (Widget a, ChildWidget a) => LabelBox a = 
        LabelBox {
                fLabelBox :: Box, 
                fLabel :: Label String,
                fLabelledWidget :: a
                }

                
-- --------------------------------------------------------------------------
--  Commands (Layout)
-- --------------------------------------------------------------------------

newLabelBox :: (Widget a, ChildWidget a) => a -> [Config (LabelBox a)] -> IO (LabelBox a)
newLabelBox w ol = do {
        b <- newHFBox [];
        lbl <- newLabel [parent b]; 
        configure w [fill Horizontal, parent b];
        configure  (LabelBox b lbl w) (defaults ++ ol)
} where defaults = [orient Horizontal]


                
-- --------------------------------------------------------------------------
--  Instances
-- --------------------------------------------------------------------------

instance Eq (LabelBox a) where 
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (LabelBox a) where 
        toGUIObject (LabelBox w _ _) = toGUIObject w
        cname _ = "LabelBox"

instance Destructible (LabelBox a) where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive (LabelBox a)

instance (Widget a,ChildWidget a) => Widget (LabelBox a) where 
        cursor c lb = 
                synchronize lb (do { 
                        cursor c (fLabelBox lb); 
                        cursor c (fLabel lb); 
                        cursor c (fLabelledWidget lb); 
                        return lb
                        })

instance (Widget a,ChildWidget a) => ChildWidget (LabelBox a) 

instance (Widget a,ChildWidget a) => HasBorder (LabelBox a)

instance (Widget a,ChildWidget a) => HasSize (LabelBox a)

instance (Widget a,ChildWidget a,HasColour a) => 
   HasColour (LabelBox a) where
        legalColourID _ _ = True
        setColour lb cid c = 
                synchronize lb (do {
                        setColour (fLabelBox lb) cid c; 
                        setColour (fLabel lb) cid c; 
                        return lb
                        })


instance (Widget a,ChildWidget a) => HasFont (LabelBox a) where
        font f lb = synchronize lb (do {font f (fLabel lb); return lb})
        getFont lb = getFont (fLabel lb)


instance (Widget a,ChildWidget a) =>  HasOrientation (LabelBox a) where 
        orient Vertical lb = 
                synchronize lb (do {
                        orient Vertical (fLabelBox lb);
                        configure (fLabel lb) [side AtTop, anchor West, fill None];
                        configure (fLabelledWidget lb) [side AtTop, anchor West];
                        return lb
                        })
        orient Horizontal lb = 
                synchronize lb (do {
                        orient Horizontal (fLabelBox lb);
                        configure (fLabel lb) [side AtLeft, anchor West];               
                        configure (fLabelledWidget lb) [side AtLeft, anchor West,fill Horizontal];
                        return lb
                        })
        getOrient lb = getOrient (fLabelBox lb)


instance (GUIValue b,Widget a,ChildWidget a) =>  HasText (LabelBox a) b where
        text t lb       = do {text t (fLabel lb); return lb}
        getText lb      = getText (fLabel lb)

instance (Widget a,ChildWidget a) => Synchronized (LabelBox a) where
        synchronize w = synchronize (toGUIObject w)
        
                
-- --------------------------------------------------------------------------
--  Selector
-- --------------------------------------------------------------------------

getLabelledWidget :: (Widget a, ChildWidget a) => LabelBox a -> a
getLabelledWidget = fLabelledWidget



 
