{- #########################################################################

MODULE        : ComboBox
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : A composite combobox widget. 

TO BE DONE    : Controlling config options of the scrollbars


   ######################################################################### -}


module ComboBox (
        ComboBox(..),
        newComboBox,

        fListBox

        ) 
where

import HTk
import Label
import Entry
import ListBox
import ScrollBar
import ScrollBox
import Mouse
import Keyboard
import Space
import Interaction()
import Debug(debug)

-- --------------------------------------------------------------------------
-- Definition 
-- --------------------------------------------------------------------------           
data GUIValue a => ComboBox a = 
        ComboBox { 
                fComboBox :: Box, 
                fComboBoxEntry :: (Entry a),
                fScrollBox :: ScrollBox (ListBox [a]),
                fSpace1 :: Space,
                fSpace2 :: Space
                }

fListBox = fScrolledWidget . fScrollBox

-- --------------------------------------------------------------------------
-- Commands 
-- --------------------------------------------------------------------------           
newComboBox :: GUIValue a => [Config (ComboBox a)] -> IO (ComboBox a)
newComboBox confs = do {
        b <- newVBox [flexible];
        eb <- newHBox [fill Horizontal, parent b];
        ent <- newEntry [flexible, fill Horizontal, parent eb];
        sp1 <- newSpace (cm 0.5) [parent eb, fill None];
        sp2 <- newSpace (cm 0.3) [parent b, fill Horizontal];
        lb <- newListBox [flexible];
        scb <- newScrollBox lb [flexible,parent b];
        configure (ComboBox b ent scb sp1 sp2) confs
}


-- --------------------------------------------------------------------------
-- Instantiations 
-- --------------------------------------------------------------------------           
instance Eq (ComboBox a) where 
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (ComboBox a) where 
        toGUIObject cb = toGUIObject (fComboBox cb)
        cname _ = "ComboBox"

instance Destructible (ComboBox a) where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive (ComboBox a)

instance Widget (ComboBox a) 

instance ChildWidget (ComboBox a)

instance Synchronized (ComboBox a) where
        synchronize w = synchronize (toGUIObject w)
        
instance HasBorder (ComboBox a)

instance HasSize (ComboBox a)

instance HasColour (ComboBox a) where
        legalColourID _ _ = True
        setColour cb cid c = 
                synchronize cb (do {
                        setColour (fComboBox cb) cid c;
                        setColour (fComboBoxEntry cb) cid c;
                        setColour (fListBox cb) cid c;
                        setColour (fSpace1 cb) cid c;
                        setColour (fSpace2 cb) cid c;
                        return cb
                        })

instance HasFont (ComboBox a) where
        font f cb = 
                synchronize cb (do {
                        font f (fComboBoxEntry cb);
                        font f (fListBox cb);
                        return cb
                        })
        getFont cb = getFont (fComboBoxEntry cb)
        

instance GUIValue a => Variable ComboBox a where
        setVar cb v = do {value v (fComboBoxEntry cb); done}
        getVar cb   = getValue (fComboBoxEntry cb)
        withVar w f = synchronize w (do {v <- getVar w; f v}) 
        updVar w f = synchronize w (do {
                v <- getVar w;
                (v',r) <- f v;
                setVar w v';
                return r
                })


-- --------------------------------------------------------------------------
-- Events 
-- --------------------------------------------------------------------------           
instance (GUIValue a, Eq a) => Reactive ComboBox a where
        triggered cb = 
                keyPressed (fComboBoxEntry cb) "Return" >>> do {
                        (clearSelection (fListBox cb));
                        val <- getValue (fComboBoxEntry cb);
                        return val
                        }
          +>    triggered (fListBox cb) >>>= return . head


instance (GUIValue a, Eq a) => HasTrigger ComboBox a where
        getTrigger = return . triggered
 
