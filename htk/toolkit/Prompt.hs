{- #########################################################################

MODULE        : Prompt
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : A composite prompt widget, like the one outlined by TkGofer. 


   ######################################################################### -}


module Prompt (
        Prompt,
        newPrompt,

        getPromptEntry
        ) 
where

import Core
import HTk
import Label
import Entry
import Debug(debug)

-- --------------------------------------------------------------------------
-- Definition 
-- --------------------------------------------------------------------------           
data GUIValue a => Prompt a = Prompt Box (Label String) (Entry String)


-- --------------------------------------------------------------------------
-- Commands 
-- --------------------------------------------------------------------------           
newPrompt :: GUIValue a => Box -> [Config (Prompt a)] -> IO (Prompt a)
newPrompt par ol =  do {
        b <- newHBox par [];
	pack b [Expand On, Fill X];
        lbl <- newLabel b []; 
	pack lbl [Expand Off, Fill X];
        ent <- newEntry b [];
        pack ent [Fill X, Expand On];
        configure (Prompt b lbl ent) ol
}


-- --------------------------------------------------------------------------
-- Instantiations 
-- --------------------------------------------------------------------------           
instance Eq (Prompt a) where 
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (Prompt a) where 
        toGUIObject (Prompt bx _ _) = toGUIObject bx
        cname _ = "Prompt"

instance Widget (Prompt a) where 
        cursor c pr @ (Prompt bx lbl ent) = 
                synchronize pr (do { 
                        cursor c bx; 
                        cursor c lbl; 
                        cursor c ent; 
                        return pr})

instance HasBorder (Prompt a)

instance HasSize (Prompt a) where
        height _ w  = return w
        getHeight _ = return 1

instance HasColour (Prompt a) where
        legalColourID _ _ = True
        setColour pr @ (Prompt bx lbl ent) cid c = 
                synchronize pr (do {
                        setColour bx cid c; 
                        setColour lbl cid c; 
                        return pr})

instance HasFont (Prompt a) where
        font f pr @ (Prompt bx lbl ent) = 
                synchronize pr (do {font f lbl; return pr})
        getFont (Prompt bx lbl ent) = getFont lbl


instance (GUIValue a, GUIValue b) => HasText (Prompt a) b where
        text t pr @ (Prompt _ lbl _) = do {text t lbl; return pr}
        getText (Prompt _ lbl _) = getText lbl

instance HasEnable (Prompt a) where
        state s pr @ (Prompt bx lbl ent) = do {state s ent; return pr}
        getState (Prompt bx lbl ent) = getState ent

instance HasVariable (Prompt a) where
        variable v pr @ (Prompt bx lbl ent) = 
                 synchronize pr (do {variable v ent; return pr})

--instance GUIValue a => Variable (Prompt a) a where
--        setVar pr@(Prompt _ _ ent) v = do {value v ent; done}
--        getVar (Prompt _ _ ent) = getValue ent
--        withVar w f = synchronize w (do {v <- getVar w; f v}) 
--        updVar w f = synchronize w (do {
--                v <- getVar w;
--                (v',r) <- f v;
--                setVar w v';
--                return r
--                })

instance Synchronized (Prompt a) where
        synchronize w = synchronize (toGUIObject w)
        

-- --------------------------------------------------------------------------
-- Entry Components 
-- --------------------------------------------------------------------------           
getPromptEntry :: Prompt a -> Entry String
getPromptEntry (Prompt _ _ ent) = ent
