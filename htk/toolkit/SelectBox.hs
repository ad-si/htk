{- #########################################################################

MODULE        : SelectBox
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : SelectBox Abstraction

   ######################################################################### -}


module SelectBox (
        SelectBox,
        newSelectBox,

        getDefault,
        selectDefault
        ) 
where

import Concurrency
import HTk
import Frame
import Button
import Space
import Interaction

import GUIIntrinsics
import Debug(debug)


-- --------------------------------------------------------------------------
-- SelectBox Type 
-- --------------------------------------------------------------------------           
data SelectBox a = SelectBox Box (Maybe (Frame,Int)) (PVar [Button a])

type Elements a = [Button a]


-- --------------------------------------------------------------------------
-- Commands 
-- --------------------------------------------------------------------------           
newSelectBox :: Maybe Int -> [Config (SelectBox a)] -> IO (SelectBox a)
newSelectBox Nothing ol = do {
        b <- newHBox [expand On, fill Horizontal];
        em <- newPVar [];
        configure (SelectBox b Nothing em) ol
}
newSelectBox (Just i) ol = do {
        b <- newHBox [expand On, fill Horizontal];
        em <- newPVar [];
        f <- newFrame [expand On,side AtLeft, relief Sunken, borderwidth 1];
        configure (SelectBox b (Just (f,i)) em) ol
}


-- --------------------------------------------------------------------------
-- SelectBox Instances 
-- --------------------------------------------------------------------------           
instance Eq (SelectBox a) where 
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance Destructible (SelectBox a) where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance GUIObject (SelectBox a) where 
        toGUIObject (SelectBox b _ e) = toGUIObject b
        cname _ = "SelectBox"

instance Interactive (SelectBox a)

instance HasColour (SelectBox a) where 
        legalColourID = hasForeGroundColour

instance Widget (SelectBox a)
        
instance ChildWidget (SelectBox a)      

instance ParentWidget (SelectBox a) (Button a) where
        parent = addSB

instance ParentWidget (SelectBox a) Space 

instance HasSize (SelectBox a)

instance HasBorder (SelectBox a)

instance HasEnable (SelectBox a) where
        state st sb@(SelectBox b _ em) = 
                synchronize sb (do {
                        ibs <- getVar em;
                        foreach ibs (\ib -> configure ib [state st]);
                        return sb
                        })
        getState sb = do {
                b <- isEnabled sb;
                if b then return Normal else return Disabled
                }
        isEnabled sb@(SelectBox b _ em) = 
                synchronize sb (do{
                        ibs <- getVar em;
                        sl <- sequence (map getState ibs);
                        return (foldr (||) False (map (/= Disabled) sl)) 
                        })


instance Synchronized (SelectBox a) where
        synchronize w = synchronize (toGUIObject w)


-- --------------------------------------------------------------------------
--  Selection 
-- --------------------------------------------------------------------------           
selectDefault :: SelectBox a -> IO ()
selectDefault sb = do {
        mbt <- getDefault sb;
        incase mbt (\bt -> do {flash bt;invoke bt})
        }


getDefault :: SelectBox a -> IO (Maybe (Button a))
getDefault (SelectBox b Nothing em) = return Nothing
getDefault (SelectBox b (Just (f,i)) em) = do {
        bts <- getVar em;
        return (Just (bts !! i));
        }



-- --------------------------------------------------------------------------
--  Elements 
-- --------------------------------------------------------------------------           
addSB :: (SelectBox a) -> Config (Button a)
addSB sb@(SelectBox b Nothing em) bt = 
        synchronize sb (do {
                changeVar em (\el -> do {
                        configure bt [parent b]; 
                        return (el ++ [bt])
                        });
                return bt
                })
addSB sb@(SelectBox b (Just (f,i)) em) bt = 
        synchronize sb (do {
                changeVar em (\el -> do {
                        if i == (length el - 1) then do {
                                configure bt [side AtLeft, parent b]; 
                                return (el ++ [bt])
                                }
                        else {- this is the default button -} do {
                                xp <- getPad Horizontal bt;
                                yp <- getPad Vertical bt;
                                configure f [pad Horizontal xp, pad Vertical yp, parent b];
                                configure bt [side AtLeft, pad Horizontal (cm 0.2), pad Vertical (cm 0.1),
                                                parent f];
                                return (el ++ [bt])
                                }
                        });
                return bt
                })



-- --------------------------------------------------------------------------
-- Trigger 
-- --------------------------------------------------------------------------           
instance HasTrigger SelectBox a where 
        getTrigger (SelectBox b _ em) = do
                bts <- getVar em
                return (choose (map triggered bts))

