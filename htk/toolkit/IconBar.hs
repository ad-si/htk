{- #########################################################################

MODULE        : IconBar
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : IconBar Composite Widget



   ######################################################################### -}


module IconBar (
        module BitMap,

        IconBar,
        newIconBar,

        Button,

        getIconButtons,
        getIconBarItems
        
        ) 
where

import Concurrency
import HTk
import Button
import Separator
import BitMap
import Image
import Interaction()
import GUIIntrinsics
import Debug(debug)


-- --------------------------------------------------------------------------
-- IconBar Type 
-- --------------------------------------------------------------------------           
data IconBar a = IconBar Box (PVar [Either Separator (Button a)])


-- --------------------------------------------------------------------------
-- Commands 
-- --------------------------------------------------------------------------           
newIconBar :: [Config (IconBar a)] -> IO (IconBar a)
newIconBar ol = do 
        b <- newBox Rigid []
        em <- newPVar []
        configure (IconBar b em) ol
                

-- --------------------------------------------------------------------------
-- IconBar Instances 
-- --------------------------------------------------------------------------           
instance Eq (IconBar a) where 
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (IconBar a) where 
        toGUIObject (IconBar b e) = toGUIObject b
        cname _ = "IconBar"

instance Destructible (IconBar a) where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive  (IconBar a)

instance HasColour (IconBar a) where 
        legalColourID = hasForeGroundColour

instance Widget (IconBar a) where 
        cursor c ib@(IconBar b pv) = synchronize ib (do {
                configure b [cursor c];
                bts <- getIconButtons ib;
                foreach bts (cursor c);
                return ib
                })
        
instance ChildWidget (IconBar a)
        
instance HasSize (IconBar a)

instance HasBorder (IconBar a)

instance HasEnable (IconBar a) where
        state st ib = 
                synchronize ib (do {
                        ibs <- getIconButtons ib;
                        foreach ibs (\ib -> configure ib [state st]);
                        return ib
                        })
        getState ib = do {
                b <- isEnabled ib;
                if b then return Normal else return Disabled
                }
        isEnabled ib = 
                synchronize ib (do {
                        ibs <- getIconButtons ib;
                        sl <- sequence (map getState ibs);
                        return (foldr (||) False (map (/= Disabled) sl)) 
                        })
                

instance HasOrientation (IconBar a) where 
        orient o sb @ (IconBar b bts) = do {orient o b; return sb}
        getOrient (IconBar b bts) = getOrient b

instance Synchronized (IconBar a) where
        synchronize w = synchronize (toGUIObject w)

instance HasTrigger IconBar a where
        getTrigger ib = do
                ibs <- getIconButtons ib
                return (choose (map triggered ibs))


-- --------------------------------------------------------------------------
-- Parent/Child Relationship
-- --------------------------------------------------------------------------   

instance ParentWidget (IconBar a) (Button a) where
        parent ib @ (IconBar box pv) bt = 
                synchronize ib (do {
                        changeVar pv ( \ el -> do {
                                configure bt [parent box];
                                return ((Right bt) : el)
                                });
                        return bt
                        })

instance ParentWidget (IconBar a) Separator where
        parent ib@(IconBar box pv) sep = 
                synchronize ib (do {
                        changeVar pv ( \ el -> do {
                                configure sep [parent box];
                                return ((Left sep) : el)
                                });
                        return sep
                        })



-- --------------------------------------------------------------------------
-- Aux
-- --------------------------------------------------------------------------           
getIconButtons :: IconBar a -> IO [Button a]
getIconButtons (IconBar _ pv) = do {
        elems <- getVar pv;
        return (map (\(Right b) -> b) (buttons elems))
} where buttons elems = filter (either (\_ -> False) (\_ -> True)) elems


getIconBarItems :: IconBar a -> IO [Either Separator (Button a)]
getIconBarItems (IconBar _ pv) = getVar pv;
