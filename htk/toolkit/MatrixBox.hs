{- #########################################################################

MODULE        : MatrixBox
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : A simple box for matices.


   ######################################################################### -}


module MatrixBox (
        MatrixBox,
        newMatrixBox,

        newRow

        ) where

import Concurrency
import HTk
import Box
import Debug(debug)

-- --------------------------------------------------------------------------
-- Horizontal/Vertical Box 
-- --------------------------------------------------------------------------           
data MatrixBox = MatrixBox Box (PVar [Box])  


-- --------------------------------------------------------------------------
-- Commands 
-- --------------------------------------------------------------------------           
newMatrixBox :: [Config MatrixBox] -> IO MatrixBox
newMatrixBox confs = do {
        b <- newBox Flexible []; 
        pv <- newPVar []; 
        configure (MatrixBox b pv) confs
        }

newRow :: MatrixBox -> IO ()
newRow mb@(MatrixBox b pv) = synchronize mb (do {
        o <- getOrient mb;
        bn <- case o of {Vertical -> newHFBox []; Horizontal -> newVFBox []};
        changeVar' pv (bn:);
        col <- getBackground b;
        crs <- getCursor b;
        configure bn [parent b, bg col, cursor crs];
        done    
        })


-- --------------------------------------------------------------------------
-- Instances 
-- --------------------------------------------------------------------------           
instance Eq MatrixBox where 
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject MatrixBox where 
        toGUIObject (MatrixBox w _ ) = toGUIObject w
        cname _ = "MatrixBox"

instance Destructible MatrixBox where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive MatrixBox

instance Synchronized MatrixBox where
        synchronize w = synchronize (toGUIObject w)

instance Widget MatrixBox where
        cursor c mb@(MatrixBox b pv) = synchronize mb (do {
                cursor c b;
                bxs <- getVar pv;
                foreach bxs (cursor c);
                return mb
                })

instance ChildWidget MatrixBox

instance (Widget wc,ChildWidget wc) => ParentWidget MatrixBox wc where
        parent mb@(MatrixBox b pv) wc = synchronize mb (do {
                bl <- getVar pv; 
                case bl of 
                        (b:_) -> configure wc [parent b] >> return wc
                        []    -> newRow mb >> parent mb wc
                })

instance HasBorder MatrixBox

instance HasColour MatrixBox where 
        legalColourID = hasBackGroundColour
        setColour mb@(MatrixBox b pv) cid v = synchronize mb (do {
                setColour b cid v;
                bxs <- getVar pv;
                foreach bxs (\b -> setColour b cid v);
                return mb
                })
        getColour mb@(MatrixBox b pv) = getColour b

instance HasOrientation MatrixBox where
        orient o mb @ (MatrixBox b pv) = do {orient o b; return mb}
        getOrient (MatrixBox b pv) = getOrient b

instance HasSize MatrixBox

