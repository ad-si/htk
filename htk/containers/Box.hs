{- #########################################################################

MODULE        : Box
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : A simple horizontal/vertical box for widgets.


   ######################################################################### -}


module Box (
        Flexibility(..),
        Box,

        newBox,
        newHBox,
        newVBox,
        newHFBox,
        newVFBox        

) where

import Concurrency
import GUICore
import Packer
import Frame
import Debug(debug)

-- --------------------------------------------------------------------------
-- Horizontal/Vertical Box 
-- --------------------------------------------------------------------------           
data Flexibility = Rigid | Flexible

data Box = Box Frame (PVar Orientation) Flexibility 


-- --------------------------------------------------------------------------
-- Commands 
-- --------------------------------------------------------------------------           
newBox :: Flexibility -> [Config Box] -> IO Box
newBox fl confs = do 
        f <- newFrame []
        pv <- newPVar cdefault 
        configure (Box f pv fl) confs

                
newHBox :: [Config Box] -> IO Box
newHBox ol = newBox Rigid ((orient Horizontal) : ol)                                     
newVBox :: [Config Box] -> IO Box
newVBox ol = newBox Rigid ((orient Vertical) : ol)

newHFBox :: [Config Box] -> IO Box
newHFBox ol = newBox Flexible ((orient Horizontal) : ol)

newVFBox :: [Config Box] -> IO Box
newVFBox ol = newBox Flexible ((orient Vertical) : ol)


-- --------------------------------------------------------------------------
-- Instances 
-- --------------------------------------------------------------------------           
instance Eq Box where 
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject Box where 
        toGUIObject (Box f _ _) = toGUIObject f
        cname _ = "Box"

instance Destructible Box where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive Box

instance Synchronized Box where
        synchronize w = synchronize (toGUIObject w)

instance Widget Box

instance ChildWidget Box

instance (Widget wc,ChildWidget wc) => ParentWidget Box wc where
        parent (Box f pv fl) wc = do {
                packW wc;
                o <- getVar pv; 
                packInBox f o fl wc;
                return wc
                }

instance HasBorder Box

instance HasColour Box where 
        legalColourID = hasBackGroundColour

instance HasOrientation Box where
        orient o b @ (Box f pv s) = do {setVar pv o; return b}
        getOrient (Box f pv s) = getVar pv

instance HasSize Box


-- --------------------------------------------------------------------------
-- Pack Command 
-- --------------------------------------------------------------------------           
packInBox :: (Widget wc, ChildWidget wc) => 
                Frame -> Orientation -> Flexibility -> wc -> IO () 
packInBox f Horizontal Rigid wc = 
        mpack wc (do {configure wc [side AtLeft]; parent f wc; done})
packInBox f Vertical Rigid wc = 
        mpack wc (do {configure wc [side AtTop]; parent f wc; done})
packInBox f Horizontal _ wc = 
        mpack wc (do {configure wc [side AtLeft,flexible]; parent f wc; done})
packInBox f Vertical _ wc = 
        mpack wc (do {configure wc [side AtTop,flexible]; parent f wc; done})

mpack :: (Widget wc, ChildWidget wc) => wc -> IO () -> IO ()
mpack w cmd = do {
        t <- hasParentObject (toGUIObject w);
        unless t cmd
        }


