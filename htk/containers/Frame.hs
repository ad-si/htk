{- #########################################################################

MODULE        : Frame
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : frame widget


   ######################################################################### -}


module Frame (
        Frame, 
        newFrame

        ) where

import Concurrency
import GUICore
import Packer
import Debug(debug)


-- --------------------------------------------------------------------------
-- Frame Widget 
-- --------------------------------------------------------------------------           
data Frame = Frame GUIOBJECT deriving Eq


-- --------------------------------------------------------------------------
-- Constructor 
-- --------------------------------------------------------------------------           
newFrame :: [Config Frame] -> IO Frame
newFrame ol = do { 
        w <- createWidget FRAME;
        configure (Frame w) ol
}


-- --------------------------------------------------------------------------
-- Instances 
-- --------------------------------------------------------------------------           
instance GUIObject Frame where 
        toGUIObject (Frame w) = w 
        cname _ = "Frame"

instance Destructible Frame where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive Frame

instance Synchronized Frame where
        synchronize w = synchronize (toGUIObject w)

instance ChildWidget Frame

instance Widget Frame

instance ChildWidget wc => ParentWidget Frame wc where 
        parent wp wc = do {
                packW wc;               
                packWidget (toGUIObject wp) (toGUIObject wc) Nothing;
                return wc
                }

instance HasBorder Frame

instance HasColour Frame where 
        legalColourID = hasBackGroundColour

instance HasSize Frame



