{- #########################################################################

MODULE        : Space
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : alpha
DESCRIPTION   : A space widget, inspired by the Haggis approach to
                defining layout.

                The space if only effective if used within a container.
                This container is assumed to be a box.

   ######################################################################### -}


module Space (
        Space,
        newSpace
        ) where

import HTk
import Frame
import Debug(debug)

-- --------------------------------------------------------------------------
-- Space Space 
-- --------------------------------------------------------------------------           
data Space = Space Distance Frame

-- --------------------------------------------------------------------------
-- Commands 
-- --------------------------------------------------------------------------           
newSpace :: Distance -> [Config Space] -> IO Space
newSpace d ol = do {
        f <- newFrame []; 
        configure (Space d f) (defaults : ol)
} where defaults = orient Vertical



-- --------------------------------------------------------------------------
-- Instances 
-- --------------------------------------------------------------------------           
instance Eq Space where
        (Space _ f1) == (Space _ f2) = f1 == f2

instance GUIObject Space where 
        toGUIObject (Space d f) = toGUIObject f
        cname _ = "Space"

instance Destructible Space where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive Space

instance Widget Space

instance ChildWidget Space 

instance Synchronized Space where
        synchronize w = synchronize (toGUIObject w)
        
instance HasColour Space where
        legalColourID = hasBackGroundColour

instance HasOrientation Space where
        orient Horizontal s @ (Space d f) = do {
                 configure f [fill Vertical, width d];
                 return s
                }
        orient Vertical s @ (Space d f) = do {
                 configure f [fill Horizontal, height d];
                 return s
                }


