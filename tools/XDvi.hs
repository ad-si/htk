{- #########################################################################

MODULE        : XDvi
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : The xdvi tool encapsulation.
                


   ######################################################################### -}


module XDvi (
        Object(..),
        Tool(..),

        XDvi,
        xdvi
        ) where

import WB
import DialogWin
import LogWin
import Editor
import PromptWin
import Debug(debug)


-- --------------------------------------------------------------------------
-- Semantic Domains
-- --------------------------------------------------------------------------

data XDvi = XDvi Expect
                
-- --------------------------------------------------------------------------
--  Commands
-- --------------------------------------------------------------------------

xdvi :: FilePath -> IO XDvi
xdvi fname = do {
        dvi <- newExpect "xdvi" [arguments [fname]]; 
        return (XDvi dvi)
        } 


-- --------------------------------------------------------------------------
--  Tool Instance
-- --------------------------------------------------------------------------

instance Object XDvi where
        objectID (XDvi t) = objectID t

instance Tool XDvi where
        getToolStatus (XDvi t) = getToolStatus t

instance Destructible XDvi where
        destroy (XDvi t) = destroy t
        destroyed (XDvi t) = destroyed t
