{- #########################################################################

MODULE        : TextEdit
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : The standard textedit tool for simple plain text editing.
                


   ######################################################################### -}


module TextEdit (
        ToolStatus(..),
        Object(..),
        Tool(..),
        UnixTool(..),
        Destructible(..),
        Collectible(..),

        TextEditSource(..),
        TextEdit
        ) where

import WB
import Debug(debug)



-- --------------------------------------------------------------------------
-- Handle
-- --------------------------------------------------------------------------

newtype TextEdit = TextEdit Expect
        
-- --------------------------------------------------------------------------
-- Classes
-- --------------------------------------------------------------------------

class TextEditSource o where
        newTextEdit :: o -> IO TextEdit
        browse      :: o -> IO TextEdit 

        
-- --------------------------------------------------------------------------
--  Commands
-- --------------------------------------------------------------------------

instance TextEditSource FilePath where
        newTextEdit fname = do
                exp <- newExpect "textedit" [arguments [fname]]
                return (TextEdit exp)
        browse fname =  do
                exp <- newExpect "textedit" [arguments ["-read_only",fname]] 
                return (TextEdit exp)


-- --------------------------------------------------------------------------
--  Tool Instance
-- --------------------------------------------------------------------------

instance Object TextEdit where
        objectID (TextEdit exp) = objectID exp

instance Tool TextEdit where
        getToolStatus (TextEdit exp) = getToolStatus exp

instance UnixTool TextEdit where 
        getUnixProcessID (TextEdit exp) = getUnixProcessID exp

instance Destructible TextEdit where
        destroy (TextEdit exp)   = destroy exp
        destroyed (TextEdit exp) = destroyed exp

instance Collectible TextEdit where
        getCollectibleObj (TextEdit exp) = getCollectibleObj exp
