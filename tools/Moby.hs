{- #########################################################################

MODULE        : Moby
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : The standard textedit tool for simple plain text editing.
                


   ######################################################################### -}


module Moby (
        ToolStatus(..),

        Object(..),
        Tool(..),

        Moby,
        moby
        ) where

import WB
import Debug(debug)


-- --------------------------------------------------------------------------
-- Semantic Domains
-- --------------------------------------------------------------------------

data Moby = Moby Observer
                
-- --------------------------------------------------------------------------
--  Commands
-- --------------------------------------------------------------------------

moby :: FilePath -> IO Moby
moby fname = do {
        moby <-getWBToolFilePath "moby";
        tool <- newObserver moby [arguments ["-w",fname]]; 
        return (Moby tool)
        } 


-- --------------------------------------------------------------------------
--  Tool Instance
-- --------------------------------------------------------------------------

instance Object Moby where
        objectID (Moby tool) = objectID tool

instance Tool Moby where
        getToolStatus (Moby tool) = getToolStatus tool

instance UnixTool Moby where
        getUnixProcessID (Moby tool) = getUnixProcessID tool

instance Destructible Moby where
        destroy (Moby tool) =  destroy tool
        destroyed (Moby tool) = destroyed tool
