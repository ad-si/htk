{- #########################################################################

MODULE        : Dvips
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : The latex tool encapsulation.
                


   ######################################################################### -}


module Dvips (
        Object(..),
        Tool(..),

        Dvips,
        newDvips
        ) where

import WB
import Expect
import DialogWin
import LogWin
import Editor
import PromptWin
import Debug(debug)


-- --------------------------------------------------------------------------
-- Semantic Domains
-- --------------------------------------------------------------------------

data Dvips      = Dvips Expect LogWin
                
-- --------------------------------------------------------------------------
--  Commands
-- --------------------------------------------------------------------------

newDvips :: FilePath -> FilePath -> [String] -> FilePath -> IO Dvips
newDvips fname psfile args' wd = do {
        dvi <- newExpect "dvips" confs; 
        lwin <- newLogWin [text "Dvips"];
        interactor (\iact -> 
                matchLine dvi >>>= (writeLogWin lwin)
                );
        registerTool (Dvips dvi lwin);
        return (Dvips dvi lwin)
        } where confs = [arguments (args' ++ ["-o",psfile,fname]),
                         workingdir wd
                        ]


-- --------------------------------------------------------------------------
--  Tool Instance
-- --------------------------------------------------------------------------

instance Object Dvips where
        objectID (Dvips tool _) = objectID tool

instance Tool Dvips where
        getToolStatus (Dvips tool _) = getToolStatus tool

instance Destructible Dvips where
        destroy (Dvips tool lwin) =  do {
                destroy tool;
                try (destroy lwin);
                done
                }
        destroyed (Dvips tool _) = destroyed tool >>> deregisterTool tool
