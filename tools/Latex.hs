{- #########################################################################

MODULE        : Latex
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : The latex tool encapsulation.
                


   ######################################################################### -}


module Latex (
        Object(..),
        Tool(..),

        Latex,
        newLatex
        ) where

import WB
import DialogWin
import LogWin
import Editor
import PromptWin
import Interaction()
import Debug(debug)
import Expect
import Variable
import SIMClasses
import Window(transient,modal)

-- --------------------------------------------------------------------------
-- Semantic Domains
-- --------------------------------------------------------------------------

data Latex = Latex Expect LogWin
                
-- --------------------------------------------------------------------------
--  Commands
-- --------------------------------------------------------------------------

newLatex :: String -> FilePath -> IO Latex
newLatex fname wd = do {
        ltx <- newExpect "latex" [arguments [fname], workingdir wd]; 
        lwin @ (LogWin win _) <- newLogWin [text "Latex Log"];
        pv <- newPVar "";
        interactor (\iact -> 
                expect ltx ("^.*LaTeX Error: File.*\n", 1::Int) >>>= 
                        (setVar pv)
           +>   expect ltx ("^Enter file name: ",1::Int) >>> do {
                        msg <- getVar pv;
                        mfname <- newPromptDialog (take (length msg -1) msg) "" [
                                        text "Latex Error",
                                        transient win, 
                                        modal True
                                        ];
                        case mfname of
                                Nothing -> do {destroy ltx; stop iact}
                                (Just fname) -> execCmd (fname ++ "\n") ltx;
                        }               
           +>   expect ltx "^\\? " >>>  execCmd "\n" ltx
           +>   expect ltx "^.*\n" >>>= (writeLogWin lwin)
           +>   destroyed win >>> do {destroy ltx; stop iact}
                );
        return (Latex ltx lwin);
        } 

-- --------------------------------------------------------------------------
--  Tool Instance
-- --------------------------------------------------------------------------

instance Object Latex where
        objectID (Latex tool _) = objectID tool

instance Tool Latex where
        getToolStatus (Latex tool _) = getToolStatus tool

instance Destructible Latex where
        destroy (Latex tool lwin) =  do {
                try(destroy tool);
                try(destroy lwin); 
                done
                }
        destroyed (Latex tool _) = destroyed tool
