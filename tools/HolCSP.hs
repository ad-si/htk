{- #########################################################################

MODULE        : HolCSP
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : The tas tool encapsulation.             


   ######################################################################### -}


module HolCSP (
        Object(..),
        Tool(..),

        HolCSP,
        holCSP
        ) where

import WB
import DialogWin
import Expect
import Debug(debug)


-- --------------------------------------------------------------------------
-- Semantic Domains
-- --------------------------------------------------------------------------

newtype HolCSP = HolCSP Expect

                
-- --------------------------------------------------------------------------
--  Commands
-- --------------------------------------------------------------------------

-- tas and CSPDemo and other instances of TAS.

holCSP :: String -> FilePath -> FilePath -> Maybe FilePath -> [Config PosixProcess] -> IO HolCSP
holCSP toolname wd scriptf mbstatef confs = do {
        tasfp <- getWBToolFilePath toolname;
        exp <- newExpect tasfp confs; 
--      sync (matchLine exp);
        sync (matchLine exp);
        sync (match exp "^- ");
        wdcmd <- return ("set_external_dir "++(show wd)++";\n");
        execCmd wdcmd exp;
        startcmd <- startCmd scriptf mbstatef;
        execCmd startcmd exp;
        interactor (\iact -> 
                tasfailed exp >>> do {
                        try(execCmd "quit();\n" exp);                   
                        stop iact
                        }               
           +>   matchLine exp >>> done
           +>   destroyed exp >>> do {print "HOLCSP ABORT" ; stop iact}
                );
        return (HolCSP exp);
} where tasfailed exp =         
                match exp ("$uncaught exception.*\n",3::Int)
           +>   match exp ("^- ",3::Int)
           +>   match exp ("^\\+\\+\\+ TAS: user abort.*\n", 3::Int)
        startCmd scriptfile Nothing =  do {
                return ("startfile " ++ show scriptfile ++ " ;\n")
                }
        startCmd scriptfile (Just statefile) =  do {
                state <- readFile statefile;
                return ("resumefile " ++ show scriptfile ++ " " ++ state ++ " ;\n")
                }

-- --------------------------------------------------------------------------
--  Tool Instance
-- --------------------------------------------------------------------------

instance Object HolCSP where
        objectID (HolCSP tool) = objectID tool

instance Tool HolCSP where
        getToolStatus (HolCSP tool) = getToolStatus tool

instance UnixTool HolCSP where
        getUnixProcessID (HolCSP tool) = getUnixProcessID tool

instance Destructible HolCSP where
        destroy (HolCSP tool ) =  destroy tool
        destroyed (HolCSP tool) = destroyed tool
