{- #########################################################################

MODULE        : Tas
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : The tas tool encapsulation.             


   ######################################################################### -}


module Tas (
        Object(..),
        Tool(..),

        Tas,
        tas,

        getTheoryImports
        ) where

import WB
import DialogWin
import Expect
import ExtendedPrelude
import Debug(debug)


-- --------------------------------------------------------------------------
-- Semantic Domains
-- --------------------------------------------------------------------------

data Tas = Tas Expect
                
-- --------------------------------------------------------------------------
--  Commands
-- --------------------------------------------------------------------------

-- tas and CSPDemo and other instances of TAS.

tas :: String -> FilePath -> Maybe FilePath -> [Config PosixProcess] -> IO Tas
tas toolname script state confs = do {  
        tasfp <- getWBToolFilePath toolname;
        exp <- newExpect tasfp confs; 
        sync (matchLine exp);
        sync (matchLine exp);
        sync (match exp "^- ");
        startcmd <- startCmd script state;
        execCmd startcmd exp;
        interactor (\iact -> 
                tasfailed exp >>> do {
                        try(execCmd "quit();\n" exp);                   
                        stop iact
                        }               
           +>   matchLine exp >>> done
           +>   destroyed exp >>> do {print "TAS ABORT" ; stop iact}
                );
        return (Tas exp);
} where tasfailed exp =         
                match exp ("$uncaught exception.*\n",3::Int)
           +>   match exp ("^- ",3::Int)
           +>   match exp ("^\\+\\+\\+ TAS: user abort.*\n", 3::Int)
        startCmd script Nothing =  do {
                scf <- return script; 
                return ("startfile " ++ show scf ++ " ;\n")
                }
        startCmd scriptfile (Just statefile) = do {
                scf <- return scriptfile; 
                stf <- return statefile;
                state <- readFile stf;
                return ("resumefile " ++ show scf ++ " " ++ state ++ " ;\n")
                }


-- --------------------------------------------------------------------------
--  Tool Instance
-- --------------------------------------------------------------------------

instance Object Tas where
        objectID (Tas tool) = objectID tool

instance Tool Tas where
        getToolStatus (Tas tool) = getToolStatus tool

instance UnixTool Tas where
        getUnixProcessID (Tas tool) = getUnixProcessID tool

instance Destructible Tas where
        destroy (Tas tool ) =  destroy tool
        destroyed (Tas tool) = destroyed tool



-- --------------------------------------------------------------------------
--  Tool Instance
-- --------------------------------------------------------------------------

getTheoryImports :: FilePath -> IO [String]
getTheoryImports fname = do {   
        exp <- newExpect "cat" [
                        arguments [fname],
                        pollinterval (Just (secs 0.1))
                        ];
        print modname;
        ch <- newChannel;
        interactor (chaser exp ch []);
        sync (receive ch);
} where chaser exp ch imports iact = 
                matchLine exp >>>= (\str -> do {
                        case (fetchImports str) of
                                [] -> done
                                imp -> do {
                                        print imp;
                                        become iact (chaser exp ch (imp ++ imports) iact)
                                        }
                        })
           +>   matchEOF exp >>> do {sendIO ch imports; stop iact}
        pattern = "^" ++ modname ++ ".*\n$"
        fetchImports str | modname == (take (length modname) str) =
                filter isImport (words str)
        fetchImports str = []
        isImport "+" = False
        isImport "=" = False
        isImport w | w == modname  = False 
        isImport w   = True 
        modname = head (split (== '.') fname')
        fname'  = let names = split (== '/') fname in
                        names !! (length names - 1)
        



