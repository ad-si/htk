{- #########################################################################

MODULE        : HolZ
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : The holz tool encapsulation.            


   ######################################################################### -}


module HolZ (
        Object(..),
        Tool(..),

        HolZ,
        holz,

        fetchZImports
        ) where

import WB

import DialogWin
import Expect
import ExtendedPrelude
import Debug(debug)


-- --------------------------------------------------------------------------
-- Semantic Domains
-- --------------------------------------------------------------------------

data HolZ = HolZ Expect
                
-- --------------------------------------------------------------------------
--  Commands
-- --------------------------------------------------------------------------

holz :: FilePath -> [Config PosixProcess] -> IO HolZ
holz toolname confs = do {      
        holzfp <- getWBToolFilePath toolname;
        exp <- newExpect holzfp confs; 
        sync (matchLine exp);
        sync (matchLine exp);
        sync (match exp "^- ");
        execCmd "go();\n" exp;
        interactor (\iact ->
                holzfailed exp >>> do {
                        try(execCmd "quit();\n" exp);                   
                        stop iact
                        }               
           +>   matchLine exp >>> done
           +>   destroyed exp >>> do {print "HOLZ ABORT" ; stop iact}
                );
        return (HolZ exp);
} where holzfailed exp =        
                match exp ("$uncaught exception.*\n",3::Int)
           +>   match exp ("^- ",3::Int)
           +>   match exp ("^\\+\\+\\+ TAS: user abort.*\n", 3::Int)


fetchZImports :: FilePath -> String -> [Config PosixProcess] -> IO [String]
fetchZImports toolname thyname confs = do {     
        holzfp <- getWBToolFilePath toolname;
        exp <- newExpect holzfp confs; 
        sync (matchLine exp);
        sync (matchLine exp);
        sync (match exp "^- ");
        execCmd (getImports thyname) exp;
        ans <- sync (
                holzfailed exp >>> do {
                        try(execCmd "quit();\n" exp);                   
                        raise (toolFailed "HOL-Z")
                        }               
           +>   matchLine exp
           +>   destroyed exp >>> raise (toolFailed "HOL-Z")
           );
        return (split (== ',') ans)
} where holzfailed exp =        
                match exp ("$uncaught exception.*\n",3::Int)
           +>   match exp ("^- ",3::Int)
           +>   match exp ("^\\+\\+\\+ TAS: user abort.*\n", 3::Int)
        getImports thyname = "bla bla"



-- --------------------------------------------------------------------------
--  Tool Instance
-- --------------------------------------------------------------------------

instance Object HolZ where
        objectID (HolZ tool) = objectID tool

instance Tool HolZ where
        getToolStatus (HolZ tool) = getToolStatus tool

instance UnixTool HolZ where
        getUnixProcessID (HolZ tool) = getUnixProcessID tool

instance Destructible HolZ where
        destroy (HolZ tool ) =  destroy tool
        destroyed (HolZ tool) = destroyed tool
