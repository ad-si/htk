{- #########################################################################

MODULE        : Main
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : 


   ######################################################################### -}


module Main (
        main    
        ) where


import WB
import HolCSP
import System
import qualified IO
import Debug(debug)


main = do {
        htk [text "HOLCSPTEST"];
        setLogFile (Just IO.stdout);
        args <- getArgs;
        print args;
        t <- startTas wdir args;
        sync (destroyed t >>> putStr "HOLCSP HAS TERMINATED\n");
        shutdown;
        done
} where wdir = "/home/ewk/programs/database/bin"


startTas wdir [tnm,scriptfile] = 
        holCSP tnm scriptfile [workingdir wdir]
startTas wdir _ = 
        error "two parameters required"




                
