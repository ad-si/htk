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
import HTk
import HolZ
import System
import qualified IO
import Debug(debug)

main = do {
        htk [text "HOLZTEST"];
        setLogFile (Just IO.stdout);
        args <- getArgs;
        t <- startHolZ wdir args;
        sync (destroyed t >>> putStr "HOLZ HAS TERMINATED\n");
        shutdown;
        done
} where wdir = "/home/ewk/programs/database/bin/"

startHolZ wdir [tnm] = holz "HOLZDemo" [workingdir wdir]
startHolZ _ _ = error "no HolZ tool found"


                
