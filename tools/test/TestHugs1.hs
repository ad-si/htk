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
import Hugs
import System
import IO(stdout)
import Debug(debug)


main = do {
        gui <- htk [{- logfile (1::Int), -} text "HUGS"];
        setLogFile (Just stdout);
        args <- getArgs;
        t <- newHugs (args::[FilePath]) [];
        sync (destroyed t);
        shutdown;
        done
}
