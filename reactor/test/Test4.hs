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


import SIM

import ChildProcess
import System
import qualified Posix
import Concurrency(block)
import Debug(debug)

main = do {
        chp <- newChildProcess "csh" parms;
        forkIO (reader chp);
        forkIO (writer chp msg);
        block;
        done
} where parms = [arguments ["-s"], linemode False]
        msg = "echo ABCDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDABCDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD"


reader chp = do {
        msg <- readMsg chp;
        putStr ("Reading:" ++ msg ++ "\n");
        putStr ("Length =" ++ show (length msg) ++ "\n");
        reader chp
}


writer chp msg = do {
        putStr ("Writing:" ++ msg ++ "\n");
        putStr ("Length =" ++ show (length msg) ++ "\n");
        sendMsg chp (msg ++ "\n");
        delay (secs 0.1);
        writer chp msg  
}
