{- #########################################################################

MODULE        : Main
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : alpha
DESCRIPTION   : 


   ######################################################################### -}


module Main (
        main    
        ) where


import WB
-- import ImpChase
import System
import Char
import Debug(debug)

main = do {
        print "starting test";
        args <- getArgs;
        fname <- filename args;
--      t <- impchase fname;
--      print t;
        done
} where filename [fname] = return fname
        filename _  = do {
                putStr "test: argument must be a file name\n";
                exitWith (ExitFailure 1)
                }
