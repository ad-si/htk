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
import Moby
import Debug(debug)

main = do {
        mb <- moby "/home/uniol/jo/IekosSmall/A2";
        interactor (\iact ->
                destroyed mb >>> do {
                        putStr "moby has terminated\n";
                        shutdown;
                        stop iact
                        }
        );
        done
}




                
