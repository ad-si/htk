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
import Hugs
import System


main = do {
        args <- getArgs;
        print (head args);
        imports <- getImports (head args);
import Debug(debug)
        print ("imports: " ++ unlines imports);
        done
}
