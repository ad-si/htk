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
import Debug(debug)


main = do {
        args <- getArgs;
        print (head args);
        imports <- getImports (head args);
        putStr ("imports: " ++ unlines imports);
        done
}
