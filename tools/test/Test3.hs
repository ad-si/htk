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
import Tas
import System


main = do {
        args <- getArgs;
--      setLogFile (Just 1);
        print (head args);
        imports <- getTheoryImports (head args);
import Debug(debug)
        putStr ("imports: " ++ show imports ++ "\n");
        done
}
