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
import Latex
import Dvips
import DialogWin
import Debug(debug)

main = do {
        gui <- htk [{- logfile (1::Int) -}];
        tex <- newLatex "ROOT" "./test";
        sync (destroyed tex);
        putStr "latex has terminated\n";
        dvi <- newDvips "ROOT.dvi" "ROOT.ps" [] "./test";
        sync(destroyed dvi);                    
        putStr "dvips has terminated\n";
        newAlertWin "Test Finished" [];
        destroy gui;
        done
        }




                
