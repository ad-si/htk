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
import Expect
import IO(stdout)

import System
import Debug(debug)

main =  do {
        setLogFile (Just stdout);
        exp <- newExpect "csh" [arguments ["-s"]];
        sync (expect exp ("`tty`:.",[Case_Insensitive,Multi_Line]) >>> 
                        putStr "Caught tty..\n");
        sync (matchLine exp >>> putStr "Caught Amb..\n");
        interactor (\iact -> 
                    expect exp ("^PERL\n",[Case_Insensitive]) >>> do {
                        print "caught perl"; 
                        }
                +>  expect exp ("^awk\n",[Case_Insensitive]) >>> do {
                        print "caught awk"; 
                        }
                +>  matchEOF exp >>> do {
                        putStr "Quitting interactor\n"; 
                        stop iact
                        }
                );
        execCmd "echo awk\n" exp;
        execCmd "echo awk\n" exp;
        execCmd "echo awk\n" exp;
        execCmd "echo awk\n" exp;
        execCmd "echo AWK\n" exp;
        execCmd "echo AWK\n" exp;
        execCmd "echo AWK\n" exp;
        execCmd "echo AWK\n" exp;
        execCmd "echo AWK\n" exp;
        execCmd "echo AWK\n" exp;
        execCmd "echo PERL\n" exp;
        delay(secs 1);
        shutdown;
        block
        }       



