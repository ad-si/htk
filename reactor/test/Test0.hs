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
import qualified IO
import Concurrency
import Debug(debug)

main =  
   do
      exp <- newExpect "cat" [arguments ["TEST1"]]
      pv <- newPVar 0
      inter <- newInterActor 
         (\iact -> 
               expect exp (".+") >>>=
                  (\ line ->
                     do
                        IO.putStr "line:"
                        IO.putStr line
                        IO.putStr "\n"
                        IO.hFlush stdout 
                        done
                     )
            +> matchEOF exp >>> 
                  do 
                     IO.putStr "end\n"
                     IO.hFlush stdout 
                     stop iact
            )
      sync(destroyed inter)
       
#if 0                       
                    expect exp ("^AWK\n",[Case_Insensitive]) >>> do {
                        changeVar' pv (+ 1);
                        putStr "awk\n";
                        done
                        }
                +>  expect exp ("^PERL\n", [Case_Insensitive]) >>> do {
                        putStr "perl\n"; 
                        destroy exp;
                        count <- getVar pv;
                        putStr ("caught " ++ show count ++ "\n");               
                        putStr "quitting\n"; 
                        }
                +>  matchEOF exp >>> do {
                        shutdown;
                        stop iact;
                        }
#endif 
