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
import Concurrency
import Debug(debug)

main =  
   do 
      exp <- newExpect "cat" [arguments ["TEST1"]]
      pv <- newPVar 0
      inter <- newInterActor 
         (\iact -> 
               expect exp ("^AWK\n") >>> 
                  do
                     changeVar' pv (+ 1)
                     putStr "awk\n"
                     done
            +> expect exp ("^PERL\n") >>> 
                  do
                     putStr "perl\n"
                     destroy exp
                     count <- getVar pv
                     putStr ("caught " ++ show count ++ "\n")               
                     putStr "quitting\n"
                     shutdown
                     stop iact
            +> matchEOF exp >>> 
                  do
                     shutdown
                     stop iact
            )
      sync(destroyed inter)

