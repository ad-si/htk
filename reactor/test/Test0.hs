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
               matchLine exp >>>=
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
                     putStr "about to stop" 
                     IO.hFlush stdout
                     stop iact
            )
      putStr "returned from newInterActor"
      sync(destroyed inter)
      shutdown



