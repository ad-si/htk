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
import RegularExpression
import IO(stdout)

import System
import Debug(debug)

main =  
   do
      exp <- newExpect "csh" [arguments ["-sf"]];
{-      sync (
         match exp ("^.+%") >>>=
            (\ matchResult ->
               putStr ("Prompt\n Matched:"++(show (getMatched matchResult))++
                   "\n Substrings:"++(show(getSubStrings matchResult))++"\n")
               )
         )
-}
      inter <- newInterActor 
         (\iact -> 
               match exp ("^PERL") >>> 
                  do
                     print "caught perl"
                      
            +> match exp ("^awk") >>> 
                  do
                     print "caught awk"
            +> matchLine exp >>>=
                  (\ line -> putStr (line++"\n"))
            +> matchEOF exp >>> 
                  do
                      putStr "Quitting interactor\n" 
                      stop iact
            )
      execCmd "echo awk\n" exp
      execCmd "echo awk\n" exp
      execCmd "echo awk\n" exp
      execCmd "echo awk\n" exp
      execCmd "echo AWK\n" exp
      execCmd "echo AWK\n" exp
      execCmd "echo AWK\n" exp
      execCmd "echo AWK\n" exp
      execCmd "echo AWK\n" exp
      execCmd "echo AWK\n" exp
      execCmd "echo PERL\n" exp
      delay(secs 1)
      execCmd "exit" exp
      sync(destroyed inter)




