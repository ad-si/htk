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
      inter <- newInterActor 
         (\iact -> 
               match exp ("^PERL") >>> 
                  do
                     debug "caught perl"
            +> match exp ("^awk") >>> 
                  do
                     debug "caught awk"
            +> matchLine exp >>>=
                  (\ line -> debug (line++"\n"))
            +> matchEOF exp >>> 
                  do
                      debug "Quitting interactor\n" 
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
      debug "About to delay"
      delay(secs 1)
      debug "End of delay"
      execCmd "exit\n" exp
      sync(destroyed inter)





