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
import Debug(debug)


main = 
   do
      args <- getArgs
      print (head args)
      imports <- getTheoryImports (head args)
      putStr ("imports: " ++ show imports ++ "\n")
      done

