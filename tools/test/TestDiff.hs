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
import Diff
import System
import Debug(debug)

main =
   do
      htk [text "DIFF" {- , logfile 1 -}]
      args <- getArgs
      print args
      (fname1,fname2) <- getFileNames args
      t <- newDiff (fname1::FilePath) (fname2::FilePath) []
      sync (destroyed t)
      try(shutdown)
      done
   where 
      getFileNames [x,y] = return (x,y)
      getFileNames _ = ioError(userError "diff: two file names required")
            


