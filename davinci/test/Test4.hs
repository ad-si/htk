{- #########################################################################

MODULE        : GraphEditor

Modified version of Test2 (in this directory) for hacking purposes
(so I can find out how Test2 works . . .)


   ######################################################################### -}


module Main (
        main

        ) where

import Computation
import HTk
import DaVinci
import GraphEditor
import PulldownMenu
import Frame
import DialogWin
import Debug(debug)

main = 
   do
--      gui <- htk []
      dav <- davinci []
--      setErrorHandler err
      newGraphEditor dav
      interactor <- newInterActor(
         \iact -> 
               destroyed dav >>> 
                  do 
--                     destroy gui
                     stop iact
            +> lastGraphClosed dav >>> 
                  do 
--                     destroy gui 
                     destroy dav
                     stop iact
            )
      sync(destroyed interactor)

err e = newErrorWin (show e) []

