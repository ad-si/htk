{- ------------------------------------------------------------------------
 -
 - HTk Bug:
 -
 - Dialog windows opened from within interactors can't be closed. 
 - (They can now - this is a test case)
 - ------------------------------------------------------------------------ -}

module Main (
   main
   ) where

import HTk
import Concurrency(block)
import PulldownMenu
import Frame
import Label
import Entry
import Keyboard

--import PromptWin
import DialogWin

import Selective(deadlock)
import IO(stdout)
import Debug(debug)

main = 
   do
      htk<- htk []
      do 
         f   <- newVBox []        
         win <- window f [text "My first HTk Program"]
         bt  <- newButton  [text "Quit",
                    command (\()-> done),
                            parent f]
         interactor <- 
            newInterActor (
               \iact -> triggered bt >>> really_quit win iact
               )

         sync (destroyed interactor)
         debug "sync ended"
         destroy htk
   where 
      really_quit win iact = 
         do 
            debug ("Waiting for yes")
            yes<- newConfirmWin "Really close?" [modal True]
            debug ("Yes received " ++ show yes)
            if yes 
               then
                  do 
                     destroy win
                     stop iact
               else 
                  done

				
	



