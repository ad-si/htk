{- ------------------------------------------------------------------------
 -
 - HTk First Steps: Hello World with an entry widget
 -
 - Author: cxl 
 - $Revision$ from $Date$  
 -
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

import IO(stdout)


main = do
        htk<- htk []
	setLogFile(Just stdout)

        f   <- newVBox []        
	win <- window f [text "My third HTk Program"]
	mbt <- newMenuButton  [text "File", parent f, side AtLeft]
        mn  <- newPulldownMenu (mbt::MenuButton ()) [tearOff Off]
        bt  <- newButton  [text "Quit",
			   command (\()-> destroy win),
	                   parent mn]
	f1  <- newHBox [parent f]
        l   <- newLabel [value "Rename: ", parent f1, 
			 pad Horizontal (cm 1), side AtLeft]
        e   <- (newEntry [side AtRight, parent f1]):: IO (Entry String)

--      interactor (\iact -> userinteraction e (KeyPress (Just "Return")) Request 
	interactor (\iact -> keyPressed e "Return"
	                     >>> do {txt<- getVar e; win # text txt; done})


        interactor (\iact -> triggered bt >>> stop iact)

	sync (destroyed win)
	destroy htk
