{- ------------------------------------------------------------------------
 -
 - HTk First Steps: Hello World with a button
 -
 - Author: cxl 
 - $Revision$ from $Date$  
 -
 - ------------------------------------------------------------------------ -}


module Main (
        main

        ) where

import HTk
import Concurrency
import PulldownMenu
import Frame
import Label

main = do
        htk []

        f   <- newFrame []        
	win <- window f [text "My second HTk Program"]
	mbt <- newMenuButton  [text "File", parent f, side AtTop]
        mn  <- newPulldownMenu (mbt::MenuButton ()) [tearOff Off]
        bt  <- newButton  [text "Quit",
			   command (\()-> destroy win),
	                   parent mn]
        l  <- newLabel  [value "Hello, world!", parent f, 
			 relief Groove, pad Horizontal (cm 1), side AtBottom]
        interactor (\iact -> triggered bt >>> stop iact)

        sync (destroyed win) 
        shutdown
