{- ------------------------------------------------------------------------
 -
 - HTk Examples: Menus
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
import Mouse
import Menu
import PulldownMenu
import RadioGroup
import Separator
import Bell(bell)

import IO (stdout)

main = do
	htk []
	-- setLogFile (Just stdout)

        f   <- newHFBox [relief Raised]        
	win <- window f [text "Menus!"]

        mb <- newMenuButton [text "First Menu", parent f]
        m  <- newPulldownMenu mb [tearOff Off]
        c1 <- newCheckItem m [text "Check", command (selected "C1")]
        b1 <- newMenuItem m [text "Menu Item", command (clicked "B1")]
        newSeparator [parent m]
	cm <- newCascadeMenu [text "Cascade", parent m]
     	r1 <- newRadioButton [text "Select 1", parent (cm::Menu()),
			      command (selected "R1")]
	r2 <- newRadioButton [text "Select 2", parent cm,		
			      command (selected "R2")]
	r3 <- newRadioButton [text "Select 3", parent cm,		
			      command (selected "R2")]

	g1 <- radiogroup [r1, r2, r3]

	-- The two radiobuttons should actually be mutually exclusive
	-- (selecting deselects the other) -- but they are not! 
	-- I think this is a bug in HTk.


        c2 <- newCheckItem m [text "Check", command (selected "C2")]

	mb2 <- newMenuButton [text "Second Menu", parent f]
        m2 <- newPulldownMenu mb2 [tearOff Off]
        b2 <- newMenuItem m2 [text "Some Text Item", command (clicked "B2")]
	b3 <- newMenuItem m2 [text "Some Other Item",	command (clicked "B3")]
	
	ra <- newRadioButton [text "Select A", parent m2]		
			      -- command (selected "RA")]
	rb <- newRadioButton [text "Select B", parent m2]		
			      -- command (selected "RB")]

	g2 <- radiogroup [(ra::RadioButton()), rb]	

	interactor (const (triggered b1 +> triggered c1 +> triggered c2
		        +> triggered r1 +> triggered r2 +> triggered r3
	                +> triggered b2 +> triggered b3))

	t1<- getTrigger g1; t2<- getTrigger g2
        interactor (const (t1 >>> done))

	
	block
	where selected nm t = putStrLn (case t of On -> nm ++ " selected!"
	                                          Off-> nm ++ " deselected!")
	                      >> bell 
	      clicked nm () = putStrLn (nm ++ " clicked!") >> bell
