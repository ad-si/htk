{- ------------------------------------------------------------------------
 -
 - HTk Examples: Listboxes and Scrollbars 
 -
 - Author: cxl 
 - $Revision$ from $Date$  
 -
 - ------------------------------------------------------------------------ -}

module Main (main) where

import HTk
import ScrollBar
import ListBox
import Mouse

main :: IO ()
main = do
        tk <- htk []
        f   <- newHFBox []        
	win <- window f [text "A Listbox"]
        lb  <- newListBox [value numbers, bg "white", size (15,10),
			   side AtLeft, parent f]
	scb <- newScrollBar [parent f, side AtRight, fill Vertical]
	lb # scrollbar Vertical scb
	interactor (\iact -> mouseButtonPress lb 1
	                     >>> do {sel<- getSelection lb;
				     putStrLn ("Selected "++ 
					        show (sel:: Maybe [Int])); done})
        sync (destroyed win)
        destroy tk

       where numbers = 
	         ["One", "Two", "Three", "Four", "Five", "Six", "Seven",
		  "Eight", "Nine", "Ten", "Eleven", "Twelve", "Thirtheen",
		  "Fourteen", "Fifteen", "Sixteen", "Seventeen",
		  "Eighteen", "Nineteen", "Twenty"]
	
