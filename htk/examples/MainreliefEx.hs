{- ------------------------------------------------------------------------
 -
 - HTk Examples: Borders/Reliefs
 -
 - Author: cxl 
 - $Revision$ from $Date$  
 -
 - ------------------------------------------------------------------------ -}

module Main (main) where

import HTk
import Label
import Button
import Frame

main:: IO ()
main = do
         tk <- htk []
	 f  <- newVBox []
	 f1 <- newHBox  [parent f, pad Vertical 10]   
	 f2 <- newHBox  [parent f, pad Vertical 10]
         newLabel [value "Groove", relief Groove, borderwidth (mm 1),
                   parent f1, pad Horizontal 10, font bigfont]
	 newLabel [value "Ridge",  relief Ridge,  borderwidth (mm 1),
                   parent f1, pad Horizontal 10, font bigfont]
	 newLabel [value "Sunken", relief Sunken, borderwidth (mm 1),
                   parent f2, pad Horizontal 10, font bigfont]
	 newLabel [value "Raised", relief Raised, borderwidth (mm 1),
                   parent f2, pad Horizontal 10, font bigfont]
         win <- window f [text "Different Reliefs"] 
         sync (destroyed win)
         destroy tk

       where bigfont=  xfont {family = Just Lucida, weight = Just Bold, 
	        	      pixels = (Just 18)}
