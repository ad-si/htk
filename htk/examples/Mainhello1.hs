{- ------------------------------------------------------------------------
 -
 - HTk First steps: the ubiquitous "Hello World!"
 -
 - Author: cxl 
 - $Revision$ from $Date$  
 -
 - ------------------------------------------------------------------------ -}

module Main (main) where

import HTk
import Label
import Frame
import Button
import Random(randomRIO)

randomColour :: IO (Int, Int, Int)
randomColour = do
   red  <-randomRIO(0,255)
   green<-randomRIO(0,255)
   blue <-randomRIO(0,255)
   return (red, green, blue)
      
main:: IO ()
main = do
        tk <- htk []  
	f  <- newFrame []        
        l  <- newLabel [value "Hello, world!", parent f]
	nb <- newButton [text "New Colour", 
			 command (\()-> do
			                  bunt<- randomColour
					  l # foreground bunt
                                 ), 
	                 parent f]
        win <- window f [text "My first HTk program"] 
        interactor (\iact-> (triggered nb) >>> done)
        sync (destroyed win)
        destroy tk
