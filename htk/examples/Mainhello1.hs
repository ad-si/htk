{- ------------------------------------------------------------------------
 -
 - HTk First steps: the ubiquitous "Hello World!"
 -
 - Author: cxl 
 - $Revision$ from $Date$  
 -
 - ------------------------------------------------------------------------ -}

module Main (
        main 
        ) 
where

import HTk
import Concurrency


import Label
import Button
import Frame
import Window

import Random(randomRIO)
import IO(stdout)


randomColour :: IO (Int, Int, Int)
randomColour = do
   red  <-randomRIO(0,255)
   green<-randomRIO(0,255)
   blue <-randomRIO(0,255)
   return (red, green, blue)
      

main:: IO ()
main = do
        htk []  
        setLogFile (Just stdout)
	f  <- newFrame []        
        l  <- newLabel [value "Hello, world!", parent f]
	nb <- newButton [text "New Colour", 
			 command (\()-> do
			                  bunt<- randomColour
					  l # foreground bunt
                                 ), 
	                 parent f]
        window f [text "My first HTk program"] 
        interactor (\iact-> (triggered nb) >>> done)
        block
