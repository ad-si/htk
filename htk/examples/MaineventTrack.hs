{- ------------------------------------------------------------------------
 -
 - This examples shows how HTk drops events.
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
import Line
import Oval
import Rectangle
import CanvasItem
import Mouse
import Canvas


import InfoBus(shutdown)

import Debug(debug)

import IO(stdout)

main = do
        htk<- htk []

	cnv <- newCanvas [size (cm 15, cm 15), background "white"]

	win <- window cnv [text "HTk Event Tracker"]

	interactor (moving cnv)
	
	sync (destroyed win)
	
	shutdown


	where  moving :: Canvas-> InterActor-> IA()
	       moving c i = 
		    (mouseEvent c (Button1, Motion) 
		      >>>= \ ((x, y), _) -> do let msg= "*** Mouse @"++"X"++
	                                              show x++ " Y"++ show y
 			 		       debug msg
					       putStrLn msg
					       putStrLn (show (fib 10)))
		     
fib :: Integer-> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
