{- ------------------------------------------------------------------------
 -
 - This examples shows how HTk drops events.
 -
 - Author: cxl 
 - $Revision$ from $Date$  
 -
 - ------------------------------------------------------------------------ -}

module Main (main) where

import HTk
import Mouse
import Canvas
import Debug(debug)

main :: IO ()
main = do
        tk <- htk []
	cnv <- newCanvas [size (cm 15, cm 15), background "white"]
	win <- window cnv [text "HTk Event Tracker"]
	interactor (moving cnv)
	sync (destroyed win)
	destroy tk

	where  moving :: Canvas-> InterActor-> IA()
	       moving c i = 
		     (mouseEvent c Motion
		      >>>= \ ((x, y), _) -> do let msg= "*** Mouse @"++"X"++
	                                              show x++ " Y"++ show y
 			 		       debug msg
					       putStrLn msg)
		  +> (mouseButtonPress c 1
		      >>>= \ (x, y) -> do let msg= "*** Button @"++"X"++
	                                           show x++ " Y"++ show y
 			 		  debug msg
					  putStrLn msg)
