{- ------------------------------------------------------------------------
 -
 - HTk Examples: Canvas #3
 -
 - This examples demonstrates the usefulness of real concurrency, and 
 - also shows that Tcl/Tk is too slow for applications with a lot of graphics. 
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
import Line
import Oval
import Rectangle
import CanvasItem
import Mouse
import Canvas
import Random(randomRIO)

import IO(stdout)


toInt :: Distance-> Int
toInt = fromInteger . toInteger

randomColour :: IO (Int, Int, Int)
randomColour = do
   red  <-randomRIO(0,255)
   green<-randomRIO(0,255)
   blue <-randomRIO(0,255)
   return (red, green, blue)

nextColour :: (Int, Int, Int)-> IO (Int, Int, Int)
nextColour (r, g, b) = do
   red  <-randomRIO(0,10)
   green<-randomRIO(0,10)
   blue <-randomRIO(0,10)
   return (mk (r+red), mk (g+green), mk (b+blue))
   where mk col = max (col `mod` 255) 25
  
 				
main = do
        win<- htk []
        -- setLogFile (Just stdout)

	f <- newVFBox[]
	win <- window f [text "Pretty Blobs"]
	cnv <- newCanvas [size (cm 15, cm 15), parent f,
		          background "black"]
	interactor (\iact-> mouseButtonPress cnv 1
			    >>>= \(x, y)-> do col<- randomColour
					      c<- colourDot cnv x y col
					      forkIO (sparkle c (x,y) col 0 255)
					      done)
        block
   where colourDot cnv x y col = newOval [parent cnv, filling col,
	  		         	  size (2, 2), position (x-1, y-1)]
	 sparkle p (x,y) col cnt fade = 
		if cnt >= 750 then do destroy p  -- doesn't remove image ?!? 
	        else do p # filling (col)
			p # size (cnt `div` 5,  cnt `div` 5)
			p # position (x- cnt `div` 10, y- cnt `div` 10)
			col<- nextColour col >>= return . fadeColour fade
		        delay 20
	                sparkle p (x, y) col (cnt+10) 
	  				 (if cnt>= 500 then fade-10 else fade)
	 fadeColour f (r, g, b) = (min r f, min g f, min b f)













