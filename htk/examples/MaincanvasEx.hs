{- ------------------------------------------------------------------------
 -
 - HTk Examples: Canvas #1
 -
 - This examples demonstrates canvasses, and also how to use the `become`
 - operation. 
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


import IO(stdout)

main = do
        win<- htk []
        setLogFile (Just stdout)

	f <- newVFBox[]

	win <- window f [text "HTk Drawing Pad"]

	cnv <- newCanvas [size (cm 15, cm 15), parent f,
		          background "white"]

	putRects cnv

	interactor (\iact-> mouseButtonPress cnv 2
			    >>>= \(x, y)-> putRect cnv ("yellow", (x, y)))
        block

putRects cnv = mapM_ (putRect cnv) [("red", (cm 0.2, cm 4)),
			            ("green", (cm 2.2, cm 4)),
		                    ("blue", (cm 4.2, cm 4))] 

putRect cnv (col, pos) = do
	r<- newRectangle [size (cm 1, cm 1), parent cnv, position pos,
			  outline "black", filling col]
	interactor (notmoving r)
	where  notmoving :: Rectangle-> InterActor-> IA () 
	       notmoving r iact =
	  	   mouseEvent r (Button1, Motion)
			    >>>= \(pos@(x,y), button)->
				   if button == 1 then
					become iact (moving r x y iact)
				   else if button == 2 then
					scaleItem r x y 0.99 0.99
				   else
					done
	       moving :: Rectangle -> Distance-> Distance->InterActor-> IA()
	       moving r x0 y0 iact = 
		    ((mouseEvent r (Button1, Motion) 
		     >>>= \((x, y), _)-> moveItem r (x- x0) (y- y0) >>
					 become iact (moving r x y iact))
	          +> (mouseEvent r (ButtonRelease Nothing)
		     >>> become iact (notmoving r iact)))


