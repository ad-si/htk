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

module Main (main) where

import HTk
import PulldownMenu
import Rectangle
import Mouse
import Canvas
import RVar

main :: IO ()
main = do
         tk <- htk []
         f <- newVFBox[]
         win <- window f [text "HTk Drawing Pad"]
         cnv <- newCanvas [size (cm 15, cm 15), parent f,
		           background "white"]
         putRects cnv
         interactor (\iact-> mouseButtonPress cnv 3 >>>=
                               \ (x, y)-> putRect cnv ("yellow", (x, y)))
         sync(destroyed win)
         destroy tk

putRects cnv = mapM_ (putRect cnv) [("red", (cm 0.2, cm 4)),
			            ("green", (cm 2.2, cm 4)),
		                    ("blue", (cm 4.2, cm 4))] 

putRect cnv (col, pos) = do
	r<- newRectangle [size (cm 1, cm 1), parent cnv, position pos,
			  outline "black", filling col]
        posref <- newRVar pos
        interactor (move r posref)

        where move :: Rectangle -> RVar Position -> InterActor -> IA ()
              move r posref iact =
                ((mouseEvent r (Button1, Motion) >>>=
                    \ ((x, y), _) -> do
                                       (x0, y0) <- getVar posref
                                       moveItem r (x - x0) (y - y0)
                                       setVar posref (x, y)))

{-
	interactor (notmoving r)

	where  notmoving :: Rectangle-> InterActor-> IA () 
	       notmoving r iact =
	  	   ((mouseEvent r (Button1, Motion)
			    >>>= \(pos@(x,y), _)->
		 		   become iact (moving r x y iact))
	           +> (mouseEvent r (Button2, Motion) 
	                    >>>= \((x, y), _)-> scaleItem r x y 0.99 0.99))

	       moving :: Rectangle -> Distance-> Distance->InterActor-> IA ()
	       moving r x0 y0 iact = 
		    ((mouseEvent r (Button1, Motion)
		     >>>= \((x, y), _)-> moveItem r (x- x0) (y- y0) >>
                                         become iact (moving r x y iact))
	          +> (mouseEvent r (ButtonRelease Nothing)
		     >>> become iact (notmoving r iact)))
-}