{- ------------------------------------------------------------------------
 -
 - HTk Examples: Canvas #2
 -
 - This show the other canvas items, and how to use tags.
 -
 - Author: cxl 
 - $Revision$ from $Date$  
 -
 - ------------------------------------------------------------------------ -}

module Main (main) where

import HTk
import Line
import Arc
import Image
import ImageItem
import TextItem
import EmbeddedCanvasWin
import Mouse
import Canvas
import Button
import CanvasTag

main :: IO ()
main = do
        tk <- htk []
	c <- newCanvas [size (cm 20, cm 15), background "white"]
	win <- window c [text "HTk CanvasExample"]

	santa <- newImage  [filename "./images/santa.gif"]
	frosty <- newImage [filename "./images/snowman.gif"]
	jingle<- newImage  [filename "./images/bells.gif"]

	newImageItem [position (cm 3, cm 5), photo santa, parent c]
	newImageItem [position (cm 7, cm 5), photo frosty, parent c]
	newImageItem [position (cm 10, cm 5), photo jingle, parent c]

	newTextItem  [position (cm 4, cm 2), value "Merry Xmas!",
			font (Helvetica, Bold, 24::Int), parent c]
	newLine [coord [(cm 2, cm 8), (cm 3, cm 9), 
			(cm 3, cm 8), (cm 2, cm 9), (cm 4, cm 8.5)],
		 capstyle CapRound, joinstyle JoinMiter, outlinewidth (mm 1),
		 arrowstyle LastEnd, filling "red", parent c]
	newArc  [position (cm 5, cm 8), size (cm 1.5, cm 1.5), extent 110,
		 filling "green", outlinewidth (mm 1), 
		 outline "black", parent c]

	b<- newButton [text "Click me!", relief Raised]
	eb <- newEmbeddedCanvasWin b [position (cm 2, cm 12), parent c]
	b # (command (\()-> destroy eb))

	interactor (\i-> triggered b)
	interactor (\i-> notmoving c i)

	sync (destroyed win)
        destroy tk

   where  notmoving :: Canvas-> InterActor-> IA ()
	  notmoving c iact = 
		mouseButtonPress c 1 >>>=
		   \(x, y)-> do {ct<- newCanvasTag [parent c];
                                 addCanvasTag (closest x y) ct;
				 become iact (moving c ct x y iact)}
	  moving :: Canvas-> CanvasTag-> Distance-> Distance->InterActor-> IA ()
	  moving c ct x0 y0 iact =
		 (mouseEvent c (Button1, Motion) >>>=
		   \((x, y), _)-> do {moveItem ct (x- x0) (y- y0) >>
				      become iact (moving c ct x y iact)})
		+> (mouseEvent c (ButtonRelease Nothing)
		    >>> do { become iact (notmoving c iact)})
