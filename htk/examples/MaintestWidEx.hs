{- ------------------------------------------------------------------------
 -
 - HTk Examples: Textwidgets.
 -
 - Author: cxl 
 - $Revision$ from $Date$  
 -
 - ------------------------------------------------------------------------ -}

module Main (main) where

import HTk
import Button
import Mouse
import Editor
import TextTag
import EmbeddedTextWin

main :: IO ()
main = do
        tk <- htk []
	f <- newVBox[]
	win <- window f [text "HTk Text Widget"]	
	t<- newEditor [wrap WordWrap, bg "white", fg "black", parent f,
		       width 40, height 10, font (Lucida, 16::Int)]
	b<- newButton [text "Click me!",
		       command (\()-> destroy win)]
	t # value ("In a text widget, you can do\n" ++
	           "different coulours,\n" ++
		   "different fonts (abgSP) and sizes, \n" ++
	           "embedded widgets, and \n" ++
		   "active text.")
	newTextTag t (at 2 0)  (at 2 4) [foreground "green"]
	newTextTag t (at 2 4)  (at 2 9) [foreground "yellow"]
	newTextTag t (at 2 10) (at 2 14) [foreground "red"]
	newTextTag t (at 2 14) (at 2 18) [foreground "blue"]
	newTextTag t (at 3 0)  (at 3 15) [font (Courier, 16::Int)]
	newTextTag t (at 3 17) (at 3 999) [font "-*-symbol-*-*-*-*-16-*-*-*-*-*-*-*"]
	newTextTag t (at 3 28) (at 3 33) [font (Lucida, 24::Int)]
	newEmbeddedTextWin t b (at 4 17) []
	ht<- newTextTag t (at 5 0) (at 5 11) [underlined On]
	t # disable
	interactor (\i-> triggered b)
	interactor (\i-> mouseButtonPress ht 1 >>> 
			    (do {t # enable;
			     	 insertText t (at 1 0) "Text clicked!";
				 t # disable; done})
			 +> (mouseEnter ht >>> (ht # fg "red" >> done))
		 	 +> (mouseLeave ht >>> (ht # fg "black" >> done)))

	sync (destroyed win)
        destroy tk

  where
	at :: Distance -> Distance -> Position
	at x y = (x,y)
