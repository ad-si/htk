-- #####################################################################
--
-- PROGRAM		: fileBrowserTest
-- AUTHOR		: Joel Wright
-- DATE			: 2000
-- VERSION		: ALPHA
-- DESCRIPTION		: Shows the basic layout for a filebrowser window
--
-- #####################################################################

module Main (
	main

	) where

import HTk
import Concurrency
import Mouse
import Menu
import PulldownMenu
import Frame
import Canvas
import ScrollBox
import TextItem
import Directory
import TreeListPath

main = do
	htk []
	
	fr <- newFrame []
	menubox <- newFrame [parent fr, side AtTop, fill Horizontal]
	treebox <- newFrame [parent fr, side AtLeft, fill Vertical]
	filebox <- newVFBox [parent fr, side AtRight, fill Vertical]

        mb1 <- newMenuButton [text "File", parent menubox, side AtLeft, pad Horizontal (mm 1)]
	mb2 <- newMenuButton [text "Edit", parent menubox, side AtLeft, pad Horizontal (mm 1)]
	mb3 <- newMenuButton [text "Help", parent menubox, side AtRight, pad Horizontal (mm 1)]
        m1  <- newPulldownMenu mb1 [tearOff Off]
        m2  <- newPulldownMenu mb2 [tearOff Off]
        m3  <- newPulldownMenu mb3 [tearOff Off]

	canvas1 <- newCanvas [background "white", size (cm 6, cm 50), scrollIncrement Horizontal (mm 3), scrollIncrement Vertical (mm 3)]
	canvas2 <- newCanvas [background "white", size (cm 50, cm 50), scrollIncrement Vertical (mm 3), scrollIncrement Horizontal (mm 3), fill Vertical]
	text1 <- newTextItem [value "tree", parent canvas1, canvAnchor NorthWest]
	text2 <- newTextItem [value "files", parent canvas2, canvAnchor NorthWest]

	sbox1 <- newScrollBox canvas1 [parent treebox]
	sbox2 <- newScrollBox canvas2 [parent filebox, fill Vertical]
	
	win <- window fr [text "Haskell-Tk FileBrowser", size (cm 20, cm 15)]
	
	sync (destroyed win)

