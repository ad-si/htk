-- #####################################################################
--
-- PROGRAM		: fileBrowserTest
-- AUTHOR		: Joel Wright
-- DATE			: 2000
-- VERSION		: ALPHA
-- DESCRIPTION		: Shows the basic layout and drawind functions 
--			  for a filebrowser window
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
import Line
import Image
import ImageItem
import TreeListPath
import CanvasItem
import EmbeddedCanvasWin
import FileActions
import Bell

main = do
	htk []
	
	fr <- newFrame []
	menubox <- newFrame [parent fr, side AtTop, fill Horizontal]
	treebox <- newFrame [parent fr, side AtLeft, fill Vertical]
	filebox <- newVFBox [parent fr, side AtRight, fill Vertical]

        mb1 <- newMenuButton [text "File", parent menubox, side AtLeft, pad Horizontal (mm 1),  pad Vertical (mm 0.5)]
	mb2 <- newMenuButton [text "Edit", parent menubox, side AtLeft, pad Horizontal (mm 1),  pad Vertical (mm 0.5)]
	mb3 <- newMenuButton [text "Help", parent menubox, side AtRight, pad Horizontal (mm 1),  pad Vertical (mm 0.5)]
        m1 <- newPulldownMenu mb1 [tearOff Off]
        m2 <- newPulldownMenu mb2 [tearOff Off]
        m3 <- newPulldownMenu mb3 [tearOff Off]

	canvas1 <- newCanvas [background "white", size (cm 6, cm 50), scrollIncrement Horizontal (mm 3), scrollIncrement Vertical (mm 3)]
	canvas2 <- newCanvas [background "white", size (cm 50, cm 50), scrollIncrement Vertical (mm 3), scrollIncrement Horizontal (mm 3), fill Vertical]

	sbox1 <- newScrollBox canvas1 [parent treebox]
	sbox2 <- newScrollBox canvas2 [parent filebox, fill Vertical]
	
	win <- window fr [text "Haskell-Tk FileBrowser", size (cm 23, cm 15)]
	
	infoButton <- newMenuItem m3 [text "Info"]
	closeButton <- newMenuItem m1 [text "Exit", command (closer win)]

	tree <- initTreeList
	drawTreeMain tree canvas1 canvas2

	interactor (const (triggered closeButton))

	sync (destroyed win)
	
	 where

	  closer win () = destroy win

-- This function calls the full drawTree function
	  drawTreeMain tlp can iconcanvas = do
		num <- getNumberElementsIO tlp
		treeCanvas <- newCanvas [background "white", size (cm 15 ,cm ((num*0.51)+0.51))]
		ecan <- newEmbeddedCanvasWin treeCanvas [parent can, position (cm 7.5, cm 5.3)]
		iconCanvas <- newCanvas [background "white", size (cm 20 ,cm 80)]
		ecan2 <- newEmbeddedCanvasWin iconCanvas [parent iconcanvas, position (cm 10,cm 40)]
		drawTree tlp treeCanvas (cm 0.5, cm 0.5) tlp can iconCanvas

	   where
-- This is the root of the drawTree function
-- it calls all the necessary functions to draw the tree
	    drawTree tlp can pos tlpstatic canstatic iconCanvas = do
		drawTreeNode tlp can pos tlpstatic canstatic iconCanvas
		drawChildrenTrees tlp can pos tlpstatic canstatic iconCanvas
		
-- This function draws the node
-- it includes the interactors to open and close the tree
	    drawTreeNode (Node a b tp tlps) can (x,y) tlpstatic canstatic iconCanvas = do
		im <- newImage []
		if b then (im # filename "./images/boxdash.gif")
		 else (im # filename "./images/boxcross.gif")
		l <- newLine [coord [(x,y),((x+(mm 1)),y)],outlinewidth (mm 0.25),filling "black",parent can]
		imdraw <- newImageItem [position ((x+(mm 1)),y), photo im, parent can, canvAnchor West]
		tx <- newTextItem [value a, parent can, position ((x+(mm 1)+(14)),y), canvAnchor West]
		interactor (\iact -> mouseButtonPress tx 1 >>>
			do
			   fpth <- getFilePathIO tlpstatic (reverse(tp))
			   setCurrentDirectory fpth
			   dcontents <- getDirectoryContents fpth
			   files <- getFiles dcontents
			   destroy iconCanvas
			   iconCanvas <- newCanvas [background "white", size (cm 20 ,cm 80)]
		           ecan2 <- newEmbeddedCanvasWin iconCanvas [parent iconcanvas, position (cm 10,cm 40)]
			   drawIcons files iconCanvas ((cm 1.3), (cm 1)))	   
		interactor (\iact2 -> mouseButtonPress imdraw 1 >>>
			if (b == False) then
			do
			   fpth <- getFilePathIO tlpstatic (reverse(tp))
			   treetoinsert <- makeChildTrees (Node a b tp tlps) fpth
			   newTree <- changeTreeIO tlpstatic (reverse(tp)) (swapTree treetoinsert)
			   destroy can
			   drawTreeMain newTree canstatic iconcanvas
			   setCurrentDirectory fpth
			   dcontents <- getDirectoryContents fpth
			   files <- getFiles dcontents
			   destroy iconCanvas
			   iconCanvas <- newCanvas [background "white", size (cm 20 ,cm 80)]
		           ecan2 <- newEmbeddedCanvasWin iconCanvas [parent iconcanvas, position (cm 10,cm 40)]
			   drawIcons files iconCanvas ((cm 1.3), (cm 1))
			else
			do
			   newTree <- changeTreeIO tlpstatic (reverse(tp)) simpleClose
			   destroy can
			   drawTreeMain newTree canstatic iconcanvas)
		   
-- This function draws the child list of trees
	    drawChildrenTrees (Node a b tp tlps) can (x,y) tlpstatic canstatic iconCanvas =
		if (b == True) then (drawChildrenTrees2 (Node a b tp tlps) can (x,y) tlpstatic canstatic iconCanvas)
		else (putStr "")

-- This function draws the child list of Trees
	    drawChildrenTrees2 (Node a b tp tlps) can (x,y) tlpstatic canstatic iconCanvas =
		if (tlps == []) then (putStr "")
		else ( do l <- newLine [coord [((x+(cm 0.5)),(y+(mm 2))),((x+(cm 0.5)),(y+(cm 0.5)))],outlinewidth (mm 0.25),filling "black",parent can]
			  drawSiblingTrees tlps can ((x+(cm 0.5)),(y+(cm 0.5))) tlpstatic canstatic iconCanvas)
	
-- This function draws the list of trees		  
	    drawSiblingTrees (tlp:tlps) can (x,y) tlpstatic canstatic iconCanvas =
		if (tlps == []) then (do drawTree tlp can (x,y) tlpstatic canstatic iconCanvas)
		else (do drawTree tlp can (x,y) tlpstatic canstatic iconCanvas
			 num <- getNumberOpenElementsIO tlp
			 l <- newLine [coord [(x,y),(x,(y+(cm (num*0.51))))],outlinewidth (mm 0.25), filling "black", parent can]
			 drawSiblingTrees tlps can (x,(y+(cm (num*0.51)))) tlpstatic canstatic iconCanvas)

-- This function draws a list of files as icons and text
	    drawIcons [] can pos = putStr ""
	    drawIcons (f:files) can (xpos, ypos) =
	    	if (xpos < (cm 13)) then
			(do
				nom <- makeNameForIcon f
				drawIcon f can (xpos, ypos) nom
				drawIcons files can ((xpos + (cm 2.8)), ypos))
			else
			(do
				nom <- makeNameForIcon f
				drawIcon f can (xpos, ypos) nom
				drawIcons files can ((cm 1.3), (ypos + (cm 2.5))))

-- This function draws individual icons
	    drawIcon file can (xpos, ypos) name = do
		icon <- selectIcon file
		img <- newImage [filename (icon)]
		imgdraw <- newImageItem [position (xpos, ypos), photo img, parent can, canvAnchor Center]
		tx <- newTextItem [value name, position (xpos, (ypos + (cm 0.9))), canvAnchor Center, parent can]
		putStr ""

-- This function outputs the contents of the clicked folder to a terminal
-- window
	    putTheFiles [] = putStr ""
	    putTheFiles (file:files) = do
		putStr (file++"\n")
		putTheFiles files


