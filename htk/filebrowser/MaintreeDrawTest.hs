-- #####################################################################
--
-- PROGRAM		: treeDrawTest
-- AUTHOR		: Joel Wright
-- DATE			: 2000
-- VERSION		: ALPHA
-- DESCRIPTION		: Demonstrates the treeDraw function
--
-- #####################################################################


module Main (
        main

        ) where

import HTk
import Concurrency(block)
import PulldownMenu
import Frame
import Line
import CanvasItem
import Mouse
import Canvas
import ScrollBar
import TextItem
import EmbeddedCanvasWin
import TreeListPath
import ScrollBox
import CanvasTag
import GUICore
import Image
import ImageItem
import Directory

import IO(stdout)

main = do
	htk[]

	can <- newCanvas [size (cm 10,cm 10), background "white", scrollIncrement Vertical (cm 0.5), scrollIncrement Horizontal (cm 0.5)]
	sbox <- newScrollBox can []
	
	wn <- window sbox [text "Tree Draw and Interaction Test"]	

	tree <- initTreeList
	drawTree2 tree can

	sync (destroyed wn)
	

-- This funtion calls the full drawTree function
drawTree2 tlp can = do
	num <- getNumberElementsIO tlp
	can2 <- newCanvas [background "white", size (cm 10 ,cm ((num*0.51)+0.51))]
	ecan <- newEmbeddedCanvasWin can2 [parent can, position (cm 5, cm 5)]
	drawTree tlp can2 (cm 0.5, cm 0.5) tlp can
	
-- This is the root of the drawTree function
-- it calls all the necessary functions to draw the tree
drawTree tlp can pos tlpstatic canstatic = do
	drawTreeNode tlp can pos tlpstatic canstatic
	drawChildrenTrees tlp can pos tlpstatic canstatic		   

-- This function draws the node
-- it includes the interactors to open and close the tree
drawTreeNode (Node a b tp tlps) can (x,y) tlpstatic canstatic = do
	im <- newImage []
	if b then (im # filename "./images/boxdash.gif")
	 else (im # filename "./images/boxcross.gif")
	l <- newLine [coord [(x,y),((x+(mm 1)),y)],outlinewidth (mm 0.25),filling "black",parent can]
	imdraw <- newImageItem [position ((x+(mm 1)),y), photo im, parent can, canvAnchor West]
	tx <- newTextItem [value a, parent can, position ((x+(mm 1)+(14)),y), canvAnchor West]
	interactor (\iact -> mouseButtonPress tx 1 >>>
		do
		   fpth <- getFilePathIO tlpstatic (reverse(tp))
		   files <- getDirectoryContents fpth
		   putTheFiles fpth files)		   
	interactor (\iact2 -> mouseButtonPress imdraw 1 >>>
		if (b == False) then
		do
		   fpth <- getFilePathIO tlpstatic (reverse(tp))
		   treetoinsert <- makeChildTrees (Node a b tp tlps) fpth
		   newTree <- changeTreeIO tlpstatic (reverse(tp)) (swapTree treetoinsert)
		   destroy can
		   drawTree2 newTree canstatic
		else
		do
		   newTree <- changeTreeIO tlpstatic (reverse(tp)) simpleClose
		   destroy can
		   drawTree2 newTree canstatic)
		   
-- This function draws the child list of trees
drawChildrenTrees (Node a b tp tlps) can (x,y) tlpstatic canstatic =
	if (b == True) then (drawChildrenTrees2 (Node a b tp tlps) can (x,y) tlpstatic canstatic)
	else (putStr "")

-- This function draws the child list of Trees
drawChildrenTrees2 (Node a b tp tlps) can (x,y) tlpstatic canstatic =
	if (tlps == []) then (putStr "")
	else ( do l <- newLine [coord [(x,y),(x,(y+(cm 0.5))),((x+(cm 0.5)),(y+(cm 0.5)))],outlinewidth (mm 0.25),filling "black",parent can]
		  drawSiblingTrees tlps can ((x+(cm 0.5)),(y+(cm 0.5))) tlpstatic canstatic
		  interactor (\i -> mouseButtonPress l 2 >>> 
		  	do putStr "line clicked\n"))
	
-- This function draws the list of trees		  
drawSiblingTrees (tlp:tlps) can (x,y) tlpstatic canstatic =
	if (tlps == []) then (do drawTree tlp can (x,y) tlpstatic canstatic)
	else (do drawTree tlp can (x,y) tlpstatic canstatic
		 num <- getNumberOpenElementsIO tlp
		 l <- newLine [coord [(x,y),(x,(y+(cm (num*0.51))))],outlinewidth (mm 0.25), filling "black", parent can]
		 drawSiblingTrees tlps can (x,(y+(cm (num*0.51)))) tlpstatic canstatic)

-- This function outputs the contents of the clicked folder to a terminal
-- window
putTheFiles fpth [file] = putStr (file++"\n")
putTheFiles fpth (file:files) = do
				b <- doesDirectoryExist file
				if b then (putTheFiles fpth files)
					else (do
						putStr (file++"\n")
						putTheFiles fpth files)

