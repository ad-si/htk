{- #########################################################################

MODULE        : Test2
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Entry Widget


   ######################################################################### -}


module Main (
        main

        ) where

import HTk
import GUICore
import SIM
import Dynamics

import Editor
import Entry
import Label
import Keyboard
import Mouse
import ListBox
import Selection

import MatrixBox

import Editor
import Mark
import TextTag
import EmbeddedTextWin

import PulldownMenu
import Scale
import Canvas
import Separator
import Message
import Button
import LabelBox
import ScrollBar

import CanvasItem
import Rectangle
import Oval
import Frame
import Line
import DialogWin

import Counter
import Bell

import TestUtil (logout)
import Debug(debug)


main = 
   do
      htk []
      
      cnt1 <- newCounter [value 0, cursor "circle"]
      controller' cnt1 (triggered cnt1 >>> done)
      window cnt1 [text "Counter1"]
      
      cnt2 <- newCounter [value 0]
      win <- window cnt2 [text "Counter2"]
      controller' win (triggered cnt2 >>> done)
      
      mkBox
      mkLabel
      mkCalculator
      mkEditor
      mkMenu
      mkMenu2
      mkWidgetSet
      
      block


mkBox = do
        b <- newFrame [flexible, bg "white"]
        button [text "Ok", pad Horizontal (cm 1),side AtLeft,expand On, parent b]
        button [text "Cancel", pad Horizontal (cm 1),side AtLeft,expand On,parent b]
        window b [text "Packer"]
        done
        
mkWidgetSet = do
        mkStateWidgets
        mkActiveWidgets
        mkContainerWidgets

mkStateWidgets = do
        b <- newVFBox [bg "white"]
        s <- newSeparator []
        newLabel [value "A simple text newLabel", pck "Label" b]
        newMessage [value "A Message containing\n several lines of text", pck "Message" b]
        newEntry [value "Please enter some text", pck "Entry" b]
        newEditor [value "Text\nEditor", bg "white", size (10,4), pck "Text" b]
        newListBox [value [1::Int,2,3,4],bg "white", size (10,4), pck "ListBox" b]
        win <- window b [text "State Widgets"]
        controller' win inaction

mkActiveWidgets = do
        b <- newVFBox [bg "white"]
        s <- newSeparator []
        newScale [value 10, orient Horizontal, rpck "Scale" b]
        newScrollBar [pck "ScrollBar" b, orient Horizontal]
        button [text "Click Here", rpck "Button" b]
        checkbutton [text "Select Here", rpck "CheckButton" b]
        radiobutton [text "Select Here", rpck "Radio Button" b]
        mb <- newMenuButton [text "Pull-Down Menu", relief Raised]
        mn <- newMenu [parent (mb::MenuButton ())]
        newMenuItem mn [text "Menu Item", reaction done]
        newSeparator [parent (mn :: Menu ())]
        newCheckItem mn [text "Check Item",reaction done]
        newRadioItem mn [text "Radio Item",reaction done]
        configure mb [rpck "MenuButton" b]
        win <- window b [text "Active Widgets"]
        controller' win inaction

mkContainerWidgets = do
        b <- newVFBox [bg "white"]
        s <- newSeparator []
        newEditor [value "Text\nEditor", bg "white", size (10,10), pck "Text" b]
        cv <- newCanvas [bg "white", size (cm 8,cm 8), pck "Canvas" b, relief Sunken]
        newRectangle [geometry (10,10,40,40), filling "blue", parent cv]
        newLine [coord [(40,40),(100,100)], filling "red", parent cv]
        newOval [geometry (50,50,100,100), filling "lightgreen", parent cv]
        win <- window b [text "Container Widgets"]
        controller' win inaction


pck t b w = do 
        newSeparator [parent b]
        l <- newLabelBox w [text t,orient Vertical]
        parent b l
        return w

rpck t b w = do
        e <- getTrigger w
        controller' w (e >>> done)
        pck t b w

mkMenu = do
        mb <- newMenuButton [text "Select"]
        mn <- newPulldownMenu (mb::MenuButton ()) []
        button [text "B1",parent mn]
        mb2 <- newMenuButton [text "Cascade",parent mn]
        mn2 <- newPulldownMenu (mb2::MenuButton ()) []
        button [text "B2",parent mn2]
        win <- window mb [text "Cascade1"]
        mne <- getTrigger mn
        controller' win mne
        
mkMenu2 = do
        mb <- newMenuButton [text "Select"]
        win <- window mb [text "Cascade1"]
        mn <- newPulldownMenu (mb::MenuButton ()) []
        button [text "B1",parent mn]
        mb2 <- newMenuButton [text "Cascade",parent mn]
        mn2 <- newPulldownMenu (mb2::MenuButton ()) []
        button [text "B2",parent mn2]
        mne <- getTrigger mn
        controller' win mne
        
        

mkLabel = do {
        tp <- newLabel [width 30, bg "white"];
        win <- window tp [text "Text Editor"];
        configure tp [value "abc\ndefg"];
        setFocus tp;
        interactor (\iact -> 
                destroyed win >>> do {logout; stop iact}        
           +>   keystroke tp (KeyPress (Just "Return")) >>> do {
                        v' <- getValue tp;                      
                        print v'
                        }); 
        done
} 


mkCalculator = do {
        mb <- newMatrixBox [bg "white",orient Vertical];
        mapM (createRow mb) elems;
        win <- window mb [text "Calculator"];
        controller' win inaction;
        done    
 } where
        elems = [ ["7","8","9","X"], ["4","5","6","/"],
                        ["1","2","3","-"], ["0",".","=","+"] ]
        createRow mb l = do
                newRow mb
                lbs <- mapM (\e -> newLabel [value e, parent mb, bg "red", relief Raised, borderwidth (cm 0.1)]) l
                done
                        

mkEditor = do 
        ed <- newEditor [wrap WordWrap, background "white",flexible,value ""]
        win <- window ed [text "Test Text Pane"]
        configure ed [height 40, width 60]
        controller' win (inaction)
        insertText ed (pos 1 0) "hello there"
        insertText ed (pos 2 0) "second line\n"
        insertText ed (pos 3 0) "third line\n"
        m <- newMark ed "mark1" (pos 2 1) 
        setMarkGravity m ToLeft
        tt <- newTextTag ed (pos 1 0) (pos 1 10) [background "red"]
        controller' tt (anyKeyPressed tt >>> bell)
        cnt <- newCounter       [value (10::Int)]
        controller' cnt (triggered cnt >>> done)
        ew <- newEmbeddedTextWin ed cnt (pos 2 1) [align InCenter]
        done

pos :: Distance -> Distance -> Position
pos x y = (x,y)
