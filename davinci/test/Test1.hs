{- #########################################################################

MODULE        : TestManual
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Entry Widget

                Scrollbox - colours, slider config options

                Space - fill directives not overwritten by box


   ######################################################################### -}


module Main (
        main

        ) where

import HTk
import DaVinci

import WidgetTreeView

import Editor
import PulldownMenu
import PromptWin
import ScrollBox
import LogWin
import IO
import Debug(debug)
import Concurrent

main = do
   gui <- htk []
   dav <- davinci []
-- (win,b) <- newTextEditor
   win <- newLogWin [text "Log Window"]
   g <- newWidgetTreeView win
   interactor <- newInterActor
       (\iact ->
             destroyed g >>> print "hello"
          +> lastGraphClosed dav >>> 
                do
                   print "stopping"
                   destroy gui
                   destroy dav
                   stop iact
                  
          +> destroyed dav >>> 
                do
                   print "shutdown"
                   destroy gui 
                   stop iact                                               
          )
   sync(destroyed interactor)

{-
-- ---------------------------------------------------------------------------
-- Building the Text Editor
-- ---------------------------------------------------------------------------

newTextEditor = do
        b <- newVBox [flexible]
        mb <- newMenuButton [text "File",parent b,anchor West]  
        mn <- newEditorMenu
        configure mn [parent (mb::MenuButton EditorCmd)]
        ed <- newEditor [size (60,30), bg "white",flexible,value ""]
        sb <- newScrollBox ed [flexible,parent b]
        win <- window b [text "Text Editor"]
        controller' win (receive mn >>>= \f -> f ed)
        return (win,b)  

-- ---------------------------------------------------------------------------
-- Building the Menu
-- ---------------------------------------------------------------------------

type EditorCmd = Editor String -> IO ()

newEditorMenu :: IO (Menu EditorCmd)
newEditorMenu = do
        mn <- newMenu []
        newButton [text "Open...", cmd openFile,  parent mn]
        newButton [text "Save...", cmd saveFile,  parent mn]
        newButton [text "Empty", cmd emptyEditor, parent mn]
        return mn
 where  cmd f = command (\() -> return f)

-- ---------------------------------------------------------------------------
-- Semantic Actions
-- ---------------------------------------------------------------------------

openFile :: Editor String -> IO ()
openFile ed    = withFile (readTextFromFile ed)

saveFile :: Editor String -> IO ()
saveFile ed    = withFile (writeTextToFile ed)

emptyEditor :: Editor String -> IO ()
emptyEditor ed = do {configure ed [value ""]; done}

withFile       = withPrompt "Enter File Name"

withPrompt :: GUIValue a => String -> (a -> IO ()) -> IO ()
withPrompt msg f =  do
        ans <- newPromptWin msg cdefault [modal True]
        incase ans f


-}
