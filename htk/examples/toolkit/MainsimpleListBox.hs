{- This program displays a list box (initially empty).  The user adds new items
   by typing a line into the keyboard, and deletes items by selecting them in
   the list box. -}
module Main where

import Control.Concurrent

import Util.Computation

import Events.Events

import HTk.Components.Focus
import HTk.Toolkit.DialogWin
import HTk.Toolkit.SimpleListBox
import HTk.Toplevel.HTk

handleSelections :: SimpleListBox String -> Event [SimpleListBoxItem String]
   -> Event ()
handleSelections simpleListBox event =
   do
      selections <- event
      always (mapM
         (deleteItem simpleListBox)
         selections
         )
      handleSelections simpleListBox event

handleAdditions :: SimpleListBox String -> IO ()
handleAdditions simpleListBox =
   do
      nextItem <- getLine
      addItemAtEnd simpleListBox nextItem
      handleAdditions simpleListBox

main =
   do
      main <- initHTk [text "Simple List Box"]
      simpleListBox <- newSimpleListBox main id [size (10,15)]
      (event,terminator) <- bindSelection simpleListBox
      spawnEvent (handleSelections simpleListBox event)
      pack simpleListBox []
      handleAdditions simpleListBox
