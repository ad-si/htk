{- This program displays a list box (initially empty).  The user adds new items
   by typing a line into the keyboard, and deletes items by selecting them in
   the list box. -}
module Main where

import Events

import SimpleListBox

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
      simpleListBox <- newSimpleListBox "Simple List Box" id (10,15)
      (event,terminator) <- bindSelection simpleListBox
      spawnEvent (handleSelections simpleListBox event)
      handleAdditions simpleListBox