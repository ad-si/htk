{- ------------------------------------------------------------------------
 -
 - HTk Examples: Drag and drop package
 -
 - Author: ludi
 - $Revision$ from $Date$  
 -
 - ------------------------------------------------------------------------ -}

module Main (main) where

import HTk
import Concurrency
import Image
import DragAndDrop
import IO(stdout)

main =
  do

    win <- htk []
    setLogFile (Just stdout)
    box <- newVBox []
    win <- window box [text "Drag and drop example"]
    notepad <- newNotepad RightBottomScroller [parent box] [size (cm 30, cm 15), background "white"] :: IO (Notepad String)

    item1_img <- newImage [filename "./images/item1.gif"]
    item2_img <- newImage [filename "./images/item2.gif"]
    item3_img <- newImage [filename "./images/item3.gif"]
    item4_img <- newImage [filename "./images/item2.gif"]
    item5_img <- newImage [filename "./images/item3.gif"]
    item6_img <- newImage [filename "./images/item1.gif"]

    newNotepadItem "item1" notepad [parent notepad, position (cm 2, cm 2),
                                    name (ItemName { full  = "NotepadItem1",
                                                     short = \n -> take n "item1" }),
                                    photo item1_img]
    newNotepadItem "item2" notepad [parent notepad, position (cm 5, cm 2),
                                    photo item2_img,
                                    name (ItemName { full  = "NotepadItem2",
                                                     short = \n -> take n "item2" })]
    newNotepadItem "item3" notepad [parent notepad, position (cm 8, cm 2),
                                    photo item3_img,
                                    name (ItemName { full  = "NotepadItem3",
                                                     short = \n -> take n "item3" })]
    newNotepadItem "item4" notepad [parent notepad, position (cm 2, cm 5),
                                    photo item4_img,
                                    name (ItemName { full  = "NotepadItem4",
                                                     short = \n -> take n "item4" })]
    newNotepadItem "item5" notepad [parent notepad, position (cm 5, cm 5),
                                    photo item5_img,
                                    name (ItemName { full  = "NotepadItem5",
                                                     short = \n -> take n "item5" })]
    newNotepadItem "item6" notepad [position (cm 8, cm 5),
                                    photo item6_img,
                                    name (ItemName { full  = "NotepadItem6",
                                                     short = \n -> take n "item6" }),
                                    parent notepad]


    sync (destroyed win)
