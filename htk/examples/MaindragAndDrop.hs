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
import Frame
import Concurrency
import Image
import DragAndDrop
import Button
import Editor
import ScrollBox

showText :: Editor String -> String -> IO ()
showText ed txt =
  do
    ed # state Normal
    appendText ed txt
    ed # state Disabled
    done

showItems :: [NotepadItem a] -> IO String
showItems (item : items) =
  do
    itemname <- getName item
    rest <- showItems items
    return (full itemname ++ (if length items > 0 then ", " else "")
            ++ rest)
showItems []             = return ""

showSelectedItems :: Notepad a -> Editor String -> IO ()
showSelectedItems notepad ed =
  do
    ed # state Normal
    selecteditems <- getSelectedItems notepad
    str <- showItems selecteditems
    appendText ed ("Selected items: \n" ++ str ++ "\n\n")
    ed # state Disabled
    done

main =
  do
    win <- htk []
    main <- newVFBox []
    box <- newHFBox [parent main]
    win <- window main [text "Drag and drop example"]
    notepad <- newNotepad True [parent box, size (cm 15, cm 10),
                                background "white"] :: IO (Notepad String)
    output <- newEditor [width 40, height 6,
                         state Disabled] :: IO (Editor String)
    buttons <- newFrame [parent box, fill Horizontal]
    selectall <- newButton [side AtTop, pad Horizontal 10, pad Vertical 5,
                            parent buttons, text "Select All", width 15,
                            command (\ () -> selectAll notepad)]
    deselectall <- newButton [side AtTop, pad Horizontal 10, pad Vertical 5,
                              parent buttons, text "Deselect All", width 15,
                              command (\ () -> deselectAll notepad)]
    showselection <- newButton [side AtTop, pad Horizontal 10, pad Vertical 5,
                                parent buttons, text "Show selection",
                                width 15, command (\ () -> showSelectedItems
                                                             notepad
                                                             output)]
    quit <- newButton [side AtBottom,  pad Horizontal 10, pad Vertical 5,
                       parent buttons, text "Quit", width 15,
                       command (\ ()-> destroy win)]
    interactor (\i -> triggered quit +> triggered selectall +>
                      triggered deselectall +> triggered showselection)
    scrollbox <- newScrollBox output [parent main]
    item1_img <- newImage [filename "./images/item1.gif"]
    size <- getSize(item1_img)
    putStr("image1 size:" ++ showSize size ++ "\n")
    item2_img <- newImage [filename "./images/item2.gif"]
    item3_img <- newImage [filename "./images/item3.gif"]
    item4_img <- newImage [filename "./images/item2.gif"]
    item5_img <- newImage [filename "./images/item3.gif"]
    item6_img <- newImage [filename "./images/item1.gif"]
    item1 <- newNotepadItem "item1" notepad
                            [position (cm 2, cm 2), photo item1_img,
                             name (ItemName { full  = "NotepadItem1",
                                              short = \n -> take n "item1" })]
    item2 <- newNotepadItem "item2" notepad
                            [position (cm 5, cm 2), photo item2_img,
                             name (ItemName { full  = "NotepadItem2",
                                              short = \n -> take n "item2" })]
    item3 <- newNotepadItem "item3" notepad
                            [position (cm 8, cm 2), photo item3_img,
                             name (ItemName { full  = "NotepadItem3",
                                              short = \n -> take n "item3" })]
    item4 <- newNotepadItem "item4" notepad
                            [position (cm 2, cm 5), photo item4_img,
                             name (ItemName { full  = "NotepadItem4",
                                              short = \n -> take n "item4" })]
    item5 <- newNotepadItem "item5" notepad
                            [position (cm 5, cm 5), photo item5_img,
                             name (ItemName { full  = "NotepadItem5",
                                              short = \n -> take n "item5" })]
    item6 <- newNotepadItem "item6" notepad
                            [position (cm 8, cm 5), photo item6_img,
                             name (ItemName { full  = "NotepadItem6",
                                              short = \n -> take n "item6" })]
    interactor
      (\i -> (selectionEvent item1 >>>= \b -> showText output ("item 1 " ++
                                                               showB b)) +>
             (selectionEvent item2 >>>= \b -> showText output ("item 2 " ++
                                                               showB b)) +>
             (selectionEvent item3 >>>= \b -> showText output ("item 3 " ++
                                                               showB b)) +>
             (selectionEvent item4 >>>= \b -> showText output ("item 4 " ++
                                                               showB b)) +>
             (selectionEvent item5 >>>= \b -> showText output ("item 5 " ++
                                                               showB b)) +>
             (selectionEvent item6 >>>= \b -> showText output ("item 6 " ++
                                                               showB b)))
    interactor
      (\i -> (dropEvent item1 >>>= \items ->
                                     do
                                       str <- showItems items
                                       showText output
                                                (str ++
                                                 " dropped on item 1\n\n")) +>
             (dropEvent item2 >>>= \items ->
                                     do
                                       str <- showItems items
                                       showText output
                                                (str ++
                                                 " dropped on item 2\n\n")) +>
             (dropEvent item3 >>>= \items ->
                                     do
                                       str <- showItems items
                                       showText output
                                                (str ++
                                                 " dropped on item 3\n\n")) +>
             (dropEvent item4 >>>= \items ->
                                     do
                                       str <- showItems items
                                       showText output
                                                (str ++
                                                 " dropped on item 4\n\n")) +>
             (dropEvent item5 >>>= \items ->
                                     do
                                       str <- showItems items
                                       showText output
                                                (str ++
                                                 " dropped on item 5\n\n")) +>
             (dropEvent item6 >>>= \items ->
                                     do
                                       str <- showItems items
                                       showText output
                                                (str ++
                                                 " dropped on item 6\n\n")))
    sync (destroyed win)
    shutdown
  where showB :: Bool -> String
        showB True  = " selected\n"
        showB False = " deselected\n"
        showSize (x, y) = "(" ++ show x ++ ", " ++ show y ++ ")"