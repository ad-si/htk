-- -----------------------------------------------------------------------
--
-- $Source$
--
-- HTk - a GUI toolkit for Haskell  -  (c) Universitaet Bremen
--
-- $Revision$ from $Date$  
-- Last modification by $Author$
--
-- -----------------------------------------------------------------------

module Main (main) where

import HTk
import DragAndDrop
import ScrollBox

showMsg :: Editor String -> String -> IO ()
showMsg ed txt =
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

main :: IO ()
main =
  do
    win <- initHTk [text "Drag and drop example"]

    main <- newVFBox win []
    pack main []

    box <- newHFBox main []
    pack box []

    notepad <- newNotepad box Scrolled (48, 48)
                          [size (cm 15, cm 10), background "white"]
    pack notepad []

    (scrollbox, output) <- newScrollBox main
                             (\ p -> newEditor p [width 40, height 6,
                                                  state Disabled] ::
                                     IO (Editor String)) []
    pack scrollbox []

    buttons <- newFrame box []
    pack buttons [Fill X]

    selectall <- newButton buttons [text "Select All", width 15]
                   :: IO (Button String)
    pack selectall [Side AtTop, PadX 10, PadY 5]
    clickedselectall <- clicked selectall
    spawnEvent (forever (clickedselectall >>
                         always (selectAll notepad)))

    deselectall <- newButton buttons [text "Deselect All", width 15]
                     :: IO (Button String)
    pack deselectall [Side AtTop, PadX 10, PadY 5]
    clickeddeselectall <- clicked deselectall
    spawnEvent (forever (clickeddeselectall >>
                         always (deselectAll notepad)))

    showselection <- newButton buttons [text "Show selection", width 15]
                       :: IO (Button String)
    pack showselection [Side AtTop, PadX 10, PadY 5]
    clickedshowselection <- clicked showselection
    spawnEvent (forever (clickedshowselection >>
                         always (showSelectedItems notepad output)))

    quit <- newButton buttons [text "Quit", width 15]
              :: IO (Button String)
    pack quit [Side AtTop, PadX 10, PadY 5]
    clickedquit <- clicked quit
    spawnEvent (forever (clickedquit >> always (destroy win)))

    item1_img <- newImage main [filename "./images/item1.gif"]
    item2_img <- newImage main [filename "./images/item2.gif"]
    item3_img <- newImage main [filename "./images/item3.gif"]
    item4_img <- newImage main [filename "./images/item2.gif"]
    item5_img <- newImage main [filename "./images/item3.gif"]
    item6_img <- newImage main [filename "./images/item1.gif"]

    item1 <- createNotepadItem "item1" notepad
                               [position (cm 2, cm 2) {-, photo item1_img,
                                name (Name { full  = "NotepadItem1",
                                             short = \n ->
                                                       take n "item1" })-}]
    item2 <- createNotepadItem "item2" notepad
                               [position (cm 5, cm 2), photo item2_img,
                                name (Name { full  = "NotepadItem2",
                                             short = \n ->
                                                      take n "item2" })]
    item3 <- createNotepadItem "item3" notepad
                               [position (cm 8, cm 2), photo item3_img,
                                name (Name { full  = "NotepadItem3",
                                             short = \n ->
                                                      take n "item3" })]
    item4 <- createNotepadItem "item4" notepad
                               [position (cm 2, cm 5), photo item4_img,
                                name (Name { full  = "NotepadItem4",
                                             short = \n ->
                                                      take n "item4" })]
    item5 <- createNotepadItem "item5" notepad
                               [position (cm 5, cm 5), photo item5_img,
                                name (Name { full  = "NotepadItem5",
                                             short = \n ->
                                                      take n "item5" })]
    item6 <- createNotepadItem "item6" notepad
                               [position (cm 8, cm 5), photo item6_img,
                                name (Name { full  = "NotepadItem6",
                                             short = \n ->
                                                      take n "item6" })]

    spawnEvent (forever (do
                           (item, b) <- receive (selectionEvent notepad)
                           always (do
                                     val <- getItemValue item
                                     showMsg output (val ++ " " ++
                                                      showB b))))

    spawnEvent (forever (do
                           (item, items) <- receive (dropEvent notepad)
                           always (do
                                     val <- getItemValue item
                                     str <- showItems items
                                     showMsg output
                                             (str ++ " dropped on " ++
                                              val ++ "\n"))))

    (controla, _) <- bind win [WishEvent [Control]
                                         (KeyPress (Just (KeySym "a")))]

    let listenKeys :: Event ()
        listenKeys = (controla >> always (selectAll notepad))
    spawnEvent (forever listenKeys)

    (htk_destr, _) <- bindSimple main Destroy
    sync(htk_destr)

  where showB :: Bool -> String
        showB True  = " selected\n"
        showB False = " deselected\n"
        showSize (x, y) = "(" ++ show x ++ ", " ++ show y ++ ")"