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
import Notepad
import ScrollBox
import Name
import ReferenceVariables
import IOExts(unsafePerformIO)

idref :: Ref Int
idref = unsafePerformIO (newRef 0)

newID :: IO Int
newID =
  do
    i <- getRef idref
    setRef idref (i + 1)
    return i

type Id = Int

data MyItem = MyItem Id Name Image

instance Eq MyItem where
  MyItem id1 _ _ == MyItem id2 _ _ = id1 == id2

instance CItem MyItem where
  getName (MyItem _ nm _) = return nm
  getIcon (MyItem _ _ ic) = return ic

main :: IO ()
main =
  do
    win <- initHTk [text "Drag and drop example"]

    main <- newVFBox win []
    pack main []

    box <- newHFBox main []
    pack box []

    notepad <- newNotepad box Scrolled (48, 48) Nothing
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

    id <- newID
    item1 <- createNotepadItem (MyItem id
                                  (newName "NotepadItem1") item1_img)
                               notepad [position (cm 2, cm 2)]

    id <- newID
    item2 <- createNotepadItem (MyItem id
                                  (newName "NotepadItem2") item2_img)
                               notepad [position (cm 5, cm 2)]

    id <- newID
    item3 <- createNotepadItem (MyItem id
                                  (newName "NotepadItem3") item3_img)
                               notepad [position (cm 8, cm 2)]

    id <- newID
    item4 <- createNotepadItem (MyItem id
                                  (newName "NotepadItem4") item4_img)
                               notepad [position (cm 2, cm 5)]

    id <- newID
    item5 <- createNotepadItem (MyItem id
                                  (newName "NotepadItem5") item5_img)
                               notepad [position (cm 5, cm 5)]

    id <- newID
    item6 <- createNotepadItem (MyItem id
                                  (newName "NotepadItem6") item6_img)
                                notepad [position (cm 8, cm 5)]

    (np_event, _) <- bindNotepadEv notepad

    spawnEvent (forever (do
                           ev_inf <- np_event
                           always (case ev_inf of
                                     Selected item ->
                                       do
                                         val <- getItemValue item
                                         nm <- getName val
                                         showMsg output ("Selected " ++
                                                         full nm)
                                     Deselected item ->
                                       do
                                         val <- getItemValue item
                                         nm <- getName val
                                         showMsg output ("Deselected " ++
                                                         full nm)
                                     Dropped (item, items) ->
                                       do
                                         val <- getItemValue item
                                         nm <- getName val
                                         str <- showItems items
                                         showMsg output
                                             (str ++ " dropped on " ++
                                              full nm ++ "\n")
                                     _ -> done)))

{-
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
-}

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

        showMsg :: Editor String -> String -> IO ()
        showMsg ed txt =
          do
            ed # state Normal
            appendText ed (txt ++ "\n")
            ed # state Disabled
            done

        showItems :: CItem a => [NotepadItem a] -> IO String
        showItems (item : items) =
          do
            val <- getItemValue item
            nm <- getName val
            rest <- showItems items
            return (full nm ++ (if length items > 0 then ", " else "")
                    ++ rest)
        showItems []             = return ""

        showSelectedItems :: CItem a => Notepad a -> Editor String ->
                                        IO ()
        showSelectedItems notepad ed =
          do
            ed # state Normal
            selecteditems <- getSelectedItems notepad
            str <- showItems selecteditems
            appendText ed ("Selected items: \n" ++ str ++ "\n\n")
            ed # state Disabled
            done
