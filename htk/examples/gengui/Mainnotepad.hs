
module Main (main) where

import HTk.Toplevel.HTk
import HTk.Toolkit.Notepad
import HTk.Toolkit.ScrollBox
import HTk.Toolkit.Name
import Reactor.ReferenceVariables

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
    idref <- newRef 0

    let newID :: IO Int
        newID = do
                  i <- getRef idref
                  setRef idref (i + 1)
                  return i

    win <- initHTk [text "notepad example", size (500, 400)]
    logwin <- createToplevel [text "log", size (500, 200)]

    notepad <- newNotepad win Scrolled (48, 48) Nothing
                 [background "white"]
    pack notepad [Fill Both, Expand On]

    (scrollbox, output) <- newScrollBox logwin
                             (\ p -> newEditor p [state Disabled]) []
    pack scrollbox [Fill Both, Expand On]

    item1_img <- newImage [filename "./images/item1.gif"]
    item2_img <- newImage [filename "./images/item2.gif"]
    item3_img <- newImage [filename "./images/item3.gif"]
    item4_img <- newImage [filename "./images/item2.gif"]
    item5_img <- newImage [filename "./images/item3.gif"]
    item6_img <- newImage [filename "./images/item1.gif"]

    id <- newID
    item1 <- createNotepadItem (MyItem id
                                  (createName "NotepadItem1") item1_img)
                               notepad True [position (cm 2, cm 2)]

    id <- newID
    item2 <- createNotepadItem (MyItem id
                                  (createName "NotepadItem2") item2_img)
                               notepad True [position (cm 5, cm 2)]

    id <- newID
    item3 <- createNotepadItem (MyItem id
                                  (createName "NotepadItem3") item3_img)
                               notepad True [position (cm 8, cm 2)]

    id <- newID
    item4 <- createNotepadItem (MyItem id
                                  (createName "NotepadItem4") item4_img)
                               notepad True [position (cm 2, cm 5)]

    id <- newID
    item5 <- createNotepadItem (MyItem id
                                  (createName "NotepadItem5") item5_img)
                               notepad True [position (cm 5, cm 5)]

    id <- newID
    item6 <- createNotepadItem
               (MyItem id
                  (createName "NotepadItem with a long name") item6_img)
                  notepad True [position (cm 8, cm 5)]

    (np_event, _) <- bindNotepadEv notepad

    (controla, _) <- bind win [WishEvent [Control]
                                         (KeyPress (Just (KeySym "a")))]

    spawnEvent (forever ((do
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
                                     _ -> done)) +>
                         (controla >> always (selectAll notepad))))
    finishHTk

  where showMsg :: Editor -> String -> IO ()
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
