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
import Name
import ReferenceVariables

main :: IO ()
main =
  do
    cnt <- newRef 1

    win1 <- initHTk [text "Notepad 1"]
    win2 <- createToplevel [text "Notepad 2"]

    notepad1 <- newNotepad win1 Scrolled (48, 48) Nothing
                          [size (cm 15, cm 10), background "white"]
    pack notepad1 []

    add1 <- newButton win1 [text "add item"] :: IO (Button String)
    pack add1 [Fill X]
    clicked_add1 <- clicked add1

    import1 <- newButton win1 [text "import other state"]
                 :: IO (Button String)
    pack import1 [Fill X]
    clicked_import1 <- clicked import1

    notepad2 <- newNotepad win2 Scrolled (48, 48) Nothing
                          [size (cm 15, cm 10), background "white"]
    pack notepad2 []

    add2 <- newButton win2 [text "add item"] :: IO (Button String)
    pack add2 [Fill X]
    clicked_add2 <- clicked add2

    import2 <- newButton win2 [text "import other state"]
                 :: IO (Button String)
    pack import2 [Fill X]
    clicked_import2 <- clicked import2

    let num :: IO Int
        num = do
                i <- getRef cnt
                setRef cnt (i + 1)
                return i

    img <- Main.img
    spawnEvent (forever ((clicked_add1 >>>
                            do
                              i <- num
                              createNotepadItem () notepad1
                                [position (cm 2, cm 2), photo img,
                                 name (newName ("item" ++ show i))]
                              done) +>
                         (clicked_add2 >>>
                            do
                              i <- num
                              createNotepadItem () notepad2
                                [position (cm 2, cm 2), photo img,
                                 name (newName ("item" ++ show i))]
                              done) +>
                         (clicked_import1 >>>
                            do
                              st <- exportNotepadState notepad2
                              importNotepadState notepad1 st) +>
                         (clicked_import2 >>>
                            do
                              st <- exportNotepadState notepad1
                              importNotepadState notepad2 st)))

    (win1_destr, _) <- bindSimple win1 Destroy
    (win2_destr, _) <- bindSimple win2 Destroy
    spawnEvent (forever (win2_destr >>> destroy win1))
    sync (win1_destr)

img :: IO Image
img = newImage NONE [filename "images/notepaditem.gif"]


{-
[imgData GIF "R0lGODlhMAAwAOMAAEuvTTqIPDJ0Mxk6GQAAABAnESlhKiFOIggTCEKbRP//////////////////
/////yH+FUNyZWF0ZWQgd2l0aCBUaGUgR0lNUAAh+QQBCgAPACwAAAAAMAAwAAAE2fDJSau9OOvN
u/9gKI5kaZ5oqq5s675w/AJ0bd/Aiu/8PfbAIK1zCwiESFwGNyDQAoNAEmnhNWkCwnFKlfSugQKh
EH0OBraBAWAoA9Q9r9UZHpcPhEEBkaCNEU0FBgViBzwPQFcAWVsABIaOa44EUlkIfk5cNIqMWHZN
aI6hnZOam5mkWWRnknmeWwSZO4hzrzQJjziui1qYh7Q7igF5An1NBgICraO9pbNyTLJNzQaxBMu2
zj4Vpt01S96aROFAP+TfKlwy6+zt7u/w8fLz9PX29/j5+vv8HxEAOw=="]
-}