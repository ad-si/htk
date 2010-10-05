
module Main (main) where

import Util.Computation

import Events.Synchronized

import HTk.Toplevel.HTk
import HTk.Toolkit.Notepad
import HTk.Toolkit.Name
import Reactor.ReferenceVariables
import System.IO.Unsafe

idref :: Ref Int
idref = unsafePerformIO (newRef 0)
{-# NOINLINE idref #-}

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
    cnt <- newRef 1

    win1 <- initHTk [text "Notepad 1"]
    win2 <- createToplevel [text "Notepad 2"]

    notepad1 <- newNotepad win1 Scrolled (48, 48) Nothing
                          [size (cm 15, cm 10), background "white"]
    pack notepad1 []

    add1 <- newButton win1 [text "add item"]
    pack add1 [Fill X]
    clicked_add1 <- clicked add1

    import1 <- newButton win1 [text "import other state"]
    pack import1 [Fill X]
    clicked_import1 <- clicked import1

    notepad2 <- newNotepad win2 Scrolled (48, 48) Nothing
                          [size (cm 15, cm 10), background "white"]
    pack notepad2 []

    add2 <- newButton win2 [text "add item"]
    pack add2 [Fill X]
    clicked_add2 <- clicked add2

    import2 <- newButton win2 [text "import other state"]
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
                              id <- newID
                              createNotepadItem
                                (MyItem id (createName ("item" ++ show i))
                                        img)
                                notepad1 True [position (cm 2, cm 2)]
                              done) +>
                         (clicked_add2 >>>
                            do
                              i <- num
                              id <- newID
                              createNotepadItem
                                (MyItem id (createName ("item" ++ show i))
                                        img)
                                notepad2 True [position (cm 2, cm 2)]
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
img = newImage [filename "images/notepaditem.gif"]
