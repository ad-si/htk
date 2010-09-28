
module Main (main) where

import HTk.Toplevel.HTk
import Reactor.ReferenceVariables

main :: IO ()
main =
  do
    main <- initHTk [text "text tag example"]
    ed <- newEditor main [size(50, 15), cursor xterm]
    pack ed [Fill Both, Expand On]

    appendText ed "This is line1\nThis is line2\nThis is line3\nThis is line4\nThis is line5\n\nClick here to quit!\n\n\n"

    ed # state Disabled

    lines <- mapM (\ (i1, i2) -> createTextTag ed i1 i2 [])
                  [(IndexPos (1,0), IndexPos (1,13)),
                   (IndexPos (2,0), IndexPos (2,13)),
                   (IndexPos (3,0), IndexPos (3,13)),
                   (IndexPos (4,0), IndexPos (4,13)),
                   (IndexPos (5,0), IndexPos (5,13))]

    quit <- createTextTag ed (IndexPos (7,0)) (IndexPos (7,19))
                          [fg "blue"]

    let line_event :: Int -> IO (Event ())
        line_event n =
          do (entered_line, _) <- bindSimple (lines !! n) Enter
             (left_line, _) <- bindSimple (lines !! n) Leave
             return
               (   (entered_line >>>
                      (ed # tooltip ("this is line " ++ show (n + 1) ++
                                     " of the editor widget") >>
                       ed # cursor arrow >> done))
                +> (left_line >>> (destroyTooltip ed >>
                                   ed # cursor xterm >> done)))

    le1 <- line_event 0
    le2 <- line_event 1
    le3 <- line_event 2
    le4 <- line_event 3
    le5 <- line_event 4

    (quit_event, _) <- bindSimple quit (ButtonPress (Just 1))
    (entered_quit, _) <- bindSimple quit Enter
    (left_quit, _) <- bindSimple quit Leave

    spawnEvent (forever (   le1 +> le2 +> le3 +> le4 +> le5
                         +> (quit_event >>> destroy main)
                         +> (entered_quit >>> (quit # fg "red" >>
                                               ed # cursor arrow >> done))
                         +> (left_quit >>> (quit # fg "blue" >>
                                            ed # cursor xterm >> done))))

    but <- newButton ed [text "This is an embedded button widget"]
    createEmbeddedTextWin ed (IndexPos (9,0)) but []

    finishHTk
