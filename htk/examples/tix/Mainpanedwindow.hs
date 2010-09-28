
module Main (main) where

import HTk.Toplevel.HTk

main:: IO ()
main =
  do
    main <- initHTk [text "paned window example!"]
    requirePackage "Tix"

    pwh <- newPanedWindow main Horizontal []
    paneh1 <- createPane pwh [initsize 300, minsize 100, maxsize 400] []
    paneh2 <- createPane pwh [initsize 300, minsize 100, maxsize 400] []

    pwv <- newPanedWindow paneh1 Vertical []
    panev1 <- createPane pwv [initsize 300, minsize 100, maxsize 400] []
    panev2 <- createPane pwv [initsize 200, minsize 100, maxsize 300] []

    pack pwh [Fill Both, Expand On]
    pack pwv [Fill Both, Expand On]

    but1 <- newButton paneh2 [text "pane1"]
    pack but1 [Fill Both, Expand On]

    but2 <- newButton panev1 [text "pane2"]
    pack but2 [Fill Both, Expand On]

    but3 <- newButton panev2 [text "pane3"]
    pack but3 [Fill Both, Expand On]

    finishHTk
