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

main:: IO ()
main =
  do
    main <- initHTk [text "paned window example!"]

    pwh <- newPanedWindow main Horizontal []
    paneh1 <- createPane pwh [initsize 300, minsize 100, maxsize 400] []
    paneh2 <- createPane pwh [initsize 300, minsize 100, maxsize 400] []

    pwv <- newPanedWindow paneh1 Vertical []
    panev1 <- createPane pwv [initsize 300, minsize 100, maxsize 400] []
    panev2 <- createPane pwv [initsize 200, minsize 100, maxsize 300] []

    pack pwh [Fill Both, Expand On]
    pack pwv [Fill Both, Expand On]

    but1 <- newButton paneh2 [text "pane1"] :: IO (Button String)
    pack but1 [Fill Both, Expand On]

    but2 <- newButton panev1 [text "pane2"] :: IO (Button String)
    pack but2 [Fill Both, Expand On]

    but3 <- newButton panev2 [text "pane3"] :: IO (Button String)
    pack but3 [Fill Both, Expand On]

    finishHTk main
