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

{-
open :: Orientation -> IO ()
open or =
  do
    pw <- createPanedWindow []
    pane1 <- createWindowPane pw []
    pane2 <- createWindowPane pw []

    l <- newLabel pane1 [text "Hello World"]
    pack l [Fill Both, Expand On]

    nb <- newNoteBook pane2 [size (400, 450)]
    pack nb [PadX 10, PadY 10, Fill X, Expand On]

    page1 <- createNoteBookPage nb "Entries" []
    page2 <- createNoteBookPage nb "Info" []

    lf_pers <- newLabelFrame page1 [text "Personal"]
    grid lf_pers [GridPos (0,0), Sticky NSEW]

    l1  <- newLabel lf_pers [text "Name:", tooltip "enter your name"]
    grid l1 [GridPos (0,0), GridPadX 5, GridPadY 5]

    e1 <- newEntry lf_pers [tooltip "enter your name"] ::IO (Entry String)
    grid e1 [GridPos (1,0), GridPadX 5, GridPadY 5]

    l2 <- newLabel lf_pers [text "Age:", tooltip "enter your age"]
    grid l2 [GridPos (0,1), GridPadX 5, GridPadY 5]

    e2 <- newEntry lf_pers [tooltip "enter your age"] ::IO (Entry String)
    grid e2 [GridPos (1,1), GridPadX 5, GridPadY 5]

    lf_web <- newLabelFrame page1 [text "WWW"]
    grid lf_web [GridPos (0,1), Sticky NSEW]

    l3  <- newLabel lf_web [text "Email:", tooltip "enter your email"]
    grid l3 [GridPos (0,0), GridPadX 5, GridPadY 5]

    e3 <- newEntry lf_web [tooltip "enter your email"] ::IO (Entry String)
    grid e3 [GridPos (1,0), GridPadX 5, GridPadY 5]

    l4 <- newLabel lf_web [text "Homepage:",
                           tooltip "enter your homepage"]
    grid l4 [GridPos (0,1), GridPadX 5, GridPadY 5]

    e4 <- newEntry lf_web [tooltip "enter your homepage"]
            :: IO (Entry String)
    grid e4 [GridPos (1,1), GridPadX 5, GridPadY 5]
-}

main:: IO ()
main =
  do
    main <- initHTk [size (600,500), text "paned window example!"]

    pwh <- newPanedWindow main Horizontal []
    paneh1 <- createPane pwh [] []
    paneh2 <- createPane pwh [] []

    pwv <- newPanedWindow paneh1 Vertical []
    panev1 <- createPane pwv [] []
    panev2 <- createPane pwv [] []

    pack pwh [Fill Both, Expand On]
    pack pwv [Fill Both, Expand On]
{-
    lab1 <- newButton paneh1 [text "pane1"] :: IO (Button String)
    pack lab1 [Fill Both, Expand On]
-}
    lab2 <- newButton paneh2 [text "pane2"] :: IO (Button String)
    pack lab2 [Fill Both, Expand On]

    lab3 <- newButton panev1 [text "pane3"] :: IO (Button String)
    pack lab3 [Fill Both, Expand On]

    lab4 <- newButton panev2 [text "pane4"] :: IO (Button String)
    pack lab4 [Fill Both, Expand On]

{-
    grid pwh [GridPos (0,0), Sticky NSEW]
    grid pwv [GridPos (1,0), Sticky NSEW]
-}

    (htk_destr, _) <- bindSimple main Destroy
    sync (htk_destr)
