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

module Main(main) where

import HTk
import Concurrent(threadDelay)

main :: IO ()
main =
  do
    htk <- initHTk [text "main window"]
      -- returns the root window handler

    lab1 <- newLabel htk [text "main window", size(30, 30), relief Raised]
    but1 <- newButton htk [text " Quit example ", size(10, 2)]
              :: IO (Button String)
    pack lab1 []
    pack but1 []
    clickedbut1 <- clicked but1
    spawnEvent (forever (clickedbut1 >> always (destroy htk)))

    top1 <- createToplevel [text "testing standard packer..."]
    -- creates a new window
    -- toplevels don't need to be packed

    lab2 <- newLabel top1 [text "left", bg "green", size(15, 5)]
    -- creates a new label in window top

    lab3 <- newLabel top1 [text "right", bg "red", size(15, 5)]
    -- creates another label

    pack lab2 [Side AtLeft]        -- packing
    pack lab3 [Side AtRight]


    -- ** testing grid packer and widget destruction **

    top2 <- createToplevel [text "testing grid packer..."]
    labels <- mapM (\col -> newLabel top2 [bg col, fg "white",
                                           size(10, 4), text col])
                   ["#a8c", "#9a9", "#7aa", "#6bb", "#23c", "#fe1"]
    mapM (\ (lab, pos) -> grid lab [GridPos pos])
         (zip labels [(0, 0), (1, 1), (2, 2), (3, 3), (4, 4), (5, 5)])

    txt <- getText lab1
    putStrLn ("text of label in main window is '" ++ txt ++ "'")

    mapM (\label -> do
                      destroy label
                      threadDelay 800000) labels
    destroy top2
    lab1 # text "finished"

    top3 <- createToplevel [text "testing events..."]
    lab4 <- newLabel top3 [bg "yellow", size(30, 30), text "Click me"]

    pack lab4 []

    (press, _) <- bind lab4 [WishEvent [] (ButtonPress (Just (BNo 1)))]

    let -- getCoords :: EventInfo -> IO (Int, Int)
        -- getCoords eventInfo = (x eventInfo, y eventInfo)          
        followMouse :: Event ()
        followMouse =
          do
            (x, y)<- press >>>= \info-> return (x info, y info) 
            always (do
                      lab4 # text ("Ouch\n(" ++ show x ++ ", " ++
                                   show y ++ ")")
                      threadDelay 800000
                      lab4 # text "Click me!"
                      done)
    spawnEvent (forever followMouse)

    (htk_destr, _) <- bindSimple htk Destroy
    sync (htk_destr)
