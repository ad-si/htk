{- -----------------------------------------------------------------------
 -
 - HTk Examples: Canvas #1
 -
 - This example demonstrates canvasses.
 -
 - Author: cxl 
 - $Revision$ from $Date$  
 -
 - -------------------------------------------------------------------- -}


module Main (main) where

import HTk

main :: IO ()
main =
  do
    main <- initHTk [text "HTk Drawing Pad"]
    cnv <- newCanvas main [size(cm 15, cm 15), background "white"]
    pack cnv []
    putRects cnv
    (click, _) <- bind cnv [WishEvent [] (ButtonPress (Just (BNo 3)))]
    let listenMouseClicks :: Event ()
        listenMouseClicks = do
                              (x, y) <- click >>>= \i-> return(x i, y i)
                              always (do
                                        putRect cnv ("yellow", (x, y)))
    spawnEvent (forever listenMouseClicks)
    (htk_destr, _) <- bindSimple main Destroy
    sync htk_destr
    finishHTk main

putRects cnv = mapM (putRect cnv) [("red", (cm 0.2, cm 4)),
			           ("green", (cm 2.2, cm 4)),
		                   ("blue", (cm 4.2, cm 4))]

putRect cnv (col, pos) =
  do
    rect <- createRectangle cnv [size(cm 1, cm 1), position pos,
			         outline "black", filling col]
    (move, _) <- bind rect [WishEvent [Button1] Motion]
    let moveRectangle :: Rectangle -> Position -> Event ()
        moveRectangle rect (x0, y0) =
          do
            (x, y) <- move >>>= \evinf -> return (x evinf, y evinf)
            always (moveItem rect (x - x0) (y - y0))
            moveRectangle rect (x, y)
    spawnEvent (moveRectangle rect pos)
    done
