
module Main (main) where

import Reactor.ReferenceVariables

import HTk.Toplevel.HTk
import HTk.Toolkit.FileDialog
import System
import Control.Exception

exportPS :: Canvas -> IO ()
exportPS cnv =
  do
    homeDir <- getEnv "HOME"
    homeDirRef <- newRef homeDir
    selev <- fileDialog "Open file" homeDirRef
    file  <- sync selev
    case file of
      Just fp -> do
                try (postscript cnv [psfile fp] >>
                      putStrLn "postscript exported")
                   :: IO (Either SomeException ())
                done
      _ -> done


main :: IO ()
main =
  do
    main <- initHTk [text "HTk Drawing Pad"]
    cnv <- newCanvas main [size(cm 15, cm 15), background "white"]
    pack cnv []
    putRects cnv
    but <- newButton main [text "PS export"]
    clickedbut <- clicked but
    pack but [Fill X, Expand On]
    (click, _) <- bind cnv [WishEvent [] (ButtonPress (Just 3))]
    let listenMouseClicks :: Event ()
        listenMouseClicks = do
                              (x, y) <- click >>>= \i-> return(x i, y i)
                              always (do
                                        putRect cnv ("yellow", (x, y)))
    spawnEvent (forever (listenMouseClicks +>
                         (clickedbut >>> exportPS cnv)))
    finishHTk

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
