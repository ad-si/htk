{- ------------------------------------------------------------------------
 -
 - File dialog example
 -
 - Author: ludi
 - $Revision$ from $Date$  
 -
 - ------------------------------------------------------------------------ -}

module Main (main) where

import HTk
import Concurrency
import Button
import FileDialog

main =
  do
    htk []
    main <- newVBox []
    win <- window main [text "file dialog example"]
    open <- newButton [pad Horizontal 10, pad Vertical 5, parent main,
                       text "Open dialog", width 15,
                       command (\ () -> launchFileDialog "Open file" "/" >> done)]
    quit <- newButton [pad Horizontal 10, pad Vertical 5, parent main,
                       text "Quit", width 15,
                       command (\ () -> destroy win)]
    interactor (\i -> triggered open +> triggered quit)
    sync (destroyed win)
