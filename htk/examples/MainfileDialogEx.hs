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
import Label
import FileDialog
import System

main =
  do
    homedir <- getEnv "HOME"
    htk []
    main <- newVFBox []
    win <- window main [text "file dialog example"]
    open <- newButton [pad Horizontal 10, pad Vertical 5, parent main,
                       text "Open file dialog", width 60]
    openModal <- newButton [pad Horizontal 10, pad Vertical 5, parent main,
                            text "Open modal file dialog"]
    msg <- newLabel [pad Horizontal 10, pad Vertical 5, value "Welcome",
                     font (Lucida, 12::Int), relief Sunken, bg "white",
                     height 2, parent main]
    quit <- newButton [pad Horizontal 10, pad Vertical 5, parent main,
                       text "Quit", command (\ () -> destroy win)]
    open # command (\ () -> do
                              fd <- fileDialog "Open file" homedir
                              interactor
                                (\i -> fileChosen fd >>>=
                                         \mfp ->
                                           case mfp of
                                             Just fp ->
                                               msg # value ("selected " ++
                                                              fp) >> done
                                             _ -> msg # value
                                                          "dialog canceled" >>
                                                        done))
    interactor (\i -> triggered open +> triggered quit)
    sync (destroyed win)
