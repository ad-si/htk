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
import FileDialog
import System

main :: IO ()
main =
  do
    args <- getArgs
    homedir <- getEnv "HOME"
    let dir = case args of a:_ -> a; [] -> homedir
    main <- initHTk [text "file dialog example"]
    open <- newButton main [text ("Open file dialog ("++ dir++ ")"),
		            width 60] :: IO (Button String)
    pack open [PadX 10, PadY 5]
    msg <- newLabel main [text "Welcome", font (Lucida, 12::Int),
                          height 2, relief Sunken, bg "white"]
    pack msg [PadX 10, PadY 5, Fill X, Expand On]
    quit <- newButton main [text "Quit"]
              :: IO (Button String)
    pack quit [PadX 10, PadY 5, Fill X, Expand On]
    clickedquit <- clicked quit
    clickedopen <- clicked open
    spawnEvent (forever ((clickedquit >> always (destroy main)) +>
                         (clickedopen >>>
                            do selev <- fileDialog "Open file" dir
			       file  <- sync selev
			       case file of
                                 Just fp ->
                                   msg # text ("selected " ++ fp) >> done
                                 _ -> msg #
                                        text "dialog canceled" >> done)))
    finishHTk main
