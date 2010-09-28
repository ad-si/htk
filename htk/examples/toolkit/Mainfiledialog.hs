
module Main (main) where

import HTk.Toplevel.HTk
import HTk.Toolkit.FileDialog
import System
import Reactor.ReferenceVariables

main :: IO ()
main =
  do
    args <- getArgs
    homedir <- getEnv "HOME"
    let dir = case args of a:_ -> a; [] -> homedir
    fref <- newRef dir
    main <- initHTk [text "file dialog example"]
    open <- newButton main [text ("Open file dialog ("++ dir++ ")"),
                            width 60]
    nuopen <- newButton main [text ("Open new file dialog ("++ dir++ ")"),
                              width 60]
    pack open [PadX 10, PadY 5]
    pack nuopen [PadX 10, PadY 5]
    msg <- newLabel main [text "Welcome", font (Lucida, 12::Int),
                          height 2, relief Sunken, bg "white"]
    pack msg [PadX 10, PadY 5, Fill X, Expand On]
    quit <- newButton main [text "Quit"]
    pack quit [PadX 10, PadY 5, Fill X, Expand On]
    clickedquit <- clicked quit
    clickedopen <- clicked open
    clickednuopen <- clicked nuopen
    spawnEvent (forever ((clickedquit >> always (destroy main)) +>
                         (clickedopen >>>
                            do selev <- fileDialog "Open file" fref
                               file  <- sync selev
                               case file of
                                 Just fp ->
                                   msg # text ("selected " ++ fp) >> done
                                 _ -> msg #
                                        text "dialog canceled" >> done) +>
                         (clickednuopen >>>
                            do selev <- newFileDialog "Open file" fref
                               file  <- sync selev
                               case file of
                                 Just fp ->
                                   msg # text ("selected " ++ fp) >> done
                                 _ -> msg #
                                        text "dialog canceled" >> done)))
    finishHTk
