
module Main (main) where

import HTk.Toplevel.HTk
import HTk.Toolkit.SpinButton
import Reactor.ReferenceVariables
import HTk.Tix.LabelFrame

main :: IO ()
main =
  do
    wref <- newRef (300 :: Distance)
    href <- newRef (200 :: Distance)

    main <- initHTk [text "spin buttons"]
    win <- createToplevel [text "window", size (300, 200)]

    titlab <- newLabel main [text "Window size:", anchor Center]
    grid titlab [GridPos (0,0), Columnspan 3, GridPadX 20, GridPadY 20]

    lab1 <- newLabel main [text "Width"]
    grid lab1 [GridPos (0,1)]

    (ent1 :: Entry Distance) <- newEntry main [width 4,
                                               value (300 :: Distance)]
    grid ent1 [GridPos (1,1), Sticky E]

    let check :: Distance -> Distance -> Distance
        check val max =
          if val < 0 then 0 else if val > max then max else val

    sb1 <- newSpinButton main (\sp -> synchronize main
                                        (do
                                           w <- getRef wref
                                           let nuw = check
                                                       (case sp of
                                                          Up -> w + 10
                                                          _ -> w - 10)
                                                       640
                                           setRef wref nuw
                                           ent1 # value nuw
                                           win # width nuw)) []
    grid sb1 [GridPos (2,1), Sticky W]

    lab2 <- newLabel main [text "Height"]
    grid lab2 [GridPos (0,2)]

    (ent2 :: Entry Distance) <- newEntry main [width 4,
                                               value (200 :: Distance)]
    grid ent2 [GridPos (1,2), Sticky E]

    sb2 <- newSpinButton main (\sp -> synchronize main
                                        (do
                                           h <- getRef href
                                           let nuh = check
                                                       (case sp of
                                                         Up -> h + 10
                                                         _ -> h - 10)
                                                       480
                                           setRef href nuh
                                           ent2 # value nuh
                                           win # height nuh)) []
    grid sb2 [GridPos (2,2), Sticky W]

    quitbutton <- newButton main [text "Quit"]
    grid quitbutton [GridPos (0,3), Columnspan 3, Sticky NSEW,
                     GridPadX 20, GridPadY 20]
    clicked_quitbutton <- clicked quitbutton
    spawnEvent (clicked_quitbutton >>> destroy main)

    finishHTk
