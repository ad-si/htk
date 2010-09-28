
module Main (main) where

import HTk.Toplevel.HTk
import HTk.Toolkit.LogWin

main :: IO ()
main =
  do
    main <- initHTk [text "log window example"]

    msg_var <- createTkVariable ""
    (msgent :: Entry String) <- newEntry main [variable msg_var, width 30]
    grid msgent [GridPos (0,1), GridPadY 5]
    (ret, _) <- bind msgent [WishEvent [] (KeyPress
                                             (Just (KeySym "Return")))]
    addmsg <- newButton main [text "Add"]
    clickedaddmsg <- clicked addmsg
    grid addmsg [GridPos (1,1)]

    quitbutton <- newButton main [text "Quit"]
    clickedquit <- clicked quitbutton
    grid quitbutton [GridPos(0,2), Columnspan 2, Sticky NSEW, GridPadX 20,
                     GridPadY 5]

    log <- createLogWin [text "log window", size (400, 300)]

    let writeMsg = do
                     str <- readTkVariable msg_var
                     writeLogWin log (str ++ "\n")

    spawnEvent (forever (clickedaddmsg >>> writeMsg +>
                         ret >>> writeMsg +>
                         clickedquit >>> destroy main))

    finishHTk
