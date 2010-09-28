
import HTk.Toplevel.HTk

main :: IO ()
main =
  do
    htk <- initHTk [withdrawMainWin]
    t <- createToplevel [text "not the main window"]
    b <- newButton t [text "Quit"]
    pack b []
    clickedb <- clicked b
    spawnEvent (clickedb >>> destroy htk)
    finishHTk
