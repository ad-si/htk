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

import HTk

main :: IO ()
main =
  do
    htk <- initHTk [withdrawMainWin]
    t <- createToplevel [text "not the main window"]
    b <- newButton t [text "Quit"] :: IO (Button String)
    pack b []
    clickedb <- clicked b
    spawnEvent (clickedb >>> destroy htk)
    finishHTk htk
