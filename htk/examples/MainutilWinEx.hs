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
import UtilWin
import Concurrent(threadDelay)

cont :: IO ()
cont = putStrLn "continued"

main :: IO ()
main =
  do
    main <- initHTk [text "utility window example"]

    err <- newButton main [text "error", width 30] :: IO (Button String)
    pack err []
    clicked_err <- clicked err

    err_cc <- newButton main [text "error_cc", width 30]
                :: IO (Button String)
    pack err_cc []
    clicked_err_cc <- clicked err_cc

    warn <- newButton main [text "warning", width 30]
              :: IO (Button String)
    pack warn []
    clicked_warn <- clicked warn

    warn_cc <- newButton main [text "warning_cc", width 30]
                 :: IO (Button String)
    pack warn_cc []
    clicked_warn_cc <- clicked warn_cc

    conf <- newButton main [text "confirm", width 30]
              :: IO (Button String)
    pack conf []
    clicked_conf <- clicked conf

    inf <- newButton main [text "info", width 30] :: IO (Button String)
    pack inf []
    clicked_inf <- clicked inf

    inf_cc <- newButton main [text "info_cc", width 30]
                :: IO (Button String)
    pack inf_cc []
    clicked_inf_cc <- clicked inf_cc

    spawnEvent
      (forever (clicked_err >>>
                  UtilWin.error "This is an error message!" +>
                clicked_err_cc >>>
                  error_cc "This is a continued error message!" cont +>
                clicked_warn >>>
                  UtilWin.warning "This is a warning message!" +>
                clicked_warn_cc >>>
                  warning_cc "This is a continued warning message!"
                             cont +>
                clicked_conf >>> confirm "Really do this ?" cont +>
                clicked_inf >>>
                  UtilWin.info "This is an informative message." +>
                clicked_inf_cc >>>
                  do
                    destr <-
                      info_cc "This is a temporary informative message."
                    threadDelay 3000000
                    destr))

    (htk_destr, _) <- bindSimple main Destroy
    sync (htk_destr)
    finishHTk main