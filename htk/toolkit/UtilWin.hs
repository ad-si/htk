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

module UtilWin (

--display error or warning, then continue
  UtilWin.error,
  UtilWin.warning,

--display error or warning, then call continuation
  error_cc,
  warning_cc,

--Demand confirmation, then call continuation.
--If user clicks "cancel", do nothing
  confirm,

--Display an informative message. The returned closure closes this window,
--ideally after it has been displayed for at least 10 secs or something.
--This window can't be closed by the user.
  info_cc,
--as above, but let the user close the window ("display&forget")
   UtilWin.info,

{-
--Display a text
--There are two variations, one where the id of the text widget is
--explicitly passed along (although the widget has not been created
--at this point), one where it is created by this function and then
--passed to the cc function.
  display, --: {title: string, width: int, height: int,
           --   text: SmlTk.AnnoText, cc: SmlTk.WidId-> unit}-> unit

  display_id, --: {winId: SmlTk.WinId, widId: SmlTk.WidId, title: string,
              --   width: int, height: int, text: SmlTk.AnnoText}-> unit
-}

{-
--prompt the user to enter a text in a separate window w/ a text widget
-- parameters are pretty self-explanatory, except for cc which is
--the continuation to be called with the entered text.
  enterText, -- : {title : string, prompt : string, default : string,
             --    width : int, height : int, 
             --    cc : string-> unit} -> unit
-}

{-
--prompt the user to enter a line of text in a separate windw 
--
--Parameters are as before (but no height). This function uses an 
--artificially intelligent semi-heuristic fuzzy logic based algorithm 
--implemented in Java to determine wether the text entry should be 
--alongside the prompt or below it.
  enterLine, -- : {title : string, prompt : string, default : string, 
	     --    width : int, cc : string-> unit } -> unit

--(Actually, if the prompt is at least twice as long as the text entry,
-- it is below the prompt, otherwise to its right).
-}

{-
--Auxiliary version of enterText which produces n entry widgets,
--as specified by a list of heights, also takes some more widgets
--and places them between the text widget, and the ok/cancel bottoms *)
  enterText0, -- : {title : string, prompt : string, default : string,
              --    widgetsbelow : SmlTk.Widget list, 
              --    heights : int list, headers : string list, 
              --    width : int, cc : string list -> unit} -> unit
-}

) where

import HTk
import BitMap

center :: Toplevel -> IO ()
center win =
  do
    let scr = Screen win
    sh <- getScreenHeight scr
    sw <- getScreenWidth scr
    (h, w) <- getSize win
    putStrLn ("(" ++ show sh ++ "," ++ show sw ++ ") (" ++ show h ++ "," ++ show w ++ ")")

simplestWin :: String -> BitMapHandle -> String -> IO Toplevel
simplestWin title bm txt =
  do
   win <- createToplevel [text title]
   box <- newHBox win []
   img <- newLabel box [bitmap bm] :: IO (Label BitMap)
   pack img [PadX 20]
   mes <- newMessage box [text txt, width 280] :: IO (Message String)
   pack mes [PadX 20]
   pack box [PadY 10]
   return win

simpleWin :: String -> BitMapHandle -> Bool -> String -> IO () -> IO ()
simpleWin title bm canc txt cc =
 do
   win <- simplestWin title bm txt
   box <- newHBox win []
   ok <- newButton box [text "Ok"] :: IO (Button String)
   pack ok [PadX 5]
   mcanc <-
     (if canc then
        do
          cancel <- newButton box [text "Cancel"] :: IO (Button String)
          pack cancel [PadX 5]
          clicked_cancel <- clicked cancel
          return (Just clicked_cancel)
      else return Nothing)
   pack box [Side AtBottom, PadY 5]
   clickedok <- clicked ok
   death <- newChannel
   let rest :: Event ()
       rest = case mcanc of
                Just clicked_cancel ->
                  clicked_cancel >> always (destroy win) +>
                  receive death
                _ -> receive death
   spawnEvent ((clickedok >> always ((if canc then cc else done) >>
                                     (destroy win))) +> rest)
   (win_destr, _) <- bindSimple win Destroy  -- unbind is not necessary,
                                             -- because the window is
                                             -- destroyed anyway
   center win
   sync win_destr
   syncNoWait (send death ())
   if canc then done else cc

error :: String -> IO ()
error txt = error_cc txt done

error_cc :: String -> IO () -> IO ()
error_cc = simpleWin "Error Message" errmap False

warning :: String -> IO ()
warning txt = warning_cc txt done

warning_cc :: String -> IO () -> IO ()
warning_cc = simpleWin "Warning Message" BitMap.warning False

confirm :: String -> IO () -> IO ()
confirm = simpleWin "Please Confirm Or Abort" questhead True

info :: String -> IO ()
info txt = simpleWin "Information" BitMap.info False txt done

info_cc :: String -> IO (IO ())
info_cc txt =
  do
    win <- simplestWin "Information" BitMap.info txt
    return (destroy win)
