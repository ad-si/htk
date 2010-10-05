-- | A simple window to display uneditable, scrollable text (e.g. error logs)

module HTk.Toolkit.TextDisplay(

  createTextDisplayExt, -- :: String-> String-> [Config ...] -> IO()-> IO Toplvl
  createTextDisplay     -- :: String-> String-> [Config ...] -> IO ()

) where

import Util.Computation

import Events.Events

import HTk.Toplevel.HTk
import HTk.Toolkit.ScrollBox

-- | Display some (longish) text in an uneditable, scrollable editor.
-- Returns immediately-- the display is forked off to separate thread.
createTextDisplayExt :: String
   -- ^ the title of the window
   -> String
   -- ^ the text to be displayed
   -> [Config Editor]
   -- ^ configuration options for the text editor
   -> IO()
   -- ^ action to be executed when the window is closed
   -> IO (Toplevel,Editor)
   -- ^ the window in which the text is displayed
createTextDisplayExt title txt conf unpost =
  do win <- createToplevel [text title]
     b   <- newFrame win  [relief Groove, borderwidth (cm 0.05)]
     t   <- newLabel b [text title, font (Helvetica, Roman, 18::Int)]
     q   <- newButton b [text "Close", width 12]
     (sb, ed) <- newScrollBox b (\p-> newEditor p (state Normal:conf)) []
     pack b [Side AtTop, Expand On, Fill Both]
     pack t [Side AtTop, Expand Off, PadY 10]
     pack sb [Side AtTop, Expand On, Fill Both]
     pack ed [Side AtTop, Expand On, Fill Both]
     pack q [Side AtRight, PadX 5, PadY 5]

     ed # value txt
     ed # state Disabled

     quit <- clicked q
     _ <- spawnEvent (quit >>> do destroy win; unpost)
     return (win, ed)

-- | Display some (longish) text in an uneditable, scrollable editor.
-- Simplified version of createTextDisplayExt
createTextDisplay :: String
   -- ^ the title of the window
   -> String
   -- ^ the text to be displayed
   -> [Config Editor]
   -- ^ configuration options for the text editor
   -> IO()
createTextDisplay t txt conf = do createTextDisplayExt t txt conf done; done
