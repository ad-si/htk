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

---
-- A simple window to display uneditable, scrollable text (e.g. error logs)

module TextDisplay(

  createTextDisplay -- :: String-> String-> [Config ...] -> IO ()

) where

import HTk
import Core
import ScrollBox


---
-- Display some (longish) text in an uneditable, scrollable editor.
-- Returns immediately-- the display is forked off to separate thread.
-- @param title   - the title of the window
-- @param txt     - the text to be displayed
-- @param conf    - configuration options for the text editor
createTextDisplay :: String-> String-> [Config Editor]-> IO ()
createTextDisplay title txt conf =
  do win <- createToplevel [text title]
     b   <- newFrame win  [relief Groove, borderwidth (cm 0.05)]    
     t   <- newLabel b [text title, HTk.font (Helvetica, Roman, 18::Int)]
     q   <- newButton b [text "Close", width 12]
     (sb, ed) <- newScrollBox b (\p-> newEditor p (state Normal:conf)) []
     pack b [Side AtTop, Fill X, Expand On]
     pack t [Side AtTop, Expand Off, PadY 10]
     pack sb [Side AtTop, Expand On]
     pack ed [Side AtTop, Expand On, Fill X]
     pack q [Side AtRight, PadX 5, PadY 5] 		 

     appendText ed txt
     ed # state Disabled

     quit <- clicked q
     spawnEvent (quit >>> destroy win)
     done
