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
-- This module provides functionality on the current focus.
module Focus (

  Window,

  CurrentFocus,

  FocusModel(..), 
  focusModel,
  getFocusModel,
  getFocus,       
  setFocus,
  forceFocus,
  getRecentFocus,

  GrabStatus(..),

  CurrentGrab(..),
  grabLocal, 
  grabGlobal, 
  releaseGrab,
  returnGrab,
  getGrabStatus,
  getCurrentGrab

) where

import Core
import BaseClasses(Widget)
import Char(isSpace)
import Computation
import Window


-- -----------------------------------------------------------------------
-- Grab Status
-- -----------------------------------------------------------------------

---
-- The <code>GrabStatus</code> datatype.
data GrabStatus = Local | Global deriving (Eq,Ord,Enum)


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

---
-- Internal.
instance GUIValue GrabStatus where
---
-- Internal.
  cdefault = Local

---
-- Internal.
instance Read GrabStatus where
---
-- Internal.
  readsPrec p b =
    case dropWhile (isSpace) b of
      'l':'o':'c':'a':'l':xs -> [(Local,xs)]
      'g':'l':'o':'b':'a':'l':xs -> [(Global,xs)]
      _ -> []

---
-- Internal.
instance Show GrabStatus where
---
-- Internal.
  showsPrec d p r = (case p of 
                       Local -> "local" 
                       Global -> "global") ++ r


-- -----------------------------------------------------------------------
-- current grab
-- -----------------------------------------------------------------------

---
-- The <code>CurrentGrab</code> datatype.
data CurrentGrab = CurrentGrab GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

---
-- Internal.
instance Object CurrentGrab where
---
-- Internal.
  objectID (CurrentGrab obj) = objectID obj

---
-- Internal.
instance GUIObject CurrentGrab where
---
-- Internal.
  toGUIObject (CurrentGrab obj) = obj
---
-- Internal.
  cname _ = ""

---
-- The current grab has standard widget properties
-- (concerning focus, cursor).
instance Widget CurrentGrab


-- -----------------------------------------------------------------------
-- window grabs
-- -----------------------------------------------------------------------

---
-- Grabs the focus local.
-- @param wid     - the concerned widget.
-- @return result - None.
grabLocal :: Widget w => w -> IO ()
grabLocal wid = execMethod wid (\name -> [tkGrabLocal name])

---
-- Grabs the focus global.
-- @param wid     - the concerned widget.
-- @return result - None.
grabGlobal :: Widget w => w -> IO ()
grabGlobal wid =
  execMethod wid (\name -> ["grab set -global " ++ show name])

---
-- Releases a focus grab.
-- @param wid     - the concerned widget.
-- @return result - None.
releaseGrab :: Widget w => w -> IO ()
releaseGrab wid = execMethod wid (\name -> ["grab release " ++ show name])

---
-- Gets the grab status from a widget.
-- @param wid     - the concerned widget.
-- @return result - The current grab status (if available).
getGrabStatus :: Widget w => w -> IO (Maybe GrabStatus)
getGrabStatus wid =
  do
    (RawData str) <- evalMethod wid (\nm -> ["grab status " ++ show nm])
    case dropWhile isSpace str of
      ('n':'o':'n':'e':_) -> return Nothing
      s          -> do {v <- creadTk s; return (Just v)}

---
-- Gets the current grab.
-- @return result - The current grab (if available).
getCurrentGrab :: IO (Maybe CurrentGrab)
getCurrentGrab =
  evalTclScript ["grab current "] >>= toCurrentGrab . WidgetName

returnGrab :: Maybe CurrentGrab -> IO ()
returnGrab Nothing = done
returnGrab (Just g) = execMethod g (\name -> [tkGrabLocal name])

toCurrentGrab :: WidgetName -> IO (Maybe CurrentGrab)
toCurrentGrab name = do {
        obj <- lookupGUIObjectByName name;
        case obj of
                Nothing -> return Nothing
                (Just o) -> (return . Just . CurrentGrab) o
}


-- -----------------------------------------------------------------------
-- Tk Commands
-- -----------------------------------------------------------------------

tkGrabLocal :: ObjectName -> TclCmd
tkGrabLocal name = "grab set " ++ show name
{-# INLINE tkGrabLocal #-}


-- -----------------------------------------------------------------------
-- FocusModel
-- -----------------------------------------------------------------------

---
-- The <code>FocusModel</code> datatype (focus model of a toplevel
-- window).
data FocusModel = ActiveFocus | PassiveFocus deriving (Eq,Ord,Enum)

---
-- Internal.
instance GUIValue FocusModel where
---
-- Internal.
  cdefault = PassiveFocus

---
-- Internal.
instance Read FocusModel where
---
-- Internal.
   readsPrec p b =
     case dropWhile (isSpace) b of
        'a':'c':'t':'i':'v':'e':xs -> [(ActiveFocus,xs)]
        'p':'a':'s':'s':'i':'v':'e':xs -> [(PassiveFocus,xs)]
        _ -> []

---
-- Internal.
instance Show FocusModel where
---
-- Internal.
   showsPrec d p r = 
      (case p of 
        ActiveFocus -> "active" 
        PassiveFocus -> "passive"
        ) ++ r


-- -----------------------------------------------------------------------
-- window focus
-- -----------------------------------------------------------------------

---
-- Sets a window's focus model.
focusModel :: Window w => FocusModel -> Config w
focusModel fm win = cset win "focusmodel" fm

---
-- Gets a window's focus model.
getFocusModel :: Window w => w -> IO FocusModel
getFocusModel win = cget win "focusmodel" 


-- -----------------------------------------------------------------------
-- current focus
-- -----------------------------------------------------------------------

---
-- The <code>CurrentFocus</code> datatype.
data CurrentFocus = CurrentFocus GUIOBJECT


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

---
-- Internal.
instance Object CurrentFocus where
---
-- Internal.
        objectID (CurrentFocus obj) = objectID obj

---
-- Internal.
instance GUIObject CurrentFocus where
---
-- Internal.
        toGUIObject (CurrentFocus obj) = obj
---
-- Internal.
        cname _ = ""

---
-- The current focus is always a widget and has standard widget properties
-- (concerning focus, cursor).
instance Widget CurrentFocus  


-- -----------------------------------------------------------------------
-- input focus
-- -----------------------------------------------------------------------

---
-- Gets the current focus inside a window.
-- @param win     - the concerned window.
-- @return result - The current focus (if available).
getFocus :: Window w => w -> IO (Maybe CurrentFocus)
getFocus win = 
  evalMethod win (\wn -> ["focus -displayof " ++ show wn]) >>= 
  toCurrentFocus . WidgetName

---
-- Sets the current for the containing window.
-- @param w       - The widget to focus.
-- @return result - None.
setFocus :: Widget w => w -> IO ()
setFocus w = execMethod w (\wn -> ["focus " ++ show wn])

---
-- Forces the current focus for the containing window.
-- @param w       - The widget to focus.
-- @return result - None.
forceFocus :: Widget w => w -> IO ()
forceFocus w = execMethod w (\wn -> ["focus -force " ++ show wn])

---
-- Gets the last focused widget inside a window.
-- @param w       - the concerned window.
-- @return result - The recent focus (if available).
getRecentFocus :: Window w => w -> IO (Maybe CurrentFocus)
getRecentFocus w =
  evalMethod w (\wn -> ["focus -lastfor " ++ show wn])    >>= 
  toCurrentFocus . WidgetName

toCurrentFocus :: WidgetName -> IO (Maybe CurrentFocus)
toCurrentFocus name =
  do
    obj <- lookupGUIObjectByName name
    case obj of Nothing -> return Nothing
                (Just o) -> (return . Just . CurrentFocus) o
