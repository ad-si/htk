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
-- The <code>module Keyboard</code> exports general functionality on a
-- window's focus.
module Keyboard (

  Window,

  CurrentFocus,

  FocusModel(..), 
  focusModel,
  getFocusModel,
  getFocus,       
  setFocus,
  forceFocus,
  getRecentFocus

) where

import Core
import BaseClasses(Widget)
import Char(isSpace)
import Computation
import Window


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
