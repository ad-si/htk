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


module Mouse (

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
