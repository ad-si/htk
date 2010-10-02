-- | This module provides functionality on the current focus.
module HTk.Components.Focus (

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

import HTk.Kernel.Core
import HTk.Kernel.BaseClasses(Widget)
import Data.Char(isSpace)
import Util.Computation
import HTk.Containers.Window


-- -----------------------------------------------------------------------
-- Grab Status
-- -----------------------------------------------------------------------

-- | The @GrabStatus@ datatype.
data GrabStatus = Local | Global deriving (Eq,Ord,Enum)


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIValue GrabStatus where
  cdefault = Local

-- | Internal.
instance Read GrabStatus where
  readsPrec p b =
    case dropWhile (isSpace) b of
      'l':'o':'c':'a':'l':xs -> [(Local,xs)]
      'g':'l':'o':'b':'a':'l':xs -> [(Global,xs)]
      _ -> []

-- | Internal.
instance Show GrabStatus where
  showsPrec d p r = (case p of
                       Local -> "local"
                       Global -> "global") ++ r


-- -----------------------------------------------------------------------
-- current grab
-- -----------------------------------------------------------------------

-- | The @CurrentGrab@ datatype.
data CurrentGrab = CurrentGrab GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

-- | Internal.
instance Object CurrentGrab where
  objectID (CurrentGrab obj) = objectID obj

-- | Internal.
instance GUIObject CurrentGrab where
  toGUIObject (CurrentGrab obj) = obj
  cname _ = ""

-- | The current grab has standard widget properties
-- (concerning focus, cursor).
instance Widget CurrentGrab


-- -----------------------------------------------------------------------
-- window grabs
-- -----------------------------------------------------------------------

-- | Grabs the focus local.
grabLocal :: Widget w => w
   -- ^ the concerned widget.
   -> IO ()
   -- ^ None.
grabLocal wid = execMethod wid (\name -> [tkGrabLocal name])

-- | Grabs the focus global.
grabGlobal :: Widget w => w
   -- ^ the concerned widget.
   -> IO ()
   -- ^ None.
grabGlobal wid =
  execMethod wid (\name -> ["grab set -global " ++ show name])

-- | Releases a focus grab.
releaseGrab :: Widget w => w
   -- ^ the concerned widget.
   -> IO ()
   -- ^ None.
releaseGrab wid = execMethod wid (\name -> ["grab release " ++ show name])

-- | Gets the grab status from a widget.
getGrabStatus :: Widget w => w
   -- ^ the concerned widget.
   -> IO (Maybe GrabStatus)
   -- ^ The current grab status (if available).
getGrabStatus wid =
  do
    (RawData str) <- evalMethod wid (\nm -> ["grab status " ++ show nm])
    case dropWhile isSpace str of
      ('n':'o':'n':'e':_) -> return Nothing
      s          -> do {v <- creadTk s; return (Just v)}

-- | Gets the current grab.
getCurrentGrab :: IO (Maybe CurrentGrab)
   -- ^ The current grab (if available).
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

-- | The @FocusModel@ datatype (focus model of a toplevel
-- window).
data FocusModel = ActiveFocus | PassiveFocus deriving (Eq,Ord,Enum)

-- | Internal.
instance GUIValue FocusModel where
  cdefault = PassiveFocus

-- | Internal.
instance Read FocusModel where
   readsPrec p b =
     case dropWhile (isSpace) b of
        'a':'c':'t':'i':'v':'e':xs -> [(ActiveFocus,xs)]
        'p':'a':'s':'s':'i':'v':'e':xs -> [(PassiveFocus,xs)]
        _ -> []

-- | Internal.
instance Show FocusModel where
   showsPrec d p r =
      (case p of
        ActiveFocus -> "active"
        PassiveFocus -> "passive"
        ) ++ r


-- -----------------------------------------------------------------------
-- window focus
-- -----------------------------------------------------------------------

-- | Sets a window\'s focus model.
focusModel :: Window w => FocusModel -> Config w
focusModel fm win = cset win "focusmodel" fm

-- | Gets a window\'s focus model.
getFocusModel :: Window w => w -> IO FocusModel
getFocusModel win = cget win "focusmodel"


-- -----------------------------------------------------------------------
-- current focus
-- -----------------------------------------------------------------------

-- | The @CurrentFocus@ datatype.
data CurrentFocus = CurrentFocus GUIOBJECT


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

-- | Internal.
instance Object CurrentFocus where
        objectID (CurrentFocus obj) = objectID obj

-- | Internal.
instance GUIObject CurrentFocus where
        toGUIObject (CurrentFocus obj) = obj
        cname _ = ""

-- | The current focus is always a widget and has standard widget properties
-- (concerning focus, cursor).
instance Widget CurrentFocus


-- -----------------------------------------------------------------------
-- input focus
-- -----------------------------------------------------------------------

-- | Gets the current focus inside a window.
getFocus :: Window w => w
   -- ^ the concerned window.
   -> IO (Maybe CurrentFocus)
   -- ^ The current focus (if available).
getFocus win =
  evalMethod win (\wn -> ["focus -displayof " ++ show wn]) >>=
  toCurrentFocus . WidgetName

-- | Sets the current for the containing window.
setFocus :: Widget w => w
   -- ^ The widget to focus.
   -> IO ()
   -- ^ None.
setFocus w = execMethod w (\wn -> ["focus " ++ show wn])

-- | Forces the current focus for the containing window.
forceFocus :: Widget w => w
   -- ^ The widget to focus.
   -> IO ()
   -- ^ None.
forceFocus w = execMethod w (\wn -> ["focus -force " ++ show wn])

-- | Gets the last focused widget inside a window.
getRecentFocus :: Window w => w
   -- ^ the concerned window.
   -> IO (Maybe CurrentFocus)
   -- ^ The recent focus (if available).
getRecentFocus w =
  evalMethod w (\wn -> ["focus -lastfor " ++ show wn])    >>=
  toCurrentFocus . WidgetName

toCurrentFocus :: WidgetName -> IO (Maybe CurrentFocus)
toCurrentFocus name =
  do
    obj <- lookupGUIObjectByName name
    case obj of Nothing -> return Nothing
                (Just o) -> (return . Just . CurrentFocus) o
