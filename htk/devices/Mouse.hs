{- #######################################################################

MODULE        : Mouse
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Defines some archetypical mouse events.

   #################################################################### -}


module Mouse (

  GrabStatus(..),

  CurrentGrab(..),
  grabLocal, 
  grabGlobal, 
  releaseGrab,
  returnGrab,
  getGrabStatus,
  getCurrentGrab,

) where

import Core
import BaseClasses(Widget)
import Char(isSpace)
import Computation

-- -----------------------------------------------------------------------
-- Grab Status
-- -----------------------------------------------------------------------

data GrabStatus = Local | Global deriving (Eq,Ord,Enum)


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

instance GUIValue GrabStatus where
  cdefault = Local

instance Read GrabStatus where
  readsPrec p b =
    case dropWhile (isSpace) b of
      'l':'o':'c':'a':'l':xs -> [(Local,xs)]
      'g':'l':'o':'b':'a':'l':xs -> [(Global,xs)]
      _ -> []

instance Show GrabStatus where
  showsPrec d p r = (case p of 
                       Local -> "local" 
                       Global -> "global") ++ r


-- -----------------------------------------------------------------------
-- current grab
-- -----------------------------------------------------------------------

data CurrentGrab = CurrentGrab GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

instance Object CurrentGrab where
  objectID (CurrentGrab obj) = objectID obj

instance GUIObject CurrentGrab where
  toGUIObject (CurrentGrab obj) = obj
  cname _ = ""

instance Widget CurrentGrab  


-- -----------------------------------------------------------------------
-- window grabs
-- -----------------------------------------------------------------------

grabLocal :: Widget w => w -> IO ()
grabLocal win = execMethod win (\name -> [tkGrabLocal name])

grabGlobal :: Widget w => w -> IO ()
grabGlobal win =
  execMethod win (\name -> ["grab set -global " ++ show name])

releaseGrab :: Widget w => w -> IO ()
releaseGrab win = execMethod win (\name -> ["grab release " ++ show name])

getGrabStatus :: Widget w => w -> IO (Maybe GrabStatus)
getGrabStatus win =
  do
    (RawData str) <- evalMethod win (\nm -> ["grab status " ++ show nm])
    case dropWhile isSpace str of
      ('n':'o':'n':'e':_) -> return Nothing
      s          -> do {v <- creadTk s; return (Just v)}

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
