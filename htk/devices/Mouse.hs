{- #########################################################################

MODULE        : Mouse
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Defines some archetypical mouse events.


   ######################################################################### -}

module Mouse (
        GrabStatus(..),

        CurrentGrab(..),
        grabLocal, 
        grabGlobal, 
        releaseGrab,
        returnGrab,
        getGrabStatus,
        getCurrentGrab,

        mouseEvent,
        mouseEvent',
        mouseMotion,
        mouseEnter,
        mouseLeave,
        mouseButtonPress,
        mouseButtonRelease
        ) where

import SIM
import GUICore
import Char(isSpace)
import Debug(debug)

import UserInteraction

-- --------------------------------------------------------------------------
-- Grab Status 
-- --------------------------------------------------------------------------           
data GrabStatus = Local | Global deriving (Eq,Ord,Enum)


-- --------------------------------------------------------------------------
-- Instantiations 
-- --------------------------------------------------------------------------           
instance GUIValue GrabStatus where
        cdefault = Local

instance Read GrabStatus where
   readsPrec p b =
     case dropWhile (isSpace) b of
        'l':'o':'c':'a':'l':xs -> [(Local,xs)]
        'g':'l':'o':'b':'a':'l':xs -> [(Global,xs)]
        _ -> []

instance Show GrabStatus where
   showsPrec d p r = 
      (case p of 
        Local -> "local" 
        Global -> "global"
        ) ++ r


-- --------------------------------------------------------------------------
-- Current Grab 
-- --------------------------------------------------------------------------

data CurrentGrab = CurrentGrab GUIOBJECT deriving Eq


-- --------------------------------------------------------------------------
-- Instantiations 
-- --------------------------------------------------------------------------

instance Object CurrentGrab where
        objectID (CurrentGrab obj) = objectID obj

instance GUIObject CurrentGrab where
        toGUIObject (CurrentGrab obj) = obj
        cname _ = ""

instance Widget CurrentGrab  


-- --------------------------------------------------------------------------
-- Window Grabs 
-- --------------------------------------------------------------------------           
grabLocal :: (Interactive w, Widget w) => w -> IO ()
grabLocal win = execMethod win (\name -> [tkGrabLocal name])

grabGlobal :: (Interactive w, Widget w) => w -> IO ()
grabGlobal win = execMethod win (\name -> ["grab set -global " ++ show name])

releaseGrab :: (Interactive w, Widget w) => w -> IO ()
releaseGrab win = execMethod win (\name -> ["grab release " ++ show name])

getGrabStatus :: (Interactive w, Widget w) => w -> IO (Maybe GrabStatus)
getGrabStatus win = do {
        (RawData str) <- evalMethod win (\nm -> ["grab status " ++ show nm]);
        case dropWhile (isSpace) str of
                ('n':'o':'n':'e':_) -> return Nothing
                s          -> do {v <- creadTk s; return (Just v)} 
        }   

getCurrentGrab :: IO (Maybe CurrentGrab)
getCurrentGrab = evalTclScript ["grab current "] >>= toCurrentGrab . WidgetName

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


-- --------------------------------------------------------------------------
-- Mouse Inter Action 
-- --------------------------------------------------------------------------

mouseEvent :: (Interactive w,GUIEventDesignator e)  
           => w -> e -> IA (Position,Int)
mouseEvent w e = userinteraction w e Request >>>= return . getMouseEventInfo
    where getMouseEventInfo info = ((xfield info,yfield info),buttonno info)

-- Experimental code: user interactions (see kernel/UserInteraction.hs)
-- Don't use this yet.
mouseEvent' :: (Interactive w,GUIEventDesignator e)  
           => w -> e -> UIA (Position,Int)
mouseEvent' w e = UIA(userinteraction w e Request, [toGUIObject w]) >>>= return . getMouseEventInfo
    where getMouseEventInfo info = ((xfield info,yfield info),buttonno info)


mouseMotion :: Interactive w => w -> IA Position
mouseMotion w = mouseEvent w Motion >>>= return . fst

mouseEnter :: Interactive w => w -> IA Position
mouseEnter w = mouseEvent w Enter >>>= return . fst

mouseLeave :: Interactive w => w -> IA Position
mouseLeave w = mouseEvent w Leave >>>= return . fst

mouseButtonPress :: Interactive w => w -> Int -> IA Position
mouseButtonPress w i = mouseEvent w (ButtonPress (Just i)) >>>= return . fst

mouseButtonRelease :: Interactive w => w -> Int -> IA Position
mouseButtonRelease w i = mouseEvent w (ButtonRelease (Just i)) >>>= return . fst


-- --------------------------------------------------------------------------
-- Tk Commands 
-- --------------------------------------------------------------------------

tkGrabLocal :: ObjectName -> TclCmd
tkGrabLocal name = "grab set " ++ show name
{-# INLINE tkGrabLocal #-}
