{- #######################################################################

MODULE        : Keyboard
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   :  

   #################################################################### -}

module Keyboard (

-- Window,

  CurrentFocus,

  FocusModel(..), 
  focusModel,
  getFocusModel,
  getFocus,       
  setFocus,
  forceFocus,
  getRecentFocus,

--  keystroke,
--  anyKeyPressed,
--  anyKeyReleased,
--  keyPressed,
--  keyReleased

) where

import Core
import BaseClasses(Widget)
import Char(isSpace)
import Computation
import Window


-- -----------------------------------------------------------------------
-- FocusModel
-- -----------------------------------------------------------------------

data FocusModel = ActiveFocus | PassiveFocus deriving (Eq,Ord,Enum)

instance GUIValue FocusModel where
  cdefault = PassiveFocus

instance Read FocusModel where
   readsPrec p b =
     case dropWhile (isSpace) b of
        'a':'c':'t':'i':'v':'e':xs -> [(ActiveFocus,xs)]
        'p':'a':'s':'s':'i':'v':'e':xs -> [(PassiveFocus,xs)]
        _ -> []

instance Show FocusModel where
   showsPrec d p r = 
      (case p of 
        ActiveFocus -> "active" 
        PassiveFocus -> "passive"
        ) ++ r


-- -----------------------------------------------------------------------
-- window focus
-- -----------------------------------------------------------------------

focusModel :: Window w => FocusModel -> Config w
focusModel fm win = cset win "focusmodel" fm

getFocusModel :: Window w => w -> IO FocusModel
getFocusModel win = cget win "focusmodel" 


-- -----------------------------------------------------------------------
-- current focus
-- -----------------------------------------------------------------------

data CurrentFocus = CurrentFocus GUIOBJECT


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

instance Object CurrentFocus where
        objectID (CurrentFocus obj) = objectID obj

instance GUIObject CurrentFocus where
        toGUIObject (CurrentFocus obj) = obj
        cname _ = ""

instance Widget CurrentFocus  


-- -----------------------------------------------------------------------
-- input focus
-- -----------------------------------------------------------------------

getFocus :: Window w => w -> IO (Maybe CurrentFocus)
getFocus win = 
  evalMethod win (\wn -> ["focus -displayof " ++ show wn]) >>= 
  toCurrentFocus . WidgetName

setFocus :: Widget w => w -> IO ()
setFocus w = execMethod w (\wn -> ["focus " ++ show wn])

forceFocus :: Widget w => w -> IO ()
forceFocus w = execMethod w (\wn -> ["focus -force " ++ show wn])

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


{-
-- -----------------------------------------------------------------------
-- archetypical keyboard events
-- -----------------------------------------------------------------------

keystroke :: (Interactive w, GUIEventDesignator e)  => 
                w -> e -> IA (Position,String)
keystroke w e = userinteraction w e Request >>>= return . getKeyEventInfo
   where getKeyEventInfo info = ((xfield info,yfield info),keysym info)
 
anyKeyPressed :: Interactive w => w -> IA (Position,String)
anyKeyPressed w = keystroke w (KeyPress Nothing) 

anyKeyReleased :: Interactive w => w -> IA (Position,String)
anyKeyReleased w = keystroke w (KeyRelease Nothing)

keyPressed :: Interactive w => w -> String -> IA Position
keyPressed w k = keystroke w (KeyPress (Just k)) >>>= return . fst

keyReleased :: Interactive w => w -> String -> IA Position
keyReleased w k = keystroke w (KeyRelease (Just k)) >>>= return . fst
-}
