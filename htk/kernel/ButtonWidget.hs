-- | This module provides general functionality on button widgets.
module ButtonWidget (

  ButtonWidget(..),
  buttonColours

) where

import Core
import BaseClasses(Widget)
import Computation
import Configuration


-- -----------------------------------------------------------------------
-- class ButtonWidget
-- -----------------------------------------------------------------------

-- | Button widgets instantiate the @class ButtonWidget@.
class Widget w => ButtonWidget w where
  -- Flashes the given button widget.
  flash   :: w -> IO ()
  -- Invokes the given button widget.
  invoke  :: w -> IO ()
  flash w  = do {try(execMethod w (\ nm -> tkFlash nm)); done}
  invoke w = execMethod (toGUIObject w) (\ nm -> tkInvoke nm)

tkFlash :: ObjectName -> TclScript
tkFlash (MenuItemName name i) = []
tkFlash name = [show name ++ " flash"]
{-# INLINE tkFlash #-}

tkInvoke :: ObjectName -> TclScript
tkInvoke (MenuItemName name i) = [show name ++ " invoke " ++ (show i)]
tkInvoke name = [show name ++ " invoke"]
{-# INLINE tkInvoke #-}


-- -----------------------------------------------------------------------
-- aux. button commands
-- -----------------------------------------------------------------------

-- | Internal.
buttonColours :: HasColour w => w -> ConfigID -> Bool
buttonColours w "background" = True
buttonColours w "foreground" = True
buttonColours w "activebackground" = True
buttonColours w "activeforeground" = True
buttonColours w "disabledforeground" = True
buttonColours w _ = False
