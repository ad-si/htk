-- -----------------------------------------------------------------------
--
-- HH  HH  TTTTTTTT  kk
-- HH  HH     TT     kk  kk
-- HH  HH     TT     kk kk
-- HHHHHH     TT     kkkk
-- HH  HH     TT     kk kk
-- HH  HH     TT     kk  kk
-- HH  HH     TT     kk   kk
--
-- (c) University of Bremen
--
-- Original author: Einar Karlsen,
--                  University of Bremen
--                  email: ewk@informatik.uni-bremen.de
--
-- Current authors: cxl, ger, ludi
--
-- Module : ButtonWidget
--
-- $Revision$ from $Date$
-- Last modification by $Author$
--
-- -----------------------------------------------------------------------


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

class Widget w => ButtonWidget w where
  flash   :: w -> IO ()
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

buttonColours :: HasColour w => w -> ConfigID -> Bool
buttonColours w "background" = True
buttonColours w "foreground" = True
buttonColours w "activebackground" = True
buttonColours w "activeforeground" = True
buttonColours w "disabledforeground" = True
buttonColours w _ = False
