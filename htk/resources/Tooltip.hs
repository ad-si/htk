module Tooltip (

  HasTooltip(..)

) where

import Wish
import GUIObject
import ReferenceVariables
import Computation


-- -----------------------------------------------------------------------
-- Tooltips (tix baloons, only available if tix is installed)
-- -----------------------------------------------------------------------

-- destruction is ignored, if no tooltip is defined

class GUIObject w => HasTooltip w where
  tooltip :: String -> Config w
  tooltip str w =
    do 
       if tixAvailable
          then 
             do
                nm <- getObjectName (toGUIObject w)
                execTclScript [
                   "destroy " ++ show nm ++ "ttip",
                   "tixBalloon " ++ show nm ++ "ttip",
                    show nm ++ "ttip bind " ++ show nm ++" -msg \"" ++ 
                       str ++ "\""
                   ]
          else 
             done
       return w

  destroyTooltip :: w -> IO ()
  destroyTooltip w =
    do nm <- getObjectName (toGUIObject w)
       execTclScript ["destroy " ++ show nm ++ "ttip"]
