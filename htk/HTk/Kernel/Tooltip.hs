module HTk.Kernel.Tooltip (

  HasTooltip(..)

) where

import HTk.Kernel.Wish
import HTk.Kernel.GUIObject
import Util.Computation


-- -----------------------------------------------------------------------
-- Tooltips (tix balloons, only available if using tixwish)
-- -----------------------------------------------------------------------

-- destruction is ignored, if no tooltip is defined

-- | Widgets can have tooltips (if you are using tixwish).
class GUIObject w => HasTooltip w where
  -- Sets the tooltip text for the given widget.
  tooltip :: String -> w -> IO w
  -- Destroys the tooltip of the given widget (if exists).
  destroyTooltip :: w -> IO ()

  tooltip str w =
     do tixAvailable <- isTixAvailable
        (if tixAvailable then
          do
            nm <- getObjectName (toGUIObject w)
            execTclScript
              ["destroy " ++ show nm ++ "ttip",
               "tixBalloon " ++ show nm ++ "ttip",
               show nm ++ "ttip bind " ++ show nm ++" -msg \"" ++
               str ++ "\""]
         else done) >> return w

  destroyTooltip w =
     do tixAvailable <- isTixAvailable
        (if tixAvailable then
          do
           nm <- getObjectName (toGUIObject w)
           execTclScript ["destroy " ++ show nm ++ "ttip"]
         else done)
