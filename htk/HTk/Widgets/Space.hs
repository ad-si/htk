-- | HTk\'s /space/ widget.
-- A simple spacer for special packing purposes.
module HTk.Widgets.Space (

  Space,
  newSpace,

) where

import HTk.Containers.Frame
import HTk.Kernel.Configuration
import HTk.Kernel.Resources
import HTk.Kernel.GUIObject
import Events.Destructible
import Events.Synchronized
import HTk.Kernel.Geometry
import Util.Computation
import HTk.Kernel.BaseClasses(Widget)
import HTk.Kernel.Packer


-- -----------------------------------------------------------------------
-- datatype
-- -----------------------------------------------------------------------

-- | The @Space@ datatype.
data Space = Space Distance Frame


-- -----------------------------------------------------------------------
-- constructor
-- -----------------------------------------------------------------------

-- | Constructs a new space widget and returns a handler.
newSpace :: Container par => par
   -- ^ the parent widget, which has to be a container widget
   -- (an instance of @class Container@).
   -> Distance
   -- ^ the horizontal or vertical distance (depending on the
   -- space widget\'s orientation).
   -> [Config Space]
   -- ^ the list of configuration options for this
   -- space widget.
   -> IO Space
   -- ^ A space widget.
newSpace par dist cnf =
  do
    f <- newFrame par []
    configure (Space dist f) (defaults : cnf)
  where defaults = orient Vertical


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

-- | Internal.
instance Eq Space where
  (Space _ f1) == (Space _ f2) = f1 == f2

-- | Internal.
instance GUIObject Space where
  toGUIObject (Space d f) = toGUIObject f
  cname _ = "Space"

-- | A space widget can be destroyed.
instance Destroyable Space where
  -- Destroys a space widget.
  destroy   = destroy . toGUIObject

-- | A radiobutton widget has standard widget properties
-- (concerning focus, cursor).
instance Widget Space

-- | You can synchronize on a space widget.
instance Synchronized Space where
  -- Synchronizes on a space widget.
  synchronize w = synchronize (toGUIObject w)

-- | A space widget has a configureable background colour.
instance HasColour Space where
  legalColourID = hasBackGroundColour

-- | The space widgets orientation can either be @vertical@ or
-- @Horizontal@.
instance HasOrientation Space where
  -- Sets the orientation of the space widget.
  orient or s @ (Space d f) =
    configure f (case or of Horizontal -> [{-fill Vertical,-} width d,
                                           height 0]
                            Vertical -> [{-fill Horizontal,-} height d,
                                         width 0]) >>
    return s
