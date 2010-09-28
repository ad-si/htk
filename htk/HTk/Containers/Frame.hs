-- | HTk\'s /frame/ widget.
-- A frame is a simple container for widgets.
module HTk.Containers.Frame (

  Frame,
  newFrame

) where

import HTk.Kernel.Core
import HTk.Kernel.BaseClasses(Widget)
import HTk.Kernel.Configuration
import Util.Computation
import Events.Synchronized
import Events.Destructible
import HTk.Kernel.Packer


-- -----------------------------------------------------------------------
-- type Frame
-- -----------------------------------------------------------------------

-- | The @Frame@ datatype.
data Frame = Frame GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

-- | Constructs a new frame widget and returns a handler.
newFrame :: Container par => par
   -- ^ the parent widget, which has to be a container widget
   -- (an instance of @class Container@).
   -> [Config Frame]
   -- ^ the list of configuration options for this frame.
   -> IO Frame
   -- ^ A frame widget.
newFrame par confs =
  do
    w <- createWidget (toGUIObject par) FRAME
    configure (Frame w) confs


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIObject Frame where
  toGUIObject (Frame w) = w
  cname _ = "Frame"

-- | A frame widget can be destroyed.
instance Destroyable Frame where
  --  Destroys a frame widget.
  destroy   = destroy . toGUIObject

-- | A frame widget has standard widget properties
-- (concerning focus, cursor).
instance Widget Frame

-- | A frame widget is a container for widgets. You can pack widgets to
-- a frame widget via pack or grid command in the
-- @module HTk.Kernel.Packer@.
instance Container Frame

-- | A frame widget has a configureable border.
instance HasBorder Frame

-- | A frame widget has a background colour.
instance HasColour Frame where
  legalColourID = hasBackGroundColour

-- | You can specify the size of a frame.
instance HasSize Frame

-- | You can synchronize on a frame object.
instance Synchronized Frame where
  --  Synchronizes on a frame object.
  synchronize = synchronize . toGUIObject
