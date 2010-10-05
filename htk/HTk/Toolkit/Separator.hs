-- | Separators for widgets. It is just
-- a frame with a given relief and borderwidth etc.
module HTk.Toolkit.Separator (
        Separator,
        newSeparator,
        newHSeparator,
        newVSeparator

        ) where

import Util.Computation

import HTk.Kernel.Core
import HTk.Toplevel.HTk

-- --------------------------------------------------------------------------
-- Separator
-- --------------------------------------------------------------------------

-- | The @Separator@ datatype.
data Separator = Separator Frame deriving Eq


-- | Constructs a new separator widget and returns it as a value.
newSeparator :: (Container par) => par
   -- ^ the parent widget, which has to be a container widget
   -- (an instance of @class Container@).
   -> [Config Separator]
   -- ^ the list of configuration options for this separator.
   -> IO Separator
   -- ^ a separator widget.
newSeparator par conf =
 do
  w <- newFrame par []
  configure (Separator w) conf


-- | Constructs a new horizontal separator widget and returns it as a value. (no packing needed)
newHSeparator :: (Container par) => par
   -- ^ the parent widget, which has to be a container widget
   -- (an instance of @class Container@).
   -> IO Separator
   -- ^ a separator widget.
newHSeparator par =
 do
  w <- newFrame par [relief Sunken, height 2, borderwidth 1]
  pack w [Expand Off, Fill X]
  configure (Separator w) []

-- | Constructs a new vertical separator widget and returns it as a value. (no packing needed)
newVSeparator :: (Container par) => par
   -- ^ the parent widget, which has to be a container widget
   -- (an instance of @class Container@).
   -> IO Separator
   -- ^ a separator widget.
newVSeparator par =
 do
  w <- newFrame par [relief Sunken, width 2, borderwidth 1]
  pack w [Expand Off, Fill Y]
  configure (Separator w) []

-- --------------------------------------------------------------------------
-- Instances
-- --------------------------------------------------------------------------
-- | Internal.
instance GUIObject Separator where
        toGUIObject (Separator w) = toGUIObject w
        cname w = "Separator"

-- | A separator can be destroyed.
instance Destroyable Separator where
        -- Destroys a separator widget.
        destroy = destroy . toGUIObject

-- | A separator widget has a configureable border.
instance HasBorder Separator

-- | You can specify the size of a separator widget.
instance HasSize Separator

-- | A spearator widget has an orientation (Horizontal or Vertical)
instance HasOrientation Separator where
        orient Horizontal s = do {
                configure s [height 2];
                return s
                }
        orient Vertical s = do {
                configure s [width 2];
                return s
                }


