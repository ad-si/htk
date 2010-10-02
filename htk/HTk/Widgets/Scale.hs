{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | HTk\'s /scale/ widget.
-- A simple slider in a through representing a range of numeric values.
module HTk.Widgets.Scale (

  ScaleValue(..),

  Scale,
  newScale,

  digits,
  getDigits,

  interval,
  getInterval,
  intervalTo,
  getIntervalTo,
  intervalFrom,
  getIntervalFrom,

  bigIncrement,
  getBigIncrement,

  showValue,
  getShowValue

) where

import Util.Computation

import Events.Synchronized
import Events.Destructible
import Reactor.ReferenceVariables

import HTk.Kernel.Core
import HTk.Kernel.BaseClasses(Widget)
import HTk.Kernel.Configuration
import HTk.Kernel.Resources
import HTk.Components.Slider
import HTk.Kernel.Packer
import HTk.Kernel.Tooltip


-- -----------------------------------------------------------------------
-- Scale type
-- -----------------------------------------------------------------------

-- | The @Scale@ datatype.
data Scale a = Scale GUIOBJECT (Ref Double)
-- the position should really be part of the kind attribute of the GUIObject


-- -----------------------------------------------------------------------
-- classes
-- -----------------------------------------------------------------------

-- | Values associated with a scale instansiate the
-- @class ScaleValue@.
class (Num a, GUIValue a) => ScaleValue a where
  toDouble :: a -> Double
  fromDouble :: Double -> a

-- | A double value is a scale value.
instance ScaleValue Double where
  toDouble = id
  fromDouble = id


-- -----------------------------------------------------------------------
-- Scale creation
-- -----------------------------------------------------------------------

-- | Constructs a new scale widget and returns a handler.
newScale :: (GUIValue a, ScaleValue a, Container par) =>
   par
   -- ^ the parent widget, which has to be a container widget.
   -> [Config (Scale a)]
   -- ^ the list of configuration options for this scale
   -- widget.
   -> IO (Scale a)
   -- ^ A scale widget.
newScale par cnf =
  do
    wid <- createGUIObject (toGUIObject par) SCALE scaleMethods
    ref <- newRef 0
    sc <- return (Scale wid ref)
    configure sc (interval (0,100) : cnf)


-- -----------------------------------------------------------------------
-- Configuration options: Instantiations
-- -----------------------------------------------------------------------

-- | Internal.
instance Eq (Scale a) where
  w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

-- | Internal.
instance GUIObject (Scale a) where
  toGUIObject (Scale w _) = w
  cname _ = "Scale"

-- | A scale widget can be destroyed.
instance Destroyable (Scale a) where
  destroy = destroy . toGUIObject

-- | A scale widget has standard widget properties
-- (concerning focus, cursor).
instance Widget (Scale a)

-- | You can synchronize on a scale widget.
instance Synchronized (Scale a) where
  -- Synchronizes on a scale widget.
  synchronize = synchronize . toGUIObject

-- | A scale widget has a configureable border.
instance HasBorder (Scale a)

-- | A scale widget has a configureable foreground, background and
-- activebackground colour.
instance HasColour (Scale a) where
  legalColourID w "background" = True
  legalColourID w "foreground" = True
  legalColourID w "activebackground" = True
  legalColourID w _ = False

-- | A scale widget is a stateful widget, it can be enabled or disabled.
instance HasEnable (Scale a)

-- | A scale widget has a configureable font.
instance HasFont (Scale a)

-- | A scale widget has a configureable incrementation interval.
instance ScaleValue a => HasIncrement (Scale a) a where
  -- Sets the scale widget\'s incrementation interval.
  increment d w  = cset w "tickinterval" (toDouble d)
  -- Gets the scale widget\'s incrementation interval.
  getIncrement w = cget w "tickinterval" >>= return . fromDouble

-- | A scale widget\'s orientation can either be vertical or horizontal.
instance HasOrientation (Scale a)

-- | A scale widget has a configureable size.
instance HasSize (Scale a) where
  -- Sets the scale widget\'s length.
  height d w  = cset w "length" d
  -- Gets the scale widget\'s length.
  getHeight w = cget w "length"

-- | A scale widget has a configureable slider.
instance HasSlider (Scale a)

-- | A scale widget has a text label.
instance GUIValue v => HasText (Scale a) v where
  -- Sets the text of the scale widget\'s label.
  text s w  = cset w  "label" s
  -- Gets the text of the scale widget\'s label.
  getText w = cget w "label"

-- | A scale widget can have a tooltip.
instance HasTooltip (Scale a)


-- -----------------------------------------------------------------------
--  Scale specific config options
-- -----------------------------------------------------------------------

-- | Sets the number of significant values in the scale widget.
digits :: Int -> Config (Scale a)
digits d w = cset w "digits" d

-- | Gets the number of significant values in the scale widget.
getDigits :: Scale a -> IO Int
getDigits w = cget w "digits"

-- | Sets the maximum value of the scale widget.
intervalTo :: ScaleValue a => a -> Config (Scale a)
intervalTo v w = cset w "to" (toDouble v)

-- | Gets the maximum value of the scale widget.
getIntervalTo :: ScaleValue a => Scale a -> IO a
getIntervalTo w = cget w "to" >>= return . fromDouble

-- | Sets the minimum value of the scale widget.
intervalFrom :: ScaleValue a => a -> Config (Scale a)
intervalFrom v w = cset w "from" (toDouble v)

-- | Gets the minimum value of the scale widget.
getIntervalFrom :: ScaleValue a => Scale a -> IO a
getIntervalFrom w = cget w "from" >>= return . fromDouble

-- | Sets the scale widgets maximum and minumum value.
interval :: ScaleValue a => (a, a) -> Config (Scale a)
interval (b,e) w =
        synchronize w (do{
                cset w "to" (toDouble b);
                cset w "from" (toDouble e)
                })

-- | Gets the scale widgets maximum and minumum value.
getInterval :: ScaleValue a => Scale a -> IO (a,a)
getInterval w =
        synchronize w (do {
                cget w "to" >>= \b ->
                cget w "from" >>= \e ->
                return (fromDouble b,fromDouble e)
                })


-- -----------------------------------------------------------------------
-- Slider specific config options
-- -----------------------------------------------------------------------

-- | A scale\'s slider has a configureable resulution.
instance ScaleValue a => HasIncrement (Slider (Scale a)) a where
        -- Sets the slider\'s resolution.
        increment d w   = cset w "resolution" (toDouble d)
        -- Gets the slider\'s resolution.
        getIncrement w  = cget w "resolution" >>= return . fromDouble

-- | A scale\'s slider has a configureable size.
instance HasSize (Slider (Scale a)) where
        -- Sets the sliders width.
        width d w       = cset w "width" d
        -- Gets the sliders width.
        getWidth w      = cget w "width"
        -- Sets the sliders height.
        height d w      = cset w "sliderlength" d
        -- Gets the sliders height.
        getHeight w     = cget w "sliderlength"

-- | Sets the coarse grain slider adjustment value.
bigIncrement ::  ScaleValue a => a -> Config (Slider (Scale a))
bigIncrement d w = cset w "bigincrement" (toDouble d)

-- | Gets the coarse grain slider adjustment value.
getBigIncrement :: ScaleValue a => (Slider (Scale a)) -> IO a
getBigIncrement w = cget w "bigincrement" >>= return . fromDouble

-- | Shows the sliders value when set.
showValue :: Toggle -> Config (Slider (Scale a))
showValue d w = cset w "showvalue" d

-- | Gets the current showvalue setting.
getShowValue :: (Slider (Scale a)) -> IO Toggle
getShowValue w = cget w "showvalue"


-- -----------------------------------------------------------------------
-- Scale methods
-- -----------------------------------------------------------------------

scaleMethods :: Methods
scaleMethods = defMethods


-- -----------------------------------------------------------------------
-- Tk intrinsics
-- -----------------------------------------------------------------------

tkScaleCmd :: ObjectID -> TclCmd
tkScaleCmd (ObjectID i) = "Scaled " ++ show i
{-# INLINE tkScaleCmd #-}

tkPackScale  _ _ name opts oid binds =
  ("pack " ++ (show name) ++ " " ++ (showConfigs opts))
