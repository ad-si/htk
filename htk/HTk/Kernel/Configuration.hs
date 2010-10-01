{-# LANGUAGE MultiParamTypeClasses #-}

-- | Basic types and classes concerning widget configuration.
module HTk.Kernel.Configuration (

  HasColour(..),

  background,
  getBackground,

  foreground,
  getForeground,

  activeBackground,
  getActiveBackground,

  activeForeground,
  getActiveForeground,

  disabledForeground,
  getDisabledForeground,

  fg,
  bg,

  hasBackGroundColour,
  hasForeGroundColour,

  HasSize(..),
  HasPosition(..),
  HasGeometry(..),
  HasCanvAnchor(..),
  HasBorder(..),
  HasValue(..),
  HasText(..),
  HasFont(..),
  HasUnderline(..),
  HasJustify(..),
  HasGrid(..),
  HasOrientation(..),
  HasFile(..),
  HasAlign(..),
  HasIncrement(..),
  HasEnable(..),
  HasAnchor(..),
  HasBBox(..)

) where

import HTk.Kernel.GUIObject
import Util.Computation
import HTk.Kernel.Geometry
import HTk.Kernel.GUIValue
import HTk.Kernel.Colour
import HTk.Kernel.Font
import HTk.Kernel.Resources


-- -----------------------------------------------------------------------
-- BBox
-- -----------------------------------------------------------------------

-- | Objects or sets of objects with a bounding box (e.g. canvas tags)
-- instantiate the @class HasBBox@.
class GUIObject w => HasBBox w i where
  -- Returns the bounding box of the given object.
  bbox :: w -> i -> IO (Maybe (Distance,Distance,Distance,Distance))


-- -----------------------------------------------------------------------
-- has anchor
-- -----------------------------------------------------------------------

-- | Objects that have an anchor position instantiate the
-- @class HasAnchor@.
class GUIObject w => HasAnchor w where
  -- Sets the anchor position.
  anchor :: Anchor -> Config w
  -- Gets the anchor position.
  getAnchor :: w -> IO Anchor
  anchor a w = cset w "anchor" a
  getAnchor w = cget w "anchor"


-- -----------------------------------------------------------------------
-- coloured
-- -----------------------------------------------------------------------

-- | Coloured objects instantiate the @class HasColour@.
class GUIObject w => HasColour w where
  legalColourID :: w -> ConfigID -> Bool
  setColour :: w -> ConfigID -> Colour -> IO w
  getColour :: w -> ConfigID -> IO Colour
  legalColourID _ "background" = True
  legalColourID _ _ = False
  setColour w cid col =
    if legalColourID w cid then cset w cid col else return w
  getColour w cid =
    if legalColourID w cid then cget w cid else return cdefault

-- | Sets the background colour.
background :: (ColourDesignator c, HasColour w) => c -> Config w
background c w = setColour w "background" (toColour c)

-- | Gets the background colour.
getBackground :: HasColour w => w -> IO Colour
getBackground w = getColour w "background"

-- | Sets the foreground colour.
foreground :: (ColourDesignator c, HasColour w) => c -> Config w
foreground c w = setColour w "foreground" (toColour c)

-- | Gets the foreground colour.
getForeground :: HasColour w => w -> IO Colour
getForeground w = getColour w "foreground"

-- | Sets the active background colour.
activeBackground :: (ColourDesignator c, HasColour w) => c -> Config w
activeBackground c w = setColour w "activebackground" (toColour c)

-- | Gets the active background colour.
getActiveBackground :: HasColour w => w -> IO Colour
getActiveBackground w = getColour w "activebackground"

-- | Sets the active foreground colour.
activeForeground :: (ColourDesignator c, HasColour w) => c -> Config w
activeForeground c w = setColour w "activeforeground" (toColour c)

-- | Gets the active foreground colour.
getActiveForeground :: HasColour w => w -> IO Colour
getActiveForeground w = getColour w "activeforeground"

-- | Sets the disabled foreground colour.
disabledForeground :: (ColourDesignator c, HasColour w) => c -> Config w
disabledForeground c w = setColour w "disabledforeground" (toColour c)

-- | Gets the disabled foreground colour.
getDisabledForeground :: HasColour w => w -> IO Colour
getDisabledForeground w = getColour w "disabledforeground"

-- | Sets the foreground colour.
fg :: (ColourDesignator c, HasColour w) => c -> Config w
fg = foreground

-- | Sets the background colour.
bg :: (ColourDesignator c, HasColour w) => c -> Config w
bg = background

-- | Internal.
hasBackGroundColour :: HasColour w => w -> ConfigID -> Bool
hasBackGroundColour w "background" = True
hasBackGroundColour w _ = False

-- | Internal.
hasForeGroundColour :: HasColour w => w -> ConfigID -> Bool
hasForeGroundColour w "background" = True
hasForeGroundColour w "foreground" = True
hasForeGroundColour w _ = False


-- -----------------------------------------------------------------------
-- geometry
-- -----------------------------------------------------------------------

-- | Objects with a configureable size instantiate the
-- @class HasSize@.
class GUIObject w => HasSize w where
  -- Sets the object\'s width.
  width       :: Distance -> Config w
  -- Gets the object\'s width.
  getWidth    :: w -> IO Distance
  -- Sets the object\'s height.
  height      :: Distance -> Config w
  -- Gets the object\'s height.
  getHeight   :: w -> IO Distance
  -- Sets the object\'s width and height.
  size        :: Size -> Config w
  -- Gets the object\'s width and height.
  getSize     :: w -> IO Size
  width s w    = cset w "width" s
  getWidth w   = cget w "width"
  height s w   = cset w "height" s
  getHeight w  = cget w "height"
  size (x,y) w = width x w >> height y w
  getSize w    =
    getWidth w >>= \ x -> getHeight w >>= \ y -> return (x,y)

-- | Objects with a configureable positon (e.g. canvas items) instantiate
-- the @class HasPosition@.
class GUIObject w => HasPosition w where
  -- Gets the object\'s position.
  position    :: Position -> Config w
  -- Sets the object\'s position.
  getPosition :: w -> IO Position

-- | Objects with a configureable size and position instantiate the
-- @class HasGeometry@.
class (HasSize w, HasPosition w) => HasGeometry w where
  -- Sets the object\'s geometry.
  geometry    :: Geometry -> Config w
  -- Gets the object\'s geometry.
  getGeometry :: w -> IO Geometry

-- | Canvasitems with an anchor position on the canvas instantiate the
-- @class HasCanvAnchor@.
class GUIObject w => HasCanvAnchor w where
  -- Sets the anchor position on the canvas.
  canvAnchor    :: Anchor -> Config w
  -- Gets the anchor position on the canvas.
  getCanvAnchor   :: w -> IO Anchor


-- -----------------------------------------------------------------------
-- has border
-- -----------------------------------------------------------------------

-- | Objects with a configureable border instantiate the
-- @class HasBorder@.
class GUIObject w => HasBorder w where
  -- Sets the width of the object\'s border.
  borderwidth     :: Distance -> Config w
  -- Gets the width of the object\'s border.
  getBorderwidth  :: w -> IO Distance
  -- Sets the object\'s relief.
  relief          :: Relief -> Config w
  -- Gets the object\'s relief.
  getRelief       :: w -> IO Relief
  borderwidth s w  = cset w "borderwidth" s
  getBorderwidth w = cget w "borderwidth"
  relief r w       = cset w "relief" r
  getRelief w      = cget w "relief"


-- -----------------------------------------------------------------------
-- objects associated with a value
-- -----------------------------------------------------------------------

-- | Objects that have a value instantiate the
-- @class HasValue@.
class (GUIObject w, GUIValue v) => HasValue w v where
  -- Sets the object\'s value.
  value      :: v -> Config w
  -- Gets the object\'s value.
  getValue   :: w -> IO v
  value v w = cset w "value" v >> return w
  getValue w = cget w "value"


-- -----------------------------------------------------------------------
-- text labelled widgets
-- -----------------------------------------------------------------------

-- | Objects containing text instantiate the class
-- @HasText@.
class (GUIObject w, GUIValue v) => HasText w v where
  -- Sets the object\'s text.
  text      :: v -> Config w
  -- Gets the object\'s text.
  getText   :: w -> IO v
  text t w  = cset w "text" t
  getText w = cget w "text"

-- | Objects with a configureable font instantiate the
-- @class HasFont@.
class GUIObject w => HasFont w where
  -- Sets the object\'s font.
  font     :: FontDesignator f => f -> Config w
  -- Gets the object\'s font.
  getFont  :: w -> IO Font
  font f w  = cset w "font" (toFont f)
  getFont w = cget w "font"

-- | Objects that have a text underline configure option instantiate th
-- @class HasUnderline@.
class GUIObject w => HasUnderline w where
  -- Sets the index position of the text character to underline.
  underline      :: Int -> Config w
  -- Gets the index position of the text character to underline.
  getUnderline   :: w -> IO Int
  -- Sets the maximum line length for text in screen units.
  wraplength     :: Int -> Config w
  -- Gets the maximum line length for text in screen units.
  getWraplength  :: w -> IO Int
  underline i w   = cset w "underline" i
  getUnderline w  = cget w "underline"
  wraplength l w  = cset w "wraplength" l
  getWraplength w = cget w "wraplength"

-- | Objects that have a configureable text justification instantiate the
-- @class HasJustify@.
class GUIObject w => HasJustify w where
  -- Sets the text justification.
  justify     :: Justify -> Config w
  -- Gets the set text justification.
  getJustify  :: w -> IO Justify
  justify js w = cset w "justify" js
  getJustify w = cget w "justify"


-- -----------------------------------------------------------------------
-- grid
-- -----------------------------------------------------------------------

-- | Objects that support geometry gridding instantiate the
-- @class HasGrid@.
class GUIObject w => HasGrid w where
  -- Enables geometry gridding.
  setgrid    :: Toggle -> Config w
  -- Gets the current setting.
  getGrid    :: w -> IO Toggle
  setgrid b w = cset w "setgrid" b
  getGrid w   = cget w "setgrid"


-- -----------------------------------------------------------------------
-- orientation
-- -----------------------------------------------------------------------

-- | Oriented objects instantiate the @class HasOrientation@.
class GUIObject w => HasOrientation w where
  -- Sets the object\'s orientation.
  orient      :: Orientation -> Config w
  -- Gets the object\'s orientation.
  getOrient   :: w -> IO Orientation
  orient o w   = cset w "orient" o
  getOrient w  = cget w "orient"


-- -----------------------------------------------------------------------
-- file
-- -----------------------------------------------------------------------

-- | Objects associated with a file instantiate the
-- @class HasFile@.
class GUIObject w => HasFile w where
  -- Sets the name of the associated file.
  filename :: String -> Config w
  -- Gets the name of the associated file.
  getFileName :: w -> IO String


-- -----------------------------------------------------------------------
-- align
-- -----------------------------------------------------------------------

-- | Objects with a configureable alignment instantiate the
-- @class HasAlign@.
class GUIObject w => HasAlign w where
  align     :: Alignment -> Config w
  getAlign  :: w -> IO Alignment
  align a w  = cset w "align" a
  getAlign w = cget w "align"


-- -----------------------------------------------------------------------
-- increment (canvas region, scales)
-- -----------------------------------------------------------------------

-- | Incrementable objects (e.g. scale wigdgets) instantiate the
-- @class HasIncrement@.
class HasIncrement w a where
  -- Increments the object.
  increment       :: a -> Config w
  -- Gets object\'s incrementation.
  getIncrement    :: w -> IO a


-- -----------------------------------------------------------------------
--  enabling and disabling of widgets
-- -----------------------------------------------------------------------

-- | Stateful objects that can be enabled or disabled instantiate the
-- @class HasEnable@.
class GUIObject w => HasEnable w where
  -- Sets the objects state.
  state      :: State -> Config w
  -- Gets the objects state.
  getState   :: w -> IO State
  -- Disables the object.
  disable    :: Config w
  -- Enables the object.
  enable     :: Config w
  -- @True@ if the object is enabled.
  isEnabled  :: w -> IO Bool
  state s w   = cset w "state" s
  getState w  = cget w "state"
  disable     = state Disabled
  enable      = state Normal
  isEnabled w = do {st <- getState w; return (st /= Disabled)}
