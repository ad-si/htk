-- -----------------------------------------------------------------------
--
-- $Source$
--
-- HTk - a GUI toolkit for Haskell  -  (c) Universitaet Bremen
--
-- $Revision$ from $Date$
-- Last modification by $Author$
--
-- -----------------------------------------------------------------------

module Configuration (

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
  HasAnchor(..)

) where

import GUIObject
import Computation
import Geometry
import GUIValue
import Colour
import Font
import Resources


-- -----------------------------------------------------------------------
-- has anchor
-- -----------------------------------------------------------------------

class GUIObject w => HasAnchor w where
  anchor :: Anchor -> Config w
  getAnchor :: w -> IO Anchor
  anchor a w = cset w "anchor" a
  getAnchor w = cget w "anchor"


-- -----------------------------------------------------------------------
-- coloured
-- -----------------------------------------------------------------------

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

background :: (ColourDesignator c, HasColour w) => c -> Config w
background c w = setColour w "background" (toColour c)

getBackground :: HasColour w => w -> IO Colour
getBackground w = getColour w "background" 

foreground :: (ColourDesignator c, HasColour w) => c -> Config w
foreground c w = setColour w "foreground" (toColour c)

getForeground :: HasColour w => w -> IO Colour
getForeground w = getColour w "foreground"

activeBackground :: (ColourDesignator c, HasColour w) => c -> Config w
activeBackground c w = setColour w "activebackground" (toColour c)

getActiveBackground :: HasColour w => w -> IO Colour    
getActiveBackground w = getColour w "activebackground"  
 
activeForeground :: (ColourDesignator c, HasColour w) => c -> Config w
activeForeground c w = setColour w "activeforeground" (toColour c)

getActiveForeground :: HasColour w => w -> IO Colour    
getActiveForeground w = getColour w "activeforeground"    

disabledForeground :: (ColourDesignator c, HasColour w) => c -> Config w
disabledForeground c w = setColour w "disabledforeground" (toColour c)

getDisabledForeground :: HasColour w => w -> IO Colour
getDisabledForeground w = getColour w "disabledforeground"

fg :: (ColourDesignator c, HasColour w) => c -> Config w
fg = foreground

bg :: (ColourDesignator c, HasColour w) => c -> Config w
bg = background

hasBackGroundColour :: HasColour w => w -> ConfigID -> Bool
hasBackGroundColour w "background" = True
hasBackGroundColour w _ = False

hasForeGroundColour :: HasColour w => w -> ConfigID -> Bool
hasForeGroundColour w "background" = True
hasForeGroundColour w "foreground" = True
hasForeGroundColour w _ = False


-- -----------------------------------------------------------------------
-- geometry
-- -----------------------------------------------------------------------

class GUIObject w => HasSize w where
  width       :: Distance -> Config w
  getWidth    :: w -> IO Distance
  height      :: Distance -> Config w
  getHeight   :: w -> IO Distance
  size        :: Size -> Config w
  getSize     :: w -> IO Size
  width s w    = cset w "width" s
  getWidth w   = cget w "width"
  height s w   = cset w "height" s
  getHeight w  = cget w "height"
  size (x,y) w = width x w >> height y w
  getSize w    =
    getWidth w >>= \ x -> getHeight w >>= \ y -> return (x,y)

class GUIObject w => HasPosition w where
  position    :: Position -> Config w
  getPosition :: w -> IO Position

class (HasSize w, HasPosition w) => HasGeometry w where
  geometry    :: Geometry -> Config w
  getGeometry :: w -> IO Geometry
        
class GUIObject w => HasCanvAnchor w where
  canvAnchor	:: Anchor -> Config w
  getCanvAnchor   :: w -> IO Anchor


-- -----------------------------------------------------------------------
-- has border
-- -----------------------------------------------------------------------

class GUIObject w => HasBorder w where
  borderwidth     :: Distance -> Config w
  getBorderwidth  :: w -> IO Distance
  relief          :: Relief -> Config w
  getRelief       :: w -> IO Relief
  borderwidth s w  = cset w "borderwidth" s
  getBorderwidth w = cget w "borderwidth"
  relief r w       = cset w "relief" r
  getRelief w      = cget w "relief"

-- -----------------------------------------------------------------------
-- widgets associated with a value
-- -----------------------------------------------------------------------

class (GUIObject w, GUIValue v) => HasValue w v where
  value      :: v -> Config w
  getValue   :: w -> IO v
  value v w = cset w "value" v >> return w
  getValue w = cget w "value"


-- -----------------------------------------------------------------------
-- text labelled widgets
-- -----------------------------------------------------------------------

class (GUIObject w, GUIValue v) => HasText w v where
  text      :: v -> Config w
  getText   :: w -> IO v
  text t w  = cset w "text" t
  getText w = cget w "text"

class GUIObject w => HasFont w where
  font     :: FontDesignator f => f -> Config w
  getFont  :: w -> IO Font
  font f w  = cset w "font" (toFont f)
  getFont w = cget w "font"

class GUIObject w => HasUnderline w where 
  underline      :: Int -> Config w
  getUnderline   :: w -> IO Int
  wraplength     :: Int -> Config w
  getWraplength  :: w -> IO Int
  underline i w   = cset w "underline" i
  getUnderline w  = cget w "underline"
  wraplength l w  = cset w "wraplength" l
  getWraplength w = cget w "wraplength" 

class GUIObject w => HasJustify w where
  justify     :: Justify -> Config w
  getJustify  :: w -> IO Justify
  justify js w = cset w "justify" js
  getJustify w = cget w "justify"


-- -----------------------------------------------------------------------
-- grid
-- -----------------------------------------------------------------------

class GUIObject w => HasGrid w where
  setgrid    :: Toggle -> Config w
  getGrid    :: w -> IO Toggle
  setgrid b w = cset w "setgrid" b
  getGrid w   = cget w "setgrid"


-- -----------------------------------------------------------------------
-- orientation
-- -----------------------------------------------------------------------

class GUIObject w => HasOrientation w where
  orient      :: Orientation -> Config w
  getOrient   :: w -> IO Orientation
  orient o w   = cset w "orient" o
  getOrient w  = cget w "orient"


-- -----------------------------------------------------------------------
-- file
-- -----------------------------------------------------------------------

class GUIObject w => HasFile w where
  filename :: String -> Config w
  getFileName :: w -> IO String
        

-- -----------------------------------------------------------------------
-- align
-- -----------------------------------------------------------------------
        
class GUIObject w => HasAlign w where
  align     :: Alignment -> Config w
  getAlign  :: w -> IO Alignment
  align a w  = cset w "align" a
  getAlign w = cget w "align"


-- -----------------------------------------------------------------------
-- increment (canvas region, scales)
-- -----------------------------------------------------------------------
        
class HasIncrement w a where
  increment       :: a -> Config w
  getIncrement    :: w -> IO a


-- -----------------------------------------------------------------------
--  enabling and disabling of widgets
-- -----------------------------------------------------------------------

class GUIObject w => HasEnable w where
  state      :: State -> Config w
  getState   :: w -> IO State
  disable    :: Config w
  enable     :: Config w
  isEnabled  :: w -> IO Bool
  state s w   = cset w "state" s
  getState w  = cget w "state"
  disable     = state Disabled
  enable      = state Normal
  isEnabled w = do {st <- getState w; return (st /= Disabled)}
