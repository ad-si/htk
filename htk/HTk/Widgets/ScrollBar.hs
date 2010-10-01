-- | HTk\'s /scrollbar/ widget.
--
-- A scroll bar is a widget which controls scrolling.
--
module HTk.Widgets.ScrollBar (

  HasScroller(..),
  ScrollBar,
  newScrollBar,

  ScrollUnit(..),

  Slider(..),
  HasSlider(..),

  ScrollBarElem(..),
  activateScrollBarElem,
  getActivatedElem,

  Fraction,
  fraction,
  identify,
  setView

) where

import HTk.Kernel.Core
import HTk.Kernel.BaseClasses(Widget)
import HTk.Kernel.Configuration
import HTk.Kernel.Geometry
import HTk.Kernel.Resources
import Events.Destructible
import HTk.Components.Slider
import Data.Char
import Util.Computation
import HTk.Kernel.Packer
import HTk.Kernel.Tooltip
import HTk.Kernel.GUIValue

-- -----------------------------------------------------------------------
-- fraction type
-- -----------------------------------------------------------------------

-- | Fractions are floating point values between 0 and 1 representing
-- relative positions within the scrolled range.
type Fraction = Double

data FractionPair = FractionPair Fraction Fraction

-- | Internal.
instance GUIValue FractionPair where
  cdefault = FractionPair 0.0 0.0

-- | Internal.
instance Show FractionPair where
  showsPrec d (FractionPair f1 f2) r = show f1 ++ " " ++ show f2 ++ r

-- | Internal.
instance Read FractionPair where
   -- Internal.
   readsPrec p b =
     case readsPrec p b of
       [(x,xs)] -> case readsPrec p xs of
                      [(y,ys)] -> [(FractionPair x y, ys)]
                      _        -> []
       _        -> []


-- -----------------------------------------------------------------------
-- classes
-- -----------------------------------------------------------------------

-- | Scrollable widgets instantiate @class HasScroller@.
class Widget w => HasScroller w where
  -- @True@ for widgets that are scrollable in the given
  -- orientation.
  isWfOrientation :: w -> Orientation -> Bool
  -- Associates a scrollbar with a scrollable widget.
  scrollbar       :: Orientation -> ScrollBar -> Config w
  -- Positions the scrolled widget so the give @Fraction@ is
  -- off-screen to the left.
  moveto          :: Orientation -> w -> Fraction -> IO ()
  -- Scrolls the associated widget by n pages or units (depending on the
  -- given @ScrollUnit@).
  scroll          :: Orientation -> w -> Int -> ScrollUnit -> IO ()
  -- Returns two fractions between 0 and 1 that describe the amount of
  -- the widget off-screen to the left and the amount of the widget visible.
  view            :: Orientation -> w -> IO (Fraction, Fraction)


  isWfOrientation _ _ = True

  scrollbar Horizontal sc w | isWfOrientation w Horizontal =
    do
      cset sc "command" (TkCommand (varname w ++ " xview"))
      execTclScript [tkDeclScrollVar w]
      cset w "xscrollcommand" (TkCommand (varname sc ++ " set"))
      execTclScript [tkDeclScrollVar sc]
      return w
  scrollbar Vertical sc w | isWfOrientation w Vertical =
    do
      cset sc "command" (TkCommand (varname w ++ " yview"))
      execTclScript [tkDeclScrollVar w]
      cset w "yscrollcommand" (TkCommand (varname sc ++ " set"))
      execTclScript [tkDeclScrollVar sc]
      return w
  scrollbar _ _ w = return w

  moveto ax w f | isWfOrientation w ax =
    execMethod w (\nm -> [tkMoveTo ax nm f])
  moveto _ _ _ = done

  scroll ax w num what | isWfOrientation w ax =
    execMethod w (\nm -> [tkScroll ax nm num what])
  scroll ax w num what = done

  view ax w =
    do
      FractionPair os vis <- (evalMethod w (tkView ax) :: IO FractionPair)
      return (os,vis)


-- -----------------------------------------------------------------------
-- datatype
-- -----------------------------------------------------------------------

-- | The @ScrollBar@ datatype.
newtype ScrollBar = ScrollBar GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- constructor
-- -----------------------------------------------------------------------

-- | Constructs a new scrollbar widget and returns a handler.
newScrollBar :: Container par => par
   -- ^ the parent widget, which has to be a container widget
   -- (an instance of @class Container@).
   -> [Config ScrollBar]
   -- ^ the list of configuration options for this scrollbar.
   -> IO ScrollBar
   -- ^ A scrollbar widget.
newScrollBar par cnf =
  do
    w <- createGUIObject (toGUIObject par) SCROLLBAR scrollbarMethods
    configure (ScrollBar w) cnf


-- -----------------------------------------------------------------------
-- ScrollBar configuration options
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIObject ScrollBar where
  toGUIObject (ScrollBar w) = w
  cname _ = "ScrollBar"

-- | A scrollbar widget can be destroyed.
instance Destroyable ScrollBar where
  -- Destroys a scrollbar widget.
  destroy = destroy . toGUIObject

-- | A scrollbar widget has standard widget properties
-- (concerning focus, cursor).
instance Widget ScrollBar

-- | A scrollbar widget has a configureable border.
instance HasBorder ScrollBar

-- | A scrollbar widget has a background and activebackground
-- (regarding slider) colour.
instance HasColour ScrollBar where
  legalColourID w "bg" = True
  legalColourID w "activebackground" = True -- regards slider actually
  legalColourID w _ = False

-- | A scrollbar widget is a stateful widget, it can be enabled or
-- disabled.
instance HasEnable ScrollBar

-- | You can specify the width of a scrollbar.
instance HasSize ScrollBar where
  -- Dummy.
  height _ w = return w
  -- Dummy.
  getHeight w = return cdefault

-- | The scrollbar has a configureable slider component.
instance HasSlider ScrollBar

-- | The scrollbars orientation can be @Horizontal@ or
-- @Vertical@.
instance HasOrientation ScrollBar

-- | A scrollbar can have a tooltip.
instance HasTooltip ScrollBar


-- -----------------------------------------------------------------------
-- ScrollBar commands
-- -----------------------------------------------------------------------

-- | Sets the active element (which can be arrow1, arrow2 or slider).
activateScrollBarElem :: ScrollBar
   -- ^ the concerned scrollbar.
   -> ScrollBarElem
   -- ^ the element to activate.
   -> IO ()
   -- ^ None.
activateScrollBarElem sc elem =
  execMethod sc (\nm -> [tkActivate nm elem])

-- | Gets the active element (arrow1, arrow2 or slider).
getActivatedElem :: ScrollBar
   -- ^ the concerned scrollbar.
   -> IO (Maybe ScrollBarElem)
   -- ^ @Just [elem]@ if an element is active,
   -- otherwise @Nothing@.
getActivatedElem sc =
  do
    e <- evalMethod sc (\nm -> [tkGetActivate nm])
    case dropWhile isSpace e of
      "" -> return Nothing
      x -> return (Just (read x))

-- | Returns a fraction between 0 and 1 indicating the relative location
-- of the given position in the through.
fraction :: ScrollBar
   -- ^ the concerned scrollbar.
   -> Position
   -- ^ the conderned position.
   -> IO Fraction
   -- ^ The fraction indicating the relative location in the
   -- through.
fraction sc pos@(x, y) = evalMethod sc (\nm -> [tkFraction nm x y])

-- | Returns the @ScrollBarElem@ to indicate what is under
-- the given position.
identify :: ScrollBar
   -- ^ the concerned scrollbar.
   -> Position
   -- ^ the concerned position.
   -> IO (Maybe ScrollBarElem)
   -- ^ @Just [elem]@ if @[elem]@ is
   -- under the given position, otherwise
   -- @Nothing@.
identify sc pos@(x, y) =
  do
    e <- evalMethod sc (\nm -> [tkIdentify nm x y])
    case dropWhile (isSpace) e of
      "" -> return Nothing
      x -> return (Just (read x))

-- | Sets the scrollbar parameters.
setView :: ScrollBar
   -- ^ the concerned scrollbar.
   -> Fraction
   -- ^ fraction between 0 and 1 representing the relative
   -- position of the top left of the display.
   -> Fraction
   -- ^ fraction between 0 and 1 representing the relative
   -- position of the bottom right of the display.
   -> IO ()
   -- ^ None.
setView sc first last = execMethod sc (\nm -> [tkSet nm first last])


-- -----------------------------------------------------------------------
-- ScrollBar elem
-- -----------------------------------------------------------------------

-- | The @ScrollBarElem@ datatype - representing the elements
-- of the scrollbar.
data ScrollBarElem =
    Arrow1
  | Trough1
  | ScrollBarSlider
  | Trough2
  | Arrow2
  deriving (Eq,Ord,Enum)

-- | Internal.
instance GUIValue ScrollBarElem where
  cdefault = ScrollBarSlider

-- | Internal.
instance Read ScrollBarElem where
  readsPrec p b =
    case dropWhile (isSpace) b of
       'a':'r':'r':'o':'w':'1':xs -> [(Arrow1,xs)]
       't':'r':'o':'u':'g':'h':'1':xs -> [(Trough1,xs)]
       's':'l':'i':'d':'e':'r':xs -> [(ScrollBarSlider,xs)]
       't':'r':'o':'u':'g':'h':'2':xs -> [(Trough2,xs)]
       'a':'r':'r':'o':'w':'2':xs -> [(Arrow2,xs)]
       _ -> []

-- | Internal.
instance Show ScrollBarElem where
  showsPrec d p r =
     (case p of
         Arrow1 -> "arrow1"
         Trough1 -> "trough1"
         ScrollBarSlider -> "slider"
         Trough2 -> "trough2"
         Arrow2 -> "arrow2"
       ) ++ r


-- -----------------------------------------------------------------------
-- scroll unit
-- -----------------------------------------------------------------------

-- | The @ScrollUnit@ datatype - units for scrolling operations.
data ScrollUnit = Units | Pages

-- | Internal.
instance GUIValue ScrollUnit where
  cdefault = Units

-- | Internal.
instance Read ScrollUnit where
   -- Internal.
   readsPrec p b =
     case dropWhile (isSpace) b of
        'u':'n':'i':'t':'s':xs -> [(Units,xs)]
        'p':'a':'g':'e':'s':xs -> [(Pages,xs)]
        _ -> []

-- | Internal.
instance Show ScrollUnit where
   -- Internal.
   showsPrec d p r =
      (case p of
          Units -> "units"
          Pages -> "pages"
        ) ++ r


-- -----------------------------------------------------------------------
-- Scrollbar methods
-- -----------------------------------------------------------------------

scrollbarMethods = defMethods { cleanupCmd = tkCleanupScrollBar,
                                createCmd = tkCreateScrollBar }


-- -----------------------------------------------------------------------
-- Tk commands
-- -----------------------------------------------------------------------

tkCreateScrollBar :: ObjectName -> ObjectKind -> ObjectName -> ObjectID ->
                     [ConfigOption] -> TclScript
tkCreateScrollBar pnm kind name oid confs =
  tkDeclVar ("sv" ++ show oid) (show name) ++
  (createCmd defMethods) pnm kind name oid confs
{-# INLINE tkCreateScrollBar #-}

tkCleanupScrollBar :: ObjectID -> ObjectName -> TclScript
tkCleanupScrollBar oid _ = tkUndeclVar ("sv" ++ show oid)
{-# INLINE tkCleanupScrollBar #-}

varname :: GUIObject w => w -> String
varname w = (tkDeclScrollVar w) ++ "; $sv" ++ ((show .getObjectNo . toGUIObject) w)
{-# INLINE varname #-}

tkDeclScrollVar :: GUIObject w => w -> String
tkDeclScrollVar w = "global sv" ++ ((show .getObjectNo . toGUIObject) w)
{-# INLINE tkDeclScrollVar #-}

tkScroll :: Orientation -> ObjectName -> Int -> ScrollUnit -> TclCmd
tkScroll ax nm no what = show nm ++ " " ++ oshow ax ++ "view scroll " ++ show no ++ " " ++ show what
{-# INLINE tkScroll #-}

-- added Oct. '01, still experimental (ludi)
tkView :: Orientation -> ObjectName -> TclScript
tkView ax nm = [show nm ++ " " ++ oshow ax ++ "view"]
{-# INLINE tkView #-}

tkMoveTo :: Orientation -> ObjectName -> Fraction -> String
tkMoveTo ax nm f = show nm ++ " " ++ oshow ax ++ "view moveto " ++ show f
{-# INLINE tkMoveTo #-}

tkActivate :: ObjectName -> ScrollBarElem -> String
tkActivate nm e = show nm ++ " activate " ++ show e
{-# INLINE tkActivate #-}

tkGetActivate :: ObjectName -> String
tkGetActivate nm = show nm ++ " activate"
{-# INLINE tkGetActivate #-}

tkFraction :: ObjectName -> Distance -> Distance -> String
tkFraction nm x y = show nm ++ " fraction " ++ show x ++ " " ++ show y
{-# INLINE tkFraction #-}

tkIdentify :: ObjectName -> Distance -> Distance -> String
tkIdentify nm x y = show nm ++ " identify " ++ show x ++ " " ++ show y
{-# INLINE tkIdentify #-}

tkSet :: ObjectName -> Fraction -> Fraction -> String
tkSet nm x y = show nm ++ " set " ++ show x ++ " " ++ show y
{-# INLINE tkSet #-}

oshow Horizontal = "x"
oshow Vertical   = "y"
