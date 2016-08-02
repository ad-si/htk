{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Basic types and classes concerning toplevel window resources.
module HTk.Containers.Window (

  Window(..),
  Display,
  maxSize,
  getMaxSize,
  minSize,
  getMinSize,
  raiseWin,
  lowerWin,
  WindowState(..),
  AspectRatio,
  Whom,
  isWMConfig,

) where

import Util.Computation

import HTk.Kernel.Geometry
import HTk.Kernel.Configuration
import HTk.Kernel.Core
import Data.Char

type Display = String


-- -----------------------------------------------------------------------
-- class Window
-- -----------------------------------------------------------------------

-- | Toplevel windows instantiate the @class Window@.
class GUIObject w => Window w where
  -- Iconifies the window.
  iconify :: w -> IO ()
  -- Deiconifies the window.
  deiconify :: w -> IO ()
  -- Withdraws the window.
  withdraw :: w -> IO ()
  -- Puts the window on top.
  putWinOnTop :: w -> IO ()
  -- Puts the window at bottom.
  putWinAtBottom :: w -> IO ()
  -- Sets the screen for this window.
  screen :: Display -> Config w
  -- Gets the screen from this window.
  getScreen :: w -> IO (Display)
  -- Returns the resource class of the given window.
  getClassName :: w -> IO String
  -- Gets the current window state.
  getWindowState :: w -> IO WindowState
  -- Sets the aspect ratio for the given window.
  aspectRatio :: AspectRatio -> Config w
  -- Gets the aspect ratio of the given window.
  getAspectRatio :: w -> IO AspectRatio
  -- Set \'@Whom@\' to be @Program@ or
  -- @User@.
  positionFrom :: Whom -> Config w
  -- Gets the current setting.
  getPositionFrom :: w -> IO Whom
  -- Set \'@Whom@\' to be @Program@ or
  sizeFrom :: Whom -> Config w
  -- Gets the current setting.
  getSizeFrom :: w -> IO Whom

  iconify win = cset win "state" Iconified >> done
  deiconify win = do {cset win "state" Deiconified; done}
  withdraw win = do {cset win "state" Withdrawn; done}
  putWinOnTop win  = execMethod win (\nm -> [tkPutOnTop nm])
  putWinAtBottom win = execMethod win (\nm -> [tkPutAtBottom nm])

  screen "" win = cset win "screen" ":0.0"
  screen scr win = cset win "screen" scr

  getScreen win = cget win "screen"

  getClassName win = evalMethod win (\nm -> [tkWInfoClass nm])

  getWindowState win = cget win "state"

  aspectRatio ratio win = cset win "aspect" ratio

  getAspectRatio win = cget win "aspect"

  positionFrom w win = cset win "positionfrom" w

  getPositionFrom win = cget win "positionfrom"

  sizeFrom w win = cset win "sizefrom" w

  getSizeFrom win = cget win "sizefrom"


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

-- | A window has a configureable size and anchor position (geometry).
instance Window w => HasGeometry w where
  --  Sets the window\'s geometry.
  geometry g win = cset win "geometry" g
  --  Gets the current geometry of the given window.
  getGeometry win = cget win "geometry"

-- | A window has a configureable size.
instance (Window w, GUIObject w) => HasSize w where
  --  Sets the window\'s width.
  width w win = getGeometry win >>= \(_,h,x,y) -> geometry (w,h,x,y) win
  --  Gets the window\'s width.
  getWidth win = getGeometry win >>= \ (w,_,_,_) -> return w
  --  Sets the window\'s height.
  height h win = getGeometry win >>= \(w,_,x,y) -> geometry (w,h,x,y) win
  --  Gets the window\'s height.
  getHeight win =
    do
      (_,h,_, _) <- getGeometry win
      return h
  --  Sets the window\'s width and height.
  size (w,h) win =
    do
      (_,_,x,y) <- getGeometry win
      geometry (w,h,x,y) win
  --  Gets the window\'s width and height.
  getSize win = getGeometry win >>= \(w,h,_,_) -> return (w,h)

-- | A window has a position on the associated screen.
instance (Window w, GUIObject w) => HasPosition w where
  --  Sets the window\'s position-
  position (x,y) win =
    do
      (w, h, _, _) <- getGeometry win
      geometry (w, h, x, y) win
  --  Gets the window\'s position.
  getPosition win =
    do
      (_, _, x, y) <- getGeometry win
      return (x, y)

-- | A window has a title.
instance (Window w, GUIValue v) => HasText w v where
  --  Sets the window\'s title.
  text s win  = cset win "iconname" s >> cset win "title" s
  --  Gets the window\'s title.
  getText win = cget win "title"


-- -----------------------------------------------------------------------
-- maximum and minimum size's
-- -----------------------------------------------------------------------

-- | Constraints the maximum size of the window.
maxSize :: Window w => Size -> Config w
maxSize s win = cset win "maxsize" s

-- | Gets the maximum size of the window.
getMaxSize :: Window w => w -> IO Size
getMaxSize win = cget win "maxsize"

-- | Constraints the minimum size of the window.
minSize :: Window w => Size -> Config w
minSize s win = cset win "minsize" s

-- | Gets the minimum size of the window.
getMinSize :: Window w => w -> IO Size
getMinSize win = cget win "minsize"


-- -----------------------------------------------------------------------
-- stack order
-- -----------------------------------------------------------------------

-- | Puts the first given window just above the second given window
-- in the stacking order.
raiseWin :: (Window w1, Window w2) => w1
   -- ^ the first window.
   -> w2
   -- ^ the second window.
   -> IO ()
   -- ^ None.
raiseWin win1 win2 =
  do
    nm2 <- getObjectName (toGUIObject win2)
    execMethod win1 (\nm1 -> [tkRaise nm1 nm2])

-- | Puts the first given window just below the second given window
-- in the stacking order.
lowerWin :: (Window w1, Window w2) => w1
   -- ^ the first window.
   -> w2
   -- ^ the second window.
   -> IO ()
   -- ^ None.
lowerWin win1 win2 =
  do
    nm2 <- getObjectName (toGUIObject win2)
    execMethod win1 (\nm1 -> [tkLower nm1 nm2])


-- -----------------------------------------------------------------------
-- WindowState
-- -----------------------------------------------------------------------

-- | The @WindowState@ datatype.
data WindowState =
  Deiconified | Iconified | Withdrawn deriving (Eq,Ord,Enum)

-- | Internal.
instance GUIValue WindowState where
  cdefault = Deiconified

-- | Internal.
instance Read WindowState where
  readsPrec p b =
    case dropWhile (isSpace) b of
      'n':'o':'r':'m':'a':'l':xs -> [(Deiconified,xs)]
      'i':'c':'o':'n':'i':'c':xs -> [(Iconified,xs)]
      'w':'i':'t':'h':'d':'r':'a':'w':xs -> [(Withdrawn,xs)]
      _ -> []

-- | Internal.
instance Show WindowState where
  showsPrec d p r =
    (case p of
       Deiconified -> "deiconify"
       Iconified -> "iconic"
       Withdrawn -> "withdraw") ++ r


-- -----------------------------------------------------------------------
-- AspectRatio
-- -----------------------------------------------------------------------

-- | The @AspectRatio@ datatype.
data AspectRatio = AspectRatio Int Int Int Int deriving Eq

-- | Internal.
instance GUIValue AspectRatio where
  cdefault = AspectRatio 0 0 0 0
  toGUIValue v  = GUIVALUE HaskellTk (show v)
  maybeGUIValue (GUIVALUE _ s)     =
    case [x | (x,t) <- reads s, ("","") <- lex t] of
      [x] -> Just x
      _ -> Nothing

-- | Internal.
instance Show AspectRatio where
  showsPrec d c r = cshow c ++ r
    where cshow (AspectRatio xt yt xf yf) =
            (show xt) ++ " " ++ (show yt) ++ " " ++
            (show xf) ++ " " ++ (show yf)

-- | Internal.
instance Read AspectRatio where
  readsPrec p str = [(cread str,[])]
    where cread str = AspectRatio (read xt) (read yt) (read xf) (read yf)
          [xt,yt,xf,yf] = words str


-- -----------------------------------------------------------------------
-- Whom
-- -----------------------------------------------------------------------

-- | The @Whom@ datatype.
data Whom = Program | User deriving (Eq,Ord,Enum)

-- | Internal.
instance GUIValue Whom where
  cdefault = Program

-- | Internal.
instance Read Whom where
  readsPrec p b =
    case dropWhile (isSpace) b of
      'u':'s':'e':'r':xs -> [(User,xs)]
      'p':'r':'o':'g':'r':'a':'m':xs -> [(Program,xs)]
      _ -> []

-- | Internal.
instance Show Whom where
  showsPrec d p r =
    (case p of
       Program -> "program"
       User -> "user") ++ r


-- -----------------------------------------------------------------------
-- auxiliary functions
-- -----------------------------------------------------------------------

-- | Internal.
isWMConfig :: ConfigID -> Bool
isWMConfig "state" = True
isWMConfig "geometry" = True
isWMConfig "minsize" = True
isWMConfig "maxsize" = True
isWMConfig "aspect" = True
isWMConfig "sizefrom" = True
isWMConfig "positionfrom" = True
isWMConfig "title" = True
isWMConfig "transient" = True
isWMConfig "group" = True
isWMConfig "iconname" = True
isWMConfig "iconbitmap" = True
isWMConfig "iconposition" = True
isWMConfig "iconmask" = True
isWMConfig "focusmodel" = True
isWMConfig _ = False


-- -----------------------------------------------------------------------
-- unparsing of commands
-- -----------------------------------------------------------------------

tkWInfoClass :: ObjectName -> TclCmd
tkWInfoClass nm = "winfo class " ++ show nm
{-# INLINE tkWInfoClass #-}

tkPutOnTop :: ObjectName -> TclCmd
tkPutOnTop win = "raise " ++ show win
{-# INLINE tkPutOnTop #-}

tkPutAtBottom :: ObjectName -> TclCmd
tkPutAtBottom win = "lower " ++ show win
{-# INLINE tkPutAtBottom #-}

tkRaise :: ObjectName -> ObjectName -> TclCmd
tkRaise win1 win2 = "raise " ++ show win1 ++ " " ++ show win2
{-# INLINE tkRaise #-}

tkLower :: ObjectName -> ObjectName -> TclCmd
tkLower win1 win2 = "lower " ++ show win1 ++ " " ++ show win2
{-# INLINE tkLower #-}
