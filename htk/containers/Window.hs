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

module Window (

  Window(..),
  Display,
  HasGeometry(..),
  HasSize(..),
  HasPosition(..),
  HasText(..),
  maxSize,
  getMaxSize,
  minSize,
  getMinSize,
  raiseWin,
  lowerWin,
--  WindowPool,
--  windowpool,
--  getWindowPool,
--  addWindowToPool,
--  addWindowToPool,
--  removeWindowFromPool,
--  getWindows,
--  lookupWindow,
  WindowState(..),
  AspectRatio,
  Whom,
  isWMConfig,

) where

import Resources
import Geometry
import Configuration
import ReferenceVariables
import Core
import Char
import Computation
import IOExts

type Display = String

-- -----------------------------------------------------------------------
-- class Window
-- -----------------------------------------------------------------------

class GUIObject w => Window w where
  iconify :: w -> IO ()
  deiconify :: w -> IO ()
  withdraw :: w -> IO ()
  putWinOnTop :: w -> IO ()
  putWinAtBottom :: w -> IO ()
--  transient :: Config w
--  getTransient :: Config w
  screen :: Display -> Config w
  getScreen :: w -> IO (Display)
  getClassName :: w -> IO String
  getWindowState :: w -> IO WindowState
  aspectRatio :: AspectRatio -> Config w
  getAspectRatio :: w -> IO AspectRatio
  positionFrom :: Whom -> Config w
  getPositionFrom :: w -> IO Whom
  sizeFrom :: Whom -> Config w
  getSizeFrom :: w -> IO Whom

  iconify win = cset win "state" Iconified >> done
  deiconify win = do {cset win "state" Deiconified; done}
  withdraw win = do {cset win "state" Withdrawn; done}
  putWinOnTop win  = execMethod win (\nm -> [tkPutOnTop nm])
  putWinAtBottom win = execMethod win (\nm -> [tkPutAtBottom nm])

{- TD (ludi)
  transient :: Window -> Config Window
  transient pwin twin @ (Window _ _ pv) = do {
          onm <- getObjectName (toGUIObject pwin); 
          case onm of
                  (Just (ObjectName pname)) -> 
                          synchronize twin (do {  
                                  cset twin "transient" pname;
                                  changeVar' pv (\(m,_) -> (m,Just pwin));
                                  return twin
                                  })
                  _ -> raise objectNotPacked
  }

  getTransient :: Window -> IO (Maybe Window)
  getTransient (Window _ _ pv) = withVar' pv (\(_,t) -> t)
-}

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

instance Window w => HasGeometry w where
  geometry g win = cset win "geometry" g
  getGeometry win = cget win "geometry"

instance Window w => HasSize w where
  width w win = getGeometry win >>= \(_,h,x,y) -> geometry (w,h,x,y) win
  getWidth win = getGeometry win >>= \ (w,_,_,_) -> return w
  height h win = getGeometry win >>= \(w,_,x,y) -> geometry (w,h,x,y) win
  getHeight win =
    do
      (_,h,_, _) <- getGeometry win
      return h
  size (w,h) win =
    do
      (_,_,x,y) <- getGeometry win
      geometry (w,h,x,y) win
  getSize win = getGeometry win >>= \(w,h,_,_) -> return (w,h)

instance Window w => HasPosition w where
  position (x,y) win =
    do
      (w, h, _, _) <- getGeometry win
      geometry (w, h, x, y) win
  getPosition win =
    do
      (_, _, x, y) <- getGeometry win
      return (x, y)

instance (Window w, GUIValue v) => HasText w v where
  text s win  = cset win "iconname" s >> cset win "title" s
  getText win = cget win "title"

--instance Window w => HasMenu w


-- -----------------------------------------------------------------------
-- maximum and minimum size's
-- -----------------------------------------------------------------------

maxSize :: Window w => Size -> Config w
maxSize s win = cset win "maxsize" s

getMaxSize :: Window w => w -> IO Size
getMaxSize win = cget win "maxsize"

minSize :: Window w => Size -> Config w
minSize s win = cset win "minsize" s

getMinSize :: Window w => w -> IO Size
getMinSize win = cget win "minsize"


-- -----------------------------------------------------------------------
-- stack order
-- -----------------------------------------------------------------------

raiseWin :: (Window w1, Window w2) => w1 -> w2 -> IO ()
raiseWin win1 win2 =
  do
    nm2 <- getObjectName (toGUIObject win2)
    execMethod win1 (\nm1 -> [tkRaise nm1 nm2])

lowerWin :: (Window w1, Window w2) => w1 -> w2 -> IO ()
lowerWin win1 win2 =
  do
    nm2 <- getObjectName (toGUIObject win2)
    execMethod win1 (\nm1 -> [tkLower nm1 nm2])

{-
-- -----------------------------------------------------------------------
--  WindowPool
-- -----------------------------------------------------------------------

type WindowPool = Ref [Window]

windowpool :: WindowPool
windowpool = IOExts.unsafePerformIO (newRef [])

getWindowPool :: IO WindowPool
getWindowPool = return windowpool

addWindowToPool :: Window -> IO ()
addWindowToPool win = changeRef windowpool (\wins -> win : wins)

removeWindowFromPool :: Window -> IO ()
removeWindowFromPool win =
  changeRef windowpool (\wins -> filter (/= win) wins)

getWindows :: IO [Window]
getWindows = getRef windowpool

lookupWindow :: String -> IO (Maybe Window)
lookupWindow nm =
  do
    mobj <- lookupGUIObjectByName (WidgetName nm);
    case mobj of
      Nothing -> return Nothing
      Just wid ->
        do
          wins <- getWindows
          case (dropWhile (\win -> wid /= (toGUIObject win))) wins of
            [] -> return Nothing
            (win:_) -> return (Just win)
-}


-- -----------------------------------------------------------------------
-- WindowState
-- -----------------------------------------------------------------------

data WindowState =
  Deiconified | Iconified | Withdrawn deriving (Eq,Ord,Enum)

instance GUIValue WindowState where
  cdefault = Deiconified

instance Read WindowState where
  readsPrec p b =
    case dropWhile (isSpace) b of
      'n':'o':'r':'m':'a':'l':xs -> [(Deiconified,xs)]
      'i':'c':'o':'n':'i':'c':xs -> [(Iconified,xs)]
      'w':'i':'t':'h':'d':'r':'a':'w':xs -> [(Withdrawn,xs)]
      _ -> []

instance Show WindowState where
  showsPrec d p r = 
    (case p of 
       Deiconified -> "normal"
       Iconified -> "iconic"
       Withdrawn -> "withdraw") ++ r


-- -----------------------------------------------------------------------
-- AspectRatio 
-- -----------------------------------------------------------------------

data AspectRatio = AspectRatio Int Int Int Int deriving Eq

instance GUIValue AspectRatio where
  cdefault = AspectRatio 0 0 0 0
  toGUIValue v  = GUIVALUE HaskellTk (show v)
  maybeGUIValue (GUIVALUE _ s)     = 
    case [x | (x,t) <- reads s, ("","") <- lex t] of
      [x] -> Just x
      _ -> Nothing

instance Show AspectRatio where
  showsPrec d c r = cshow c ++ r
    where cshow (AspectRatio xt yt xf yf) = 
            (show xt) ++ " " ++ (show yt) ++ " " ++
            (show xf) ++ " " ++ (show yf)

instance Read AspectRatio where
  readsPrec p str = [(cread str,[])] 
    where cread str = AspectRatio (read xt) (read yt) (read xf) (read yf)
          [xt,yt,xf,yf] = words str 


-- -----------------------------------------------------------------------
-- Whom
-- -----------------------------------------------------------------------

data Whom = Program | User deriving (Eq,Ord,Enum)

instance GUIValue Whom where
        cdefault = Program

instance Read Whom where
  readsPrec p b =
    case dropWhile (isSpace) b of
      'u':'s':'e':'r':xs -> [(User,xs)]
      'p':'r':'o':'g':'r':'a':'m':xs -> [(Program,xs)]
      _ -> []

instance Show Whom where
  showsPrec d p r = 
    (case p of 
       Program -> "program"
       User -> "user") ++ r


-- -----------------------------------------------------------------------
-- auxiliary functions
-- -----------------------------------------------------------------------

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
