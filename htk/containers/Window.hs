{- #########################################################################

MODULE        : Window
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   :  

TO BE DONE    : Packing of master windows. 

                Packing of transient windows should be changed so that
                it would work event if the parent widget is not realised
                at the moment when the application calls transient!

                Some of the events for setting geometries have overlapping
                interaction patters, i.e. if used in a +> only one
                will trigger. They should probably be factored out.
                
                wm grid command
                class attribute


   ######################################################################### -}

module Window (
        Window, 

        window,
        renderWindow,
        newWindow,

        getWindows,
        lookupWindow,

        Toplevel,
        getToplevel,

        closed,

        modal,
        getModal,

        transient,
        getTransient,
        
        Display,
        screen,
        getScreen,

        AspectRatio(..),
        aspectRatio,
        getAspectRatio,

        Whom(..),
        positionFrom,
        getPositionFrom,

        sizeFrom,
        getSizeFrom,

        raiseWin,
        lowerWin,

        WindowState(..), 
        getWindowState,
 
        Distance,
        Position, 
        Size, 
        Geometry,

        maxSize, 
        getMaxSize, 

        minSize, 
        getMinSize      

        ) where

import qualified IOExts(unsafePerformIO)

import SIM
import Resources
import GUIObject
import GUIState
import GUIRealise
import GUIBaseClasses
import GUIInteraction
import Packer
import Toplevel
import FiniteMap
import Char
import List
import Debug(debug)


-- --------------------------------------------------------------------------
-- Windows 
-- --------------------------------------------------------------------------           
data Window = Window GUIOBJECT GUI (PVar (Bool,Maybe Window))


-- --------------------------------------------------------------------------
-- Window Creation 
-- --------------------------------------------------------------------------           
window :: (Widget w, Synchronized w) => w -> [Config Window] -> IO Window
window w opts = 
        synchronize w (do {
                win <- newWindow w opts;
                renderWindow win;
                return win
                })


renderWindow :: Window -> IO ()
renderWindow win @ (Window top _ _) = do {
        master <- return Nothing;               -- TBD
        displayWindow master top;
        done
        }


newWindow :: (Widget w, Synchronized w) => w -> [Config Window] -> IO Window
newWindow w opts = 
        synchronize w (do {
                checkIfUnpacked wid;
                kind <- getObjectKind wid;
                tp <- newToplevel w [];
                pv <- newPVar (False,Nothing);
                gui <- getToolInstance;
                win <- return (Window (toGUIObject tp) gui pv);
                configure win opts;
                addWindowToPool win;
                return win              
                })
         where wid = toGUIObject w


getToplevel :: Window -> IO (Toplevel)
getToplevel win = return (Toplevel (toGUIObject win))


-- --------------------------------------------------------------------------
-- Instantions 
-- --------------------------------------------------------------------------           
instance Eq Window where
        (Window win1 _ _) == (Window win2 _ _) = win1 == win2
        win1 /= win2 = not (win1 == win2)

instance Ord Window where
        (Window win1 _ _) <= (Window win2 _ _) = win1 <= win2

instance GUIObject Window where 
        toGUIObject (Window wid _ _) = wid
        cname _ = "Window"

instance Destructible Window where
        destroy win @ (Window w _ _) = do { 
                emitGUIEvent win (WindowDestroyed) NoEventInfo;
                removeWindowFromPool win;
                destroy w
                }
        destroyed win @ (Window _ gui _) = 
                closed win >>> do {try (destroy win); done}
           +>   destroyed gui

instance Interactive Window

instance ToplevelWindow Window where
        iconify win = do {cset win "state" Iconified; done}
        deiconify win = do {cset win "state" Deiconified; done}
        withdraw win = do {cset win "state" Withdrawn; done}
        putWinOnTop win  = execMethod win (\nm -> [tkRaise nm Nothing])
        putWinAtBottom win = execMethod win (\nm -> [tkLower nm Nothing])

instance Synchronized Window where
        synchronize (Window wid _ _) = synchronize wid



-- --------------------------------------------------------------------------
-- Screen 
-- --------------------------------------------------------------------------           
type Display = String

screen :: Display -> Config Window
screen "" win = cset win "screen" ":0.0"
screen scr win = cset win "screen" scr

getScreen :: Window -> IO (Display)
getScreen win = cget win "screen"


-- --------------------------------------------------------------------------
-- Modality of Window 
-- --------------------------------------------------------------------------           
modal :: Bool -> Config Window
modal m win @ (Window _ _ pv) =  do {
        changeVar' pv (\(_,t) -> (m,t));
        return win
}

getModal :: Window -> IO Bool
getModal win @ (Window _ _ pv) = withVar' pv (\(m,_) -> m)



-- --------------------------------------------------------------------------
-- Transient Window 
-- --------------------------------------------------------------------------           
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


-- --------------------------------------------------------------------------
-- Events 
-- --------------------------------------------------------------------------           
closed :: Window -> IA ()
closed win = userinteraction win WindowDestroyed Notice >>> done

saveYourself :: Window -> IA ()
saveYourself win = userinteraction win SaveYourself Notice >>> done


-- --------------------------------------------------------------------------
-- Window Title 
-- --------------------------------------------------------------------------           
instance GUIValue v => HasText Window v where
        text s win  = synchronize win(do {cset win "iconname" s;cset win "title" s})
        getText win = cget win "title"
        

-- --------------------------------------------------------------------------
-- Class Name 
-- --------------------------------------------------------------------------           
getClassName :: Window -> IO String
getClassName win = evalMethod win (\nm -> [tkWInfoClass nm])


-- --------------------------------------------------------------------------
-- Window State 
-- --------------------------------------------------------------------------           
getWindowState :: Window -> IO WindowState
getWindowState win = cget win "state"


-- --------------------------------------------------------------------------
-- Window Size/Control Configs 
-- --------------------------------------------------------------------------           
aspectRatio :: AspectRatio -> Config Window
aspectRatio ratio win = cset win "aspect" ratio

getAspectRatio :: Window -> IO AspectRatio
getAspectRatio win = cget win "aspect" 

positionFrom :: Whom -> Config Window
positionFrom w win = cset win "positionfrom" w

getPositionFrom :: Window -> IO Whom
getPositionFrom win = cget win "positionfrom"

sizeFrom :: Whom -> Config Window
sizeFrom w win = cset win "sizefrom" w

getSizeFrom :: Window -> IO Whom
getSizeFrom win = cget win "sizefrom"


-- --------------------------------------------------------------------------
-- Geometry 
-- --------------------------------------------------------------------------           
instance HasGeometry Window where
        geometry g win = cset win "geometry" g
        getGeometry win = tkcget win "geometry" 

instance HasSize Window  where
        width w win = getGeometry win >>= \(_,h,x,y) -> geometry (w,h,x,y) win
        getWidth win = getGeometry win >>= \ (w,_,_,_) -> return w
        height h win = getGeometry win >>= \(w,_,x,y) -> geometry (w,h,x,y) win
        getHeight win = do {(_,h,_, _) <- getGeometry win; return h}
        size (w,h) win = do {
                (_,_,x,y) <- getGeometry win; 
                geometry (w,h,x,y) win
                }
        getSize win = getGeometry win >>= \(w,h,_,_) -> return (w,h)


instance HasPosition Window where
        position (x,y) win = do {
                (w,h,_,_) <- getGeometry win; 
                geometry (w,h,x,y) win
                }
        getPosition win = do {
                (_,_,x,y) <- getGeometry win;
                return (x,y)
                }


-- --------------------------------------------------------------------------
-- Maximum and Minimum Size's 
-- --------------------------------------------------------------------------           
maxSize :: Size -> Config Window
maxSize s win = cset win "maxsize" s

getMaxSize :: Window -> IO Size
getMaxSize win = cget win "maxsize"

minSize :: Size -> Config Window
minSize s win = cset win "minsize" s

getMinSize :: Window -> IO Size
getMinSize win = cget win "minsize"


-- --------------------------------------------------------------------------
-- Stack Order 
-- --------------------------------------------------------------------------           
raiseWin :: Window -> Window -> IO ()
raiseWin win1 win2 = do {
        nm2 <- getObjectName (toGUIObject win2); 
        execMethod win1 (\nm1 -> [tkRaise nm1 nm2])
        }

lowerWin :: Window -> Window -> IO ()
lowerWin win1 win2 = do {
        nm2 <- getObjectName (toGUIObject win2);
        execMethod win1 (\nm1 -> [tkLower nm1 nm2])
        }


-- --------------------------------------------------------------------------
--  WindowPool 
-- --------------------------------------------------------------------------

type WindowPool = PVar ([Window])

windowpool :: WindowPool
windowpool = IOExts.unsafePerformIO (newPVar [])

getWindowPool :: IO WindowPool
getWindowPool = return windowpool

addWindowToPool :: Window -> IO ()
addWindowToPool win = changeVar' windowpool (\wins -> win : wins)

removeWindowFromPool :: Window -> IO ()
removeWindowFromPool win = changeVar' windowpool (\wins -> filter (/= win) wins)

getWindows :: IO [Window]
getWindows = getVar windowpool

lookupWindow :: String -> IO (Maybe Window)
lookupWindow nm = do {
        mobj <- lookupGUIObjectByName (WidgetName nm);
        case mobj of
                Nothing -> return Nothing
                (Just wid) -> do {
                        wins <- getWindows;
                        case (dropWhile (\win -> wid /= (toGUIObject win))) wins of
                                [] -> return Nothing
                                (win:_) -> return (Just win)
                        }       
        }

        
-- --------------------------------------------------------------------------
--  WindowState 
-- --------------------------------------------------------------------------

data WindowState = Deiconified | Iconified | Withdrawn deriving (Eq,Ord,Enum)

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
         Withdrawn -> "withdraw"
        ) ++ r


-- --------------------------------------------------------------------------
--  Aspect Ratio 
-- --------------------------------------------------------------------------

data AspectRatio = AspectRatio Int Int Int Int deriving Eq

instance GUIValue AspectRatio where
        cdefault = AspectRatio 0 0 0 0
        toGUIValue v  = GUIVALUE HaskellTk (show v)
        maybeGUIValue (GUIVALUE _ s)     = 
                case [x | (x,t) <- reads s, ("","") <- lex t] of
                        [x] -> Just x
                        _   -> Nothing  

instance Show AspectRatio where
   showsPrec d c r = cshow c ++ r
     where cshow (AspectRatio xt yt xf yf) = 
                (show xt) ++ " " ++ (show yt) ++ " " ++
                (show xf) ++ " " ++ (show yf)

instance Read AspectRatio where
    readsPrec p str = [(cread str,[])] 
     where cread str = AspectRatio (read xt) (read yt) (read xf) (read yf)
           [xt,yt,xf,yf] = words str 
                

-- --------------------------------------------------------------------------
--  Whom 
-- --------------------------------------------------------------------------

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
          User -> "user"
      ) ++ r


-- --------------------------------------------------------------------------
-- Unparsing of Commands
-- --------------------------------------------------------------------------

tkRaise :: ObjectName -> Maybe ObjectName -> TclCmd
tkRaise win1 Nothing = "raise " ++ show win1
tkRaise win1 (Just win2) = "raise " ++ show win1 ++ " " ++ show win2
{-# INLINE tkRaise #-}


tkLower :: ObjectName -> Maybe ObjectName -> TclCmd
tkLower win1 Nothing = "lower " ++ show win1 
tkLower win1 (Just win2) = "lower " ++ show win1 ++ " " ++ show win2
{-# INLINE tkLower #-}


tkWInfoClass :: ObjectName -> TclCmd
tkWInfoClass nm = "winfo class " ++ show nm
{-# INLINE tkWInfoClass #-}
