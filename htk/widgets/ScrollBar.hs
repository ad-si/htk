{- #######################################################################

MODULE        : ScrollBar
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Scrollbar Widget

TO BE DONE    : Interaction for scrolled event

   #################################################################### -}


module ScrollBar (

  HasScroller(..),
  ScrollBar,
  newScrollBar,

  ScrollUnit(..),

  Slider(..),
  HasSlider(..),

  ScrollBarElem(..),
  activateScrollBarElem,
  getActivatedElem,

  fraction,
  identify,
  setView
        
) where

import Core
import BaseClasses(Widget)
import Configuration
import Geometry
import Resources
import Destructible
import Slider
import Char
import Computation
import Synchronized
import ReferenceVariables
import Packer
import Tooltip


-- -----------------------------------------------------------------------
-- classes
-- -----------------------------------------------------------------------

type Fraction = Double

class Widget w => HasScroller w where
  isWfOrientation :: w -> Orientation -> Bool
  scrollbar       :: Orientation -> ScrollBar -> Config w
  moveto          :: Orientation -> w -> Fraction -> IO ()
  scroll          :: Orientation -> w -> Int -> ScrollUnit -> IO ()

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


-- -----------------------------------------------------------------------
-- ScrollBar type
-- -----------------------------------------------------------------------

newtype ScrollBar = ScrollBar GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- ScrollBar creation
-- -----------------------------------------------------------------------

newScrollBar :: Container par => par -> [Config ScrollBar] -> IO ScrollBar
newScrollBar par ol =
  do
    w <- createGUIObject (toGUIObject par) SCROLLBAR scrollbarMethods 
    configure (ScrollBar w) ol


-- -----------------------------------------------------------------------
-- ScrollBar configuration options
-- -----------------------------------------------------------------------

instance GUIObject ScrollBar where 
  toGUIObject (ScrollBar w) = w
  cname _ = "ScrollBar"

instance Destroyable ScrollBar where
  destroy   = destroy . toGUIObject

instance Widget ScrollBar 

instance HasBorder ScrollBar

instance HasColour ScrollBar where
  legalColourID w "bg" = True
  legalColourID w "activebackground" = True -- regards slider actually
  legalColourID w _ = False

instance HasEnable ScrollBar

instance HasSize ScrollBar where
  height _ w = return w
  getHeight w = return cdefault

instance HasSlider ScrollBar

instance HasOrientation ScrollBar

---
-- A scrollbar can have a tooltip.
instance HasTooltip ScrollBar


-- -----------------------------------------------------------------------
--  ScrollBar commands
-- -----------------------------------------------------------------------

activateScrollBarElem :: ScrollBar -> ScrollBarElem -> IO ()
activateScrollBarElem sc elem =
  execMethod sc (\nm -> [tkActivate nm elem])

getActivatedElem :: ScrollBar -> IO (Maybe ScrollBarElem)
getActivatedElem sc =
  do
    e <- evalMethod sc (\nm -> [tkGetActivate nm])
    case dropWhile isSpace e of
      "" -> return Nothing
      x -> return (Just (read x))

fraction :: ScrollBar -> Distance -> Distance -> IO Fraction
fraction sc x y = evalMethod sc (\nm -> [tkFraction nm x y])

identify :: ScrollBar -> Distance -> Distance -> IO (Maybe ScrollBarElem)
identify sc x y =
  do
    e <- evalMethod sc (\nm -> [tkIdentify nm x y])
    case dropWhile (isSpace) e of
      "" -> return Nothing
      x -> return (Just (read x))

setView :: ScrollBar -> Fraction -> Fraction -> IO ()
setView sc f l = execMethod sc (\nm -> [tkSet nm f l])


-- -----------------------------------------------------------------------
-- ScrollBar elem
-- -----------------------------------------------------------------------

data ScrollBarElem = 
    Arrow1 
  | Trough1 
  | ScrollBarSlider 
  | Trough2 
  | Arrow2
  deriving (Eq,Ord,Enum)

instance GUIValue ScrollBarElem where
  cdefault = ScrollBarSlider

instance Read ScrollBarElem where
  readsPrec p b =
    case dropWhile (isSpace) b of
       'a':'r':'r':'o':'w':'1':xs -> [(Arrow1,xs)]
       't':'r':'o':'u':'g':'h':'1':xs -> [(Trough1,xs)]
       's':'l':'i':'d':'e':'r':xs -> [(ScrollBarSlider,xs)]
       't':'r':'o':'u':'g':'h':'2':xs -> [(Trough2,xs)]
       'a':'r':'r':'o':'w':'2':xs -> [(Arrow2,xs)]
       _ -> []

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

data ScrollUnit = Units | Pages

instance GUIValue ScrollUnit where
        cdefault = Units

instance Read ScrollUnit where
   readsPrec p b =
     case dropWhile (isSpace) b of
        'u':'n':'i':'t':'s':xs -> [(Units,xs)]
        'p':'a':'g':'e':'s':xs -> [(Pages,xs)]
        _ -> []

instance Show ScrollUnit where
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
