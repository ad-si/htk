{- #########################################################################

MODULE        : Packer
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : A simple packer for Widgets.


   ######################################################################### -}


module Packer (
        FillDesignator(..),
        None(..),
        Both(..),

        Side(..), 
        Anchor(..),

        Widget(..), 
        side,
        flexible,

        Cursor(..),
        CursorDesignator(..),

        Orientation(..),
        HasPadding(..),
        getPad,
        pad,
        ipad,
        getIPad,

        highlightBackground,
        getHighlightBackground,

        highlightColour,
        getHighlightColour,

        highlightThickness,
        getHighlightThickness,

        ParentWidget(..),
        ChildWidget(..)

        ) where

import Concurrency
import Resources
import GUIObject
import GUIState
import GUIRealise
import GUIBaseClasses
import FiniteMap
import Char
import Debug(debug)


-- --------------------------------------------------------------------------
-- Fill Designators 
-- --------------------------------------------------------------------------

data None = None

data Both = Both

class FillDesignator f where
        toFill :: f -> String

instance FillDesignator Orientation where
        toFill Horizontal = "x"
        toFill Vertical   = "y"

instance FillDesignator None where
        toFill None = "none"

instance FillDesignator Both where
        toFill Both = "both"


-- --------------------------------------------------------------------------
-- Parent/Child Relationships 
-- --------------------------------------------------------------------------

class (Widget wp, GUIObject wc) => ParentWidget wp wc where
        parent          :: wp -> Config wc
        parent wp wc = do
                packWidget (toGUIObject wp) (toGUIObject wc) Nothing
                return wc


class Widget wc => ChildWidget wc where
        packW          :: wc -> IO ()
        packW wc        = done


class GUIObject w => Widget w where
        cursor          :: CursorDesignator ch => ch -> Config w
        getCursor       :: w -> IO Cursor
        takeFocus       :: Bool -> Config w
        getTakeFocus    :: w -> IO Bool
        fill            :: FillDesignator f => f -> Config w
        expand          :: Toggle -> Config w
        anchor          :: Anchor -> Config w
        getAnchor       :: w -> IO Anchor
        cursor s w       = cset w "cursor" (toCursor s)
        getCursor w      = cget w "cursor"
        takeFocus b w    = cset w "takefocus" b
        getTakeFocus w   = cget w "takefocus"
        fill f w         = pset w "fill" (toFill f)
        expand b w       = pset w "expand" b
        anchor a w       = pset w "anchor" a
        getAnchor w      = pget w "anchor"


-- --------------------------------------------------------------------------
-- Pack Options 
-- --------------------------------------------------------------------------

side :: Widget w => Side -> Config w
side s w = pset w "side" s

flexible :: Widget w => Config w
flexible w = configure w [fill Both, expand On]


-- --------------------------------------------------------------------------
-- Packer Paddings 
-- --------------------------------------------------------------------------

pad :: Widget w => Orientation -> Distance -> Config w
pad Horizontal s w = pset w "padx" s
pad Vertical s w = pset w "pady" s

getPad :: Widget w => Orientation -> w -> IO Distance
getPad Horizontal w = pget w "padx"
getPad Vertical w = pget w "pady"

ipad :: Widget w => Orientation -> Distance -> Config w
ipad Horizontal s w = pset w "ipadx" s
ipad Vertical s w = pset w "ipady" s

getIPad :: Widget w => Orientation -> w -> IO Distance
getIPad Horizontal w = pget w "ipadx"
getIPad Vertical w = pget w "ipady"


-- --------------------------------------------------------------------------
-- Config Paddings 
-- --------------------------------------------------------------------------

class GUIObject w => HasPadding w where
        cpad                :: Orientation -> Distance -> Config w
        getCPad             :: Orientation -> w -> IO Distance
        cpad Horizontal s w  = cset w "padx" s
        cpad Vertical s w    = cset w "pady" s
        getCPad Horizontal w = cget w "padx"
        getCPad Vertical w   = cget w "pady"


-- --------------------------------------------------------------------------
-- Highlight Colours 
-- --------------------------------------------------------------------------

highlightBackground :: (Widget w,ColourDesignator c) => c -> Config w
highlightBackground c w = cset w  "highlightbackground"  (toColour c)

getHighlightBackground :: Widget w => w -> IO Colour
getHighlightBackground w = cget w  "highlightbackground" 

highlightColour      :: (Widget w,ColourDesignator c) => c -> Config w
highlightColour c w  = cset w "highlightcolor"  (toColour c)

getHighlightColour :: Widget w => w -> IO Colour
getHighlightColour w = cget w "highlightcolor" 

highlightThickness  :: Widget w => Distance -> Config w
highlightThickness s w = cset w "highlightthickness" s

getHighlightThickness :: Widget w => w -> IO Distance
getHighlightThickness w = cget w "highlightthickness" 


-- --------------------------------------------------------------------------
--  Anchor 
-- --------------------------------------------------------------------------

data Anchor = SouthEast 
        | South 
        | SouthWest  
        | East 
        | Center 
        | West
        | NorthEast 
        | North 
        | NorthWest
        deriving (Eq,Ord,Enum)


instance GUIValue Anchor where
        cdefault = Center

instance Read Anchor where
   readsPrec p b =
     case dropWhile (isSpace) b of
        's':'e':xs -> [(SouthEast,xs)]
        's':'w':xs -> [(SouthWest,xs)]
        'c':'e':'n':'t':'e':'r':xs -> [(Center,xs)]
        'n':'e':xs -> [(NorthEast,xs)]
        'n':'w':xs -> [(NorthWest,xs)]
        'e':xs -> [(East,xs)]
        'n':xs -> [(North,xs)]
        'w':xs -> [(West,xs)]
        's': xs -> [(South,xs)]
        _ -> []

instance Show Anchor where
   showsPrec d p r = 
      (case p of 
         SouthEast -> "se"
         South -> "s" 
         SouthWest -> "sw"
         East -> "e"
         Center -> "center" 
         West -> "w" 
         NorthEast -> "ne" 
         North -> "n" 
         NorthWest -> "nw"
        ) ++ r


-- --------------------------------------------------------------------------
--  Side 
-- --------------------------------------------------------------------------

data Side = AtTop | AtBottom | AtLeft | AtRight deriving (Eq,Ord,Enum)

instance GUIValue Side where
        cdefault = AtTop

instance Read Side where
   readsPrec p b =
     case dropWhile (isSpace) b of
        't':'o':'p':xs -> [(AtTop,xs)]
        'b':'o':'t':'t':'o':'m': xs -> [(AtBottom,xs)]
        'l':'e':'f':'t':xs -> [(AtLeft,xs)]
        'r':'i':'g':'h':'t':xs -> [(AtRight,xs)]
        _ -> []

instance Show Side where
   showsPrec d p r = 
      (case p of 
         AtTop -> "top"
         AtBottom -> "bottom"
         AtLeft -> "left"
         AtRight -> "right"
        ) ++ r
