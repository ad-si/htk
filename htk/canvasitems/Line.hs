{- #########################################################################

MODULE        : Line
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Canvas Widget & Canvas Tags & Embedded Windows

   ######################################################################### -}


module Line (
        module CanvasItem,

        ArrowHead(..),
        CapStyle(..),
        JoinStyle(..),

        Line,
        newLine,

        arrowshape,
        getArrowshape,

        arrowstyle,
        getArrowstyle,

        capstyle,
        getCapstyle,

        joinstyle,
        getJoinstyle

        ) where

import Concurrency
import GUICore
import CanvasItem
import CanvasTag
import CanvasItemAux
import Char
import Debug(debug)


-- --------------------------------------------------------------------------
-- Line
-- --------------------------------------------------------------------------

newtype Line = Line GUIOBJECT deriving Eq

type ArrowShape = (Distance,Distance,Distance)


-- --------------------------------------------------------------------------
-- Constructor
-- --------------------------------------------------------------------------

newLine :: [Config Line] -> IO Line
newLine ol = createCanvasItem LINE Line ol 


-- --------------------------------------------------------------------------
-- Instantiations
-- --------------------------------------------------------------------------

instance GUIObject Line where 
        toGUIObject (Line w) = w; cname _ = "Line"

instance Destructible Line where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive Line

instance CanvasItem Line where 
        defaultCoord w = [(0,0),(0,0)]

instance TaggedCanvasItem Line

instance FilledCanvasItem Line where
        outline c w  = return w
        getOutline w = return cdefault

instance SegmentedCanvasItem Line

instance Synchronized Line where
        synchronize w = synchronize (toGUIObject w)

instance HasSize Line where
        height _ w  = return w
        getHeight _ = return cdefault


-- --------------------------------------------------------------------------
-- Configuration Options
-- --------------------------------------------------------------------------

arrowstyle :: ArrowHead -> Config Line
arrowstyle d w = cset w "arrow" d

getArrowstyle :: Line -> IO ArrowHead
getArrowstyle w = cget w "arrow"

arrowshape :: ArrowShape -> Config Line
arrowshape (x,y,z) w = cset w "arrowshape" [x, y, z]

getArrowshape :: Line -> IO ArrowShape
getArrowshape w = cget w "arrowshape" >>= next 
        where next (x:y:z:_) = return (x, y, z)
              next _ = return (0,0,0)

capstyle :: CapStyle -> Config Line
capstyle d w = cset w "capstyle" d

getCapstyle :: Line -> IO CapStyle
getCapstyle w = cget w "capstyle"

joinstyle :: JoinStyle -> Config Line
joinstyle d w = cset w "joinstyle" d

getJoinstyle :: Line -> IO JoinStyle
getJoinstyle w = cget w "joinstyle"


-- --------------------------------------------------------------------------
--  ArrowHead 
-- --------------------------------------------------------------------------

data ArrowHead = BothEnds | LastEnd | FirstEnd | NoHead deriving (Eq,Ord,Enum)

instance GUIValue ArrowHead where
        cdefault = NoHead

instance Read ArrowHead where
   readsPrec p b =
     case dropWhile (isSpace) b of
        'b':'o':'t':'h':xs -> [(BothEnds,xs)]
        'l':'a':'s':'t': xs -> [(LastEnd,xs)]
        'f':'i':'r':'s':'t':xs -> [(FirstEnd,xs)]
        'n':'o':'n':'e':xs -> [(NoHead,xs)]
        _ -> []

instance Show ArrowHead where
   showsPrec d p r = 
      (case p of 
         BothEnds -> "both"
         LastEnd -> "last"
         FirstEnd -> "first"
         NoHead -> "none"
        ) ++ r

        
-- --------------------------------------------------------------------------
--  CapStyle 
-- --------------------------------------------------------------------------

data CapStyle = CapRound | CapProjecting | CapButt deriving (Eq,Ord,Enum)

instance GUIValue CapStyle where
        cdefault = CapButt

instance Read CapStyle where
   readsPrec p b =
     case dropWhile (isSpace) b of
        'r':'o':'u':'n':'d':xs -> [(CapRound,xs)]
        'p':'r':'o':'j':'e':'c':'t':'i':'n':'g': xs -> [(CapProjecting,xs)]
        'b':'u':'t':'t':xs -> [(CapButt,xs)]
        _ -> []

instance Show CapStyle where
   showsPrec d p r = 
      (case p of 
         CapRound -> "round"
         CapProjecting -> "projecting"
         CapButt -> "butt"
        ) ++ r

        
-- --------------------------------------------------------------------------
--  JoinStyle 
-- --------------------------------------------------------------------------

data JoinStyle = JoinRound | JoinMiter | JoinBevel deriving (Eq,Ord,Enum)

instance GUIValue JoinStyle where
        cdefault = JoinMiter

instance Read JoinStyle where
   readsPrec p b =
     case dropWhile (isSpace) b of
        'r':'o':'u':'n':'d':xs -> [(JoinRound,xs)]
        'm':'i':'t':'e':'r': xs -> [(JoinMiter,xs)]
        'b':'e':'v':'e':'l':xs -> [(JoinBevel,xs)]
        _ -> []

instance Show JoinStyle where
   showsPrec d p r = 
      (case p of 
         JoinRound -> "round"
         JoinMiter -> "miter"
         JoinBevel -> "bevel"
        ) ++ r




