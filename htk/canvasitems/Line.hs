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

---
-- HTk's <strong>line</strong> canvas item.<br>
-- A line object on a canvas widget.
module Line (

  module CanvasItem,

  ArrowHead(..),
  CapStyle(..),
  JoinStyle(..),

  Line,
  createLine,

  arrowshape,
  getArrowshape,

  arrowstyle,
  getArrowstyle,

  capstyle,
  getCapstyle,

  joinstyle,
  getJoinstyle

) where

import Core
import Configuration
import Geometry(Distance)
import CanvasItem
import CanvasTag
import CanvasItemAux
import Char
import Destructible
import Computation
import Synchronized


-- -----------------------------------------------------------------------
-- datatype
-- -----------------------------------------------------------------------

---
-- The <code>Line</code> datatype.
newtype Line = Line GUIOBJECT deriving Eq

---
-- The <code>ArrowShape</code> datatype.<br>
-- Describes the shape of an arrow at an end of a line.
type ArrowShape = (Distance, Distance, Distance)


-- -----------------------------------------------------------------------
-- constructor
-- -----------------------------------------------------------------------

---
-- Constructs a new line item.
-- @param cnv     - the parent canvas.
-- @param cnf     - the list of configuration options for this line item.
-- @return result - A line item.
createLine :: Canvas -> [Config Line] -> IO Line
createLine cnv cnf = createCanvasItem cnv LINE Line cnf [(-1,-1),(-1,-1)]


-- -----------------------------------------------------------------------
-- Instantiations
-- -----------------------------------------------------------------------

---
-- Internal.
instance GUIObject Line where 
---
-- Internal.
  toGUIObject (Line w) = w
---
-- Internal.
  cname _ = "Line"

---
-- A line item can be destroyed.
instance Destroyable Line where
---
-- Destroys a line item.
  destroy = destroy . toGUIObject

---
-- A line item is a canvas item (any canvas item is an instance of the
-- abstract <code>class CanvasItem</code>).
instance CanvasItem Line

---
-- A line item can have several tags (handlers for a set of canvas items).
instance TaggedCanvasItem Line

---
-- A line item has filling, outline width and stipple configurations.
instance FilledCanvasItem Line where
---
-- Dummy.
  outline c w  = return w
---
-- Dummy.
  getOutline w = return cdefault

---
-- A line is a segmented canvas item. It has a splinesteps and smooth
-- configuration.
instance SegmentedCanvasItem Line

---
-- You can synchronize on a line item.
instance Synchronized Line where
---
-- Synchronizes on a line item.
  synchronize w = synchronize (toGUIObject w)

---
-- You can specify the width of the line.
instance HasSize Line where
---
-- Dummy.
  height _ w  = return w
---
-- Dummy.
  getHeight _ = return cdefault


-- -----------------------------------------------------------------------
-- configuration options
-- -----------------------------------------------------------------------

---
-- Sets the style of the arrows at the ends of a line.
arrowstyle :: ArrowHead -> Config Line
arrowstyle d w = cset w "arrow" d

---
-- Gets the style of the arrows at the ends of a line.
getArrowstyle :: Line -> IO ArrowHead
getArrowstyle w = cget w "arrow"

---
-- Sets the shape of the arrows at the ends of a line.
arrowshape :: ArrowShape -> Config Line
arrowshape (x,y,z) w = cset w "arrowshape" [x, y, z]

---
-- Gets the shape of the arrows at the end of a line.
getArrowshape :: Line -> IO ArrowShape
getArrowshape w = cget w "arrowshape" >>= next 
  where next (x:y:z:_) = return (x, y, z)
        next _ = return (0, 0, 0)

---
-- Sets the capstyle at the ends of a line (butt, projecting or round).
capstyle :: CapStyle -> Config Line
capstyle d w = cset w "capstyle" d

---
-- Gets the capstyle at the ends of a line.
getCapstyle :: Line -> IO CapStyle
getCapstyle w = cget w "capstyle"

---
-- Sets the joinstyle between the line segments (bevel, miter or round).
joinstyle :: JoinStyle -> Config Line
joinstyle d w = cset w "joinstyle" d

---
-- Gets the joinstyle between the line segments.
getJoinstyle :: Line -> IO JoinStyle
getJoinstyle w = cget w "joinstyle"


-- -----------------------------------------------------------------------
--  ArrowHead
-- -----------------------------------------------------------------------

---
-- The <code>ArrowHead</code> datatype (see <code>Line.arrowstyle</code>).
data ArrowHead =
  BothEnds | LastEnd | FirstEnd | NoHead deriving (Eq,Ord,Enum)

---
-- Internal.
instance GUIValue ArrowHead where
---
-- Internal.
  cdefault = NoHead

---
-- Internal.
instance Read ArrowHead where
---
-- Internal.
  readsPrec p b =
    case dropWhile (isSpace) b of
       'b':'o':'t':'h':xs -> [(BothEnds,xs)]
       'l':'a':'s':'t': xs -> [(LastEnd,xs)]
       'f':'i':'r':'s':'t':xs -> [(FirstEnd,xs)]
       'n':'o':'n':'e':xs -> [(NoHead,xs)]
       _ -> []

---
-- Internal.
instance Show ArrowHead where
---
-- Internal.
  showsPrec d p r = (case p of 
                       BothEnds -> "both"
                       LastEnd -> "last"
                       FirstEnd -> "first"
                       NoHead -> "none") ++ r


-- -----------------------------------------------------------------------
--  CapStyle
-- -----------------------------------------------------------------------

---
-- The <code>CapStyle</code> datatype (see <code>Line.capstyle</code>).
data CapStyle = CapRound | CapProjecting | CapButt deriving (Eq,Ord,Enum)

---
-- Internal.
instance GUIValue CapStyle where
---
-- Internal.
  cdefault = CapButt

---
-- Internal.
instance Read CapStyle where
---
-- Internal.
  readsPrec p b =
    case dropWhile (isSpace) b of
       'r':'o':'u':'n':'d':xs -> [(CapRound,xs)]
       'p':'r':'o':'j':'e':'c':'t':'i':'n':'g': xs -> [(CapProjecting,xs)]
       'b':'u':'t':'t':xs -> [(CapButt,xs)]
       _ -> []

---
-- Internal.
instance Show CapStyle where
---
-- Internal.
  showsPrec d p r = (case p of 
                       CapRound -> "round"
                       CapProjecting -> "projecting"
                       CapButt -> "butt") ++ r

        
-- -----------------------------------------------------------------------
--  JoinStyle
-- -----------------------------------------------------------------------

---
-- The <code>JoinStyle</code> datatype (see <code>Line.joinstyle</code>).
data JoinStyle = JoinRound | JoinMiter | JoinBevel deriving (Eq,Ord,Enum)

---
-- Internal.
instance GUIValue JoinStyle where
---
-- Internal.
  cdefault = JoinMiter

---
-- Internal.
instance Read JoinStyle where
---
-- Internal.
  readsPrec p b = case dropWhile (isSpace) b of
                    'r':'o':'u':'n':'d':xs -> [(JoinRound,xs)]
                    'm':'i':'t':'e':'r': xs -> [(JoinMiter,xs)]
                    'b':'e':'v':'e':'l':xs -> [(JoinBevel,xs)]
                    _ -> []

---
-- Internal.
instance Show JoinStyle where
---
-- Internal.
   showsPrec d p r = (case p of 
                        JoinRound -> "round"
                        JoinMiter -> "miter"
                        JoinBevel -> "bevel") ++ r
