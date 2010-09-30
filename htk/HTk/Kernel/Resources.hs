-- | Basic resources used with object configuration options.
module HTk.Kernel.Resources (

  State(..),
  Justify(..),
  Relief(..),
  Anchor(..),
  Toggle(..),
  toggle,
  Orientation(..),
  Alignment(..),
  Flexibility(..),

  CreationConfig,
  showCreationConfigs

) where

import HTk.Kernel.GUIValue
import Data.Char


-- -----------------------------------------------------------------------
--  creation configs
-- -----------------------------------------------------------------------

-- | Internal.
type CreationConfig w = IO String

-- | Internal.
showCreationConfigs :: [CreationConfig a] -> IO String
showCreationConfigs (c : cs) =
  do
    str <- c
    rest <- showCreationConfigs cs
    return ("-" ++ str ++ " " ++ rest)
showCreationConfigs _ = return ""


-- -----------------------------------------------------------------------
-- state
-- -----------------------------------------------------------------------

-- | The @State@ datatype - the state of certain widgets
-- can be normal, disabled or active.
data State = Disabled | Active | Normal deriving (Eq,Ord,Enum)

-- | Internal.
instance GUIValue State where
  -- Internal.
  cdefault = Disabled

-- | Internal.
instance Read State where
   -- Internal.
   readsPrec p b =
     case dropWhile (isSpace) b of
        'd':'i':'s':'a':'b':'l':'e':'d': xs -> [(Disabled,xs)]
        'a':'c':'t':'i':'v':'e': xs -> [(Active,xs)]
        'n':'o':'r':'m':'a':'l': xs -> [(Normal,xs)]
        _ -> []

-- | Internal.
instance Show State where
   -- Internal.
   showsPrec d p r =
      (case p of
          Disabled -> "disabled"
          Active -> "active"
          Normal -> "normal"
        ) ++ r


-- -----------------------------------------------------------------------
-- Justify
-- -----------------------------------------------------------------------

-- | The @Justify@ datatype - representing a text justification.
data Justify = JustLeft | JustCenter | JustRight deriving (Eq,Ord,Enum)


-- | Internal.
instance GUIValue Justify where
  -- Internal.
  cdefault = JustLeft

-- | Internal.
instance Read Justify where
   -- Internal.
   readsPrec p b =
     case dropWhile (isSpace) b of
        'l':'e':'f':'t':xs -> [(JustLeft,xs)]
        'c':'e':'n':'t':'e':'r':xs -> [(JustCenter,xs)]
        'r':'i':'g':'h':'t':xs -> [(JustRight,xs)]
        _ -> []

-- | Internal.
instance Show Justify where
  -- Internal.
  showsPrec d p r =
    (case p of
       JustLeft -> "left"
       JustCenter -> "center"
       JustRight -> "right") ++ r


-- -----------------------------------------------------------------------
-- relief
-- -----------------------------------------------------------------------

-- | The @Relief@ datatype - represents the relief of certain
-- widgets.
data Relief =
  Groove | Ridge | Flat | Sunken | Raised deriving (Eq,Ord,Enum)

-- | Internal.
instance GUIValue Relief where
  -- Internal.
  cdefault = Flat

-- | Internal.
instance Read Relief where
   -- Internal.
   readsPrec p b =
     case dropWhile (isSpace) b of
        'g':'r':'o':'o':'v':'e':xs -> [(Groove,xs)]
        'r':'i':'d':'g':'e':xs -> [(Ridge,xs)]
        'f':'l':'a':'t':xs -> [(Flat,xs)]
        's':'u':'n':'k':'e':'n':xs -> [(Sunken,xs)]
        'r':'a':'i':'s':'e':'d':xs -> [(Raised,xs)]
        _ -> []

-- | Internal.
instance Show Relief where
  -- Internal.
  showsPrec d p r =
    (case p of
       Groove -> "groove"
       Ridge -> "ridge"
       Flat -> "flat"
       Sunken -> "sunken"
       Raised -> "raised") ++ r


-- -----------------------------------------------------------------------
-- Orientation
-- -----------------------------------------------------------------------

-- | The @Orientation@ datatype - used for different purposes.
data Orientation = Horizontal | Vertical deriving (Eq,Ord,Enum)

-- | Internal.
instance GUIValue Orientation where
  -- Internal.
  cdefault = Horizontal

-- | Internal.
instance Read Orientation where
  -- Internal.
  readsPrec p b =
    case dropWhile (isSpace) b of
      'h':'o':'r':'i':'z':'o':'n':'t':'a':'l':xs -> [(Horizontal,xs)]
      'v':'e':'r':'t':'i':'c':'a':'l':xs -> [(Vertical,xs)]
      _ -> []

-- | Internal.
instance Show Orientation where
  -- Internal.
  showsPrec d p r =
    (case p of
       Horizontal -> "horizontal"
       Vertical -> "vertical") ++ r


-- -----------------------------------------------------------------------
-- Toggle
-- -----------------------------------------------------------------------

-- | A simple @Toggle@ datatype - used for different purposes.
data Toggle = Off | On deriving (Eq,Ord)

-- | Internal.
instance GUIValue Toggle where
  -- Internal.
  cdefault = Off

-- | Internal.
instance Read Toggle where
  -- Internal.
  readsPrec p b =
    case dropWhile (isSpace) b of
      '0':xs -> [(Off,xs)]
      '1':xs -> [(On,xs)]
      _ -> []

-- | Internal.
instance Show Toggle where
  -- Internal.
  showsPrec d p r =
    (case p of
       Off -> "0"
       On -> "1") ++ r

toggle :: Toggle -> Toggle
toggle On = Off
toggle Off = On


-- -----------------------------------------------------------------------
-- Flexibility
-- -----------------------------------------------------------------------

-- | The @Flexibility@ datatype - used in the context of boxes
-- (see containers).
data Flexibility = Rigid | Flexible


-- -----------------------------------------------------------------------
-- Alignment
-- -----------------------------------------------------------------------

-- | The @Alignment@ datatype - widget alignment etc.
data Alignment = Top | InCenter | Bottom | Baseline deriving (Eq,Ord,Enum)

-- | Internal.
instance GUIValue Alignment where
  -- Internal.
  cdefault = Top

-- | Internal.
instance Read Alignment where
  -- Internal.
  readsPrec p b =
    case dropWhile (isSpace) b of
      'c':'e':'n':'t':'e':'r':xs -> [(InCenter,xs)]
      't':'o':'p': xs -> [(Top,xs)]
      'b':'o':'t':'t':'o':'m':xs -> [(Bottom,xs)]
      'b':'a':'s':'e':'l':'i':'n':'e':xs -> [(Baseline,xs)]
      _ -> []

-- | Internal.
instance Show Alignment where
  -- Internal.
  showsPrec d p r =
    (case p of
       Top -> "top"
       InCenter -> "center"
       Bottom -> "bottom"
       Baseline -> "baseline") ++ r


-- -----------------------------------------------------------------------
-- Anchor
-- -----------------------------------------------------------------------

-- | The @Anchor@ datatype - used for different purposes, e.g.
-- text anchors or anchor positions of canvas items.
data Anchor =
          SouthEast
        | South
        | SouthWest
        | East
        | Center
        | West
        | NorthEast
        | North
        | NorthWest
        deriving (Eq,Ord,Enum)

-- | Internal.
instance GUIValue Anchor where
  -- Internal.
  cdefault = Center

-- | Internal.
instance Read Anchor where
   -- Internal.
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

-- | Internal.
instance Show Anchor where
   -- Internal.
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
