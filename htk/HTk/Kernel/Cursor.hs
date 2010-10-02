{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Basic types and classes associated with the mouse cursor.
module HTk.Kernel.Cursor (

  CursorDesignator(..),

  Cursor(..),
  XCursor(..),
  BCursor(..),

  arrow,
  circle,
  clock,
  diamondCross,
  dot,
  drapedBox,
  exchange,
  fleur,
  gobbler,
  gumby,
  hand1,
  hand2,
  pencil,
  plus,
  spraycan,
  tcross,
  watch,
  xterm

) where

import HTk.Kernel.GUIValue
import HTk.Kernel.Colour
import Data.Char

-- -----------------------------------------------------------------------
-- Cursor Type
-- -----------------------------------------------------------------------

-- | The general @Cursor@ datatype.
newtype Cursor = Cursor String

-- | The @XCursor@ dataype for predefined X cursors.
data XCursor = XCursor String (Maybe Colour) (Maybe Colour)

-- | The @BCursor@ datatype for bitmap cursors.
data BCursor = BCursor String (Maybe String) Colour (Maybe Colour)


-- -----------------------------------------------------------------------
-- Cursor Handle
-- -----------------------------------------------------------------------

-- | Datatypes that describe cursors instantiate the
-- @class CursorDesignator@.
class CursorDesignator ch where
  toCursor :: ch -> Cursor

-- | A @Cursor@ object itself describes a cursor.
instance CursorDesignator Cursor where
  -- Internal.
  toCursor = id

-- | An @XCursor@ object describes a cursor (see type).
instance  CursorDesignator XCursor where
  -- Internal.
  toCursor = Cursor . show

-- | A @BCursor@ object describes a cursor (see type).
instance CursorDesignator BCursor where
  -- Internal.
  toCursor = Cursor . show

-- | A @String@ describes a standard X cursor.
instance CursorDesignator String where
  -- Internal.
  toCursor nm = toCursor (XCursor nm Nothing Nothing)

-- | A tuple of @(String,Colour)@ describes a coloured standard
-- X cursor.
instance CursorDesignator (String,Colour) where
  -- Internal.
  toCursor (nm,fg) = toCursor (XCursor nm (Just fg) Nothing)

-- | A tuple of @(String,Colour,Colour)@ describes a standard
-- X cursor with foreground and background colour.
instance CursorDesignator (String,Colour,Colour) where
  -- Internal.
  toCursor (nm,fg,bg) =  toCursor (XCursor nm (Just fg) (Just bg))

-- | A tuple of @(String,String,Colour,Colour)@ describes a
-- bitmap cursor with its X bitmap filename, mask filename, foreground
-- and background colour.
instance CursorDesignator ([Char],[Char],Colour,Colour) where
  -- Internal.
  toCursor (bfile,mfile,fg,bg) =
    toCursor (BCursor bfile (Just mfile) fg (Just bg))


-- -----------------------------------------------------------------------
-- Standard X Cursors
-- -----------------------------------------------------------------------

-- | A standard X cursor.
arrow :: Cursor
arrow = Cursor "arrow"

-- | A standard X cursor.
circle :: Cursor
circle = Cursor "circle"

-- | A standard X cursor.
clock :: Cursor
clock = Cursor "clock"

-- | A standard X cursor.
diamondCross :: Cursor
diamondCross = Cursor "diamondcross"

-- | A standard X cursor.
dot :: Cursor
dot = Cursor "dot"

-- | A standard X cursor.
drapedBox :: Cursor
drapedBox = Cursor "drapedbox"

-- | A standard X cursor.
exchange :: Cursor
exchange = Cursor "exchange"

-- | A standard X cursor.
fleur :: Cursor
fleur = Cursor "fleur"

-- | A standard X cursor.
gobbler :: Cursor
gobbler = Cursor "gobbler"

-- | A standard X cursor.
gumby :: Cursor
gumby = Cursor "gumby"

-- | A standard X cursor.
hand1 :: Cursor
hand1 = Cursor "hand1"

-- | A standard X cursor.
hand2 :: Cursor
hand2 = Cursor "hand2"

-- | A standard X cursor.
pencil :: Cursor
pencil = Cursor "pencil"

-- | A standard X cursor.
plus :: Cursor
plus = Cursor "plus"

-- | A standard X cursor.
spraycan :: Cursor
spraycan = Cursor "spraycan"

-- | A standard X cursor.
tcross :: Cursor
tcross = Cursor "tcross"

-- | A standard X cursor.
watch :: Cursor
watch = Cursor "watch"

-- | A standard X cursor.
xterm :: Cursor
xterm = Cursor "xterm"


-- -----------------------------------------------------------------------
-- Parsing/Unparsing
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIValue Cursor where
  -- Internal.
  cdefault = Cursor "xterm"

-- | Internal.
instance Read Cursor where
   -- Internal.
   readsPrec p b =
     case dropWhile (isSpace) b of
        ('{':xs) -> [(Cursor ("{" ++ (takeWhile (/= '}') xs) ++ "}"),"")]
        xs     -> [(Cursor (takeWhile (/= ' ') xs),"")]

-- | Internal.
instance Show Cursor where
   -- Internal.
   showsPrec d (Cursor p) r = p ++ r


-- -----------------------------------------------------------------------
-- XCursor
-- -----------------------------------------------------------------------

-- | Internal.
instance Show XCursor where
   -- Internal.
   showsPrec d c r = cshow c ++ r
     where
        cshow (XCursor s Nothing Nothing) = s
        cshow (XCursor s (Just fg) Nothing) =
                "{" ++ s ++ " " ++ show fg ++ "}"
        cshow (XCursor s (Just fg) (Just bg)) =
                "{" ++ s ++ " " ++ show fg ++ " " ++ show bg ++ "}"


-- -----------------------------------------------------------------------
-- BCursor
-- -----------------------------------------------------------------------

-- | Internal.
instance Show BCursor where
   -- Internal.
   showsPrec d c r = cshow c ++ r
     where
        cshow (BCursor fname Nothing fg Nothing) =
                "{@" ++ fname ++ " " ++ show fg ++ "}"
        cshow (BCursor fname (Just bname) fg (Just bg)) =
                "{" ++ fname ++ " " ++ bname ++ " " ++ show fg ++
                " " ++ show bg ++ "}"
