{-# LANGUAGE FlexibleInstances #-}

-- | The @module Geometry@ exports basic geometric types and
-- functionality.
module HTk.Kernel.Geometry (

  Distance(..),

  Size,
  Coord,
  Position,
  Geometry,

  cm, pp, mm, ic, tocm, toinch

) where

import HTk.Kernel.GUIValue
import Data.Char


-- -----------------------------------------------------------------------
-- Position/Size
-- -----------------------------------------------------------------------

-- | The @Position@ - a pair of two @Distance@ values.
type Position = (Distance, Distance)

-- | The @Size@ datatype - a pair of two @Distance@
-- values.
type Size = (Distance, Distance)

-- | The @Point@ datatype.
data Point = Point (Distance, Distance)

-- | Internal.
instance GUIValue (Distance,Distance) where
  -- Internal.
  cdefault = (cdefault,cdefault)
  -- Internal.
  toGUIValue v  = GUIVALUE HaskellTk (show (Point v))
  -- Internal.
  maybeGUIValue (GUIVALUE _ s)     =
    case [x | (Point x,t) <- reads s, ("","") <- lex t] of
      [x] -> Just x
      _  -> Nothing

-- | Internal.
instance Read Point where
   -- Internal.
   readsPrec p b =
        case (readsPrec p b) of
                [(x,xs)] -> (case (readsPrec p xs) of
                                [(y,ys)] -> [(Point (x,y),ys)]
                                _        -> []
                            )
                _        -> []

-- | Internal.
instance Show Point where
   showsPrec d (Point (x,y)) r = show x ++ " " ++ show y ++  r


-- -----------------------------------------------------------------------
-- Geometry
-- -----------------------------------------------------------------------

-- | The Geometry datatype - normally representing position, width and
-- height.
type Geometry = (Distance, Distance, Distance, Distance)

data Geometry' = Geometry' Geometry

-- | Internal.
instance GUIValue (Distance, Distance, Distance, Distance) where
  cdefault = (cdefault, cdefault, cdefault, cdefault)
  toGUIValue v = GUIVALUE HaskellTk (show (Geometry' v))
  maybeGUIValue (GUIVALUE _ s) =
    case [x | (Geometry' x,t) <- reads s, ("","") <- lex t] of
      [x] -> Just x
      _ -> Nothing

-- | Internal.
instance Show Geometry' where
   -- Internal.
   showsPrec d (Geometry' (w, h, x, y)) r =
        show w ++ "x" ++ show h ++ "+" ++ show x ++ "+" ++ show y ++ r

-- | Internal.
instance Read Geometry' where
   -- Internal.
   readsPrec p str =
         case readsPrec p str of
                [(w,s')] -> readsPrecX1 p s' w
                _        -> []
    where
        readsPrecX1 p s w =
                case (dropWhile isSpace s) of
                   ('x':s') -> readsPrecH p s' w
                   s'       -> readsPrecH p s' w
        readsPrecH p s w =
                case  readsPrec p s of
                   [(h,s')] -> readsPrecP1 p s' w h
                   _        -> []
        readsPrecP1 p s w h =
                case (dropWhile isSpace s) of
                   ('+':s') -> readsPrecX p s' w h
                   s'       -> readsPrecX p s' w h
        readsPrecX p s w h =
                case  readsPrec p s of
                   [(x,s')] -> readsPrecP2 p s' w h x
                   _        -> []
        readsPrecP2 p s w h x =
                case (dropWhile isSpace s) of
                   ('+':s') -> readsPrecY p s' w h x
                   s'       -> readsPrecY p s' w h x
        readsPrecY p s w h x =
                case  readsPrec p s of
                   [(y,s')] -> [(Geometry' (w,h,x,y),s')]
                   _        -> []


-- -----------------------------------------------------------------------
-- Coordinates
-- -----------------------------------------------------------------------

-- | The @Coord@ datatype - e.g. representing the coords of
-- a canvas item.
type Coord = [Position]

-- | Internal.
data Coord' = Coord' Coord

-- | Internal.
instance GUIValue [(Distance,Distance)] where
  cdefault = []
  toGUIValue v = GUIVALUE HaskellTk (show (Coord' v))
  maybeGUIValue (GUIVALUE _ s) =
    case [x | (Coord' x,t) <- reads s, ("","") <- lex t] of
      [x] -> Just x
      _ -> Nothing

-- | Internal.
instance Show Coord' where
   -- Internal.
   showsPrec d (Coord' []) r =
        r
   showsPrec d (Coord' (x:l)) r =
        show (toGUIValue x) ++ " " ++ showsPrec d (Coord' l) r

-- | Internal.
instance Read Coord' where
  readsPrec p s =
        case (dropWhile isSpace s) of
                [] -> [(Coord' [],[])]
                s' -> readsPrecElem p s'
   where
        readsPrecElem p s =
                case (readsPrec p s) of
                        [(Point pos,s')] -> readsPrecTail p s' pos
                        _          -> []
        readsPrecTail p s pos =
                case (readsPrec p s) of
                        [(Coord' l,s')] -> [(Coord' (pos:l),s')]
                        _        -> []


-- -----------------------------------------------------------------------
-- Distance
-- -----------------------------------------------------------------------

-- | The @Distance@ datatype - general representation of
-- distances in HTk.
newtype Distance = Distance Int deriving (Eq, Ord)

-- | Internal.
instance Show Distance where
   -- Internal.
   showsPrec d (Distance i) r = showsPrec d i r

-- | Internal.
instance Read Distance where
   -- Internal.
   readsPrec p b =
     case (readsPrec p b) of
        [(i,xs)] -> [(Distance (round (i::Double)),xs)]
        _ -> []

-- | Internal.
instance GUIValue Distance where
  cdefault = Distance (-100)

-- | Internal.
instance Enum Distance where
  fromEnum (Distance d)= d
  toEnum d = Distance d

-- | Internal.
instance Num Distance where
  (Distance p1) + (Distance p2) = Distance (p1 + p2)
  (Distance p1) * (Distance p2) = Distance (p1 * p2)
  negate (Distance p) = Distance (negate p)
  abs (Distance p) = Distance (abs p)
  -- Internal.
  signum (Distance p) = Distance (signum p)
  -- Internal.
  fromInteger i = Distance (fromInteger i)

-- | Internal.
instance Real Distance where
  -- Internal.
  toRational (Distance i) = toRational i

-- | Internal.
instance Integral Distance where
  -- Internal.
  toInteger (Distance i) = toInteger i
  -- Internal.
  (Distance d1) `quotRem` (Distance d2) = (Distance q, Distance d)
    where (q, d)= d1 `quotRem` d2

-- -----------------------------------------------------------------------
-- Distance List
-- -----------------------------------------------------------------------

data Distances = Distances [Distance]

-- | Internal.
instance GUIValue [Distance] where
  -- Internal.
  cdefault = []
  -- Internal.
  toGUIValue v  = GUIVALUE HaskellTk (show (Distances v))
  -- Internal.
  maybeGUIValue (GUIVALUE _ s) =
    case [x | (Distances x,t) <- reads s, ("","") <- lex t] of
      [x] -> Just x
      _ -> Nothing

instance Show Distances where
   showsPrec d (Distances []) r =
        r
   showsPrec d (Distances (x:l)) r =
        show x ++ " " ++ showsPrec d (Distances l) r

instance Read Distances where
   readsPrec p s =
        case (dropWhile isSpace s) of
                [] -> [(Distances [],[])]
                s' -> readsPrecElem p s'
    where
        readsPrecElem p s =
                case (readsPrec p s) of
                        [(d,s')] -> readsPrecTail p s' d
                        _          -> []
        readsPrecTail p s d =
                case (readsPrec p s) of
                        [(Distances l,s')] -> [(Distances (d:l),s')]
                        _        -> []


-- -----------------------------------------------------------------------
-- Conversion
-- -----------------------------------------------------------------------

-- | Conversion from cm to @Distance@.
cm :: Double -> Distance
cm c = (Distance . round) (c * 35.4)

-- | Conversion from points to @Distance@.
pp :: Double -> Distance
pp i = ic (i / 72)

-- | Conversion from mm to @Distance@.
mm :: Double -> Distance
mm i = cm (i / 10)

-- | Conversion from inch to @Distance@.
ic :: Double -> Distance
ic i = (Distance . round) (i * 90.0)

-- | Conversion from @Distance@ to cm.
tocm :: Distance -> Double
tocm (Distance p) = (fromIntegral p) / 35.4

-- | Conversion from @Distance@ to inch.
toinch :: Distance -> Double
toinch (Distance p) = (fromIntegral p) / 90.0
