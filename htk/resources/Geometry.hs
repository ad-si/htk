{- #########################################################################

MODULE        : Geometry
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Geometry specifications 


   ######################################################################### -}

module Geometry (
        Distance(..),

        Size, 
        Coord, 
        Position,
        Geometry, 

        cm, pp, mm, ic, tocm, toinch    
        ) where

import GUIValue
import Char

import GlaExts -- needed now from fromInt, sigh.
import Debug(debug)

-- --------------------------------------------------------------------------
--  Position/Size
-- --------------------------------------------------------------------------

type Position = (Distance, Distance) 

type Size = (Distance, Distance) 

data Point = Point (Distance, Distance) 

instance GUIValue (Distance,Distance) where
        cdefault = (cdefault,cdefault)
        toGUIValue v  = GUIVALUE HaskellTk (show (Point v))
        maybeGUIValue (GUIVALUE _ s)     = 
                case [x | (Point x,t) <- reads s, ("","") <- lex t] of
                        [x] -> Just x
                        _   -> Nothing  

instance Read Point where
   readsPrec p b = 
        case (readsPrec p b) of
                [(x,xs)] -> (case (readsPrec p xs) of
                                [(y,ys)] -> [(Point (x,y),ys)]
                                _        -> []
                            )
                _        -> []

        
instance Show Point where
   showsPrec d (Point (x,y)) r = show x ++ " " ++ show y ++  r


-- --------------------------------------------------------------------------
--  Geometry
-- --------------------------------------------------------------------------

type Geometry = (Distance, Distance, Distance, Distance)

data Geometry' = Geometry' Geometry

instance GUIValue (Distance, Distance, Distance, Distance) where
        cdefault = (cdefault, cdefault, cdefault, cdefault)
        toGUIValue v  = GUIVALUE HaskellTk (show (Geometry' v))
        maybeGUIValue (GUIVALUE _ s)     = 
                case [x | (Geometry' x,t) <- reads s, ("","") <- lex t] of
                        [x] -> Just x
                        _   -> Nothing  

instance Show Geometry' where
   showsPrec d (Geometry' (w, h, x, y)) r =             
        show w ++ "x" ++ show h ++ "+" ++ show x ++ "+" ++ show y ++ r

instance Read Geometry' where
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
        

-- --------------------------------------------------------------------------
--  Coordinates
-- --------------------------------------------------------------------------

type Coord = [Position]

data Coord' = Coord' Coord

instance GUIValue [(Distance,Distance)] where
        cdefault = []
        toGUIValue v  = GUIVALUE HaskellTk (show (Coord' v))
        maybeGUIValue (GUIVALUE _ s)     = 
                case [x | (Coord' x,t) <- reads s, ("","") <- lex t] of
                        [x] -> Just x
                        _   -> Nothing  

instance Show Coord' where
   showsPrec d (Coord' []) r = 
        r
   showsPrec d (Coord' (x:l)) r = 
        show (toGUIValue x) ++ " " ++ showsPrec d (Coord' l) r


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


-- --------------------------------------------------------------------------
--  Distance 
-- --------------------------------------------------------------------------

newtype Distance = Distance Int deriving (Eq, Ord)


instance Show Distance where
   showsPrec d (Distance i) r = showsPrec d i r

instance Read Distance where
   readsPrec p b =
     case (readsPrec p b) of
        [(i,xs)] -> [(Distance (round (i::Double)),xs)]
        _ -> []


instance GUIValue Distance where
        cdefault = Distance 0

instance Enum Distance where 
	fromEnum (Distance d)= d
	toEnum d = Distance d

instance Num Distance where
        (Distance p1) + (Distance p2) = Distance (p1 + p2)
        (Distance p1) * (Distance p2) = Distance (p1 + p2)
        negate (Distance p) = Distance (negate p)
        abs (Distance p) = Distance (abs p)
        signum (Distance p) = Distance (signum p)
        fromInteger i = Distance (fromInteger i)

instance Real Distance where
	toRational (Distance i) = toRational i

instance Integral Distance where
	toInteger (Distance i) = toInteger i
	(Distance d1) `quotRem` (Distance d2) = (Distance q, Distance d) 
		where (q, d)= d1 `quotRem` d2		

-- --------------------------------------------------------------------------
--  Distance List
-- --------------------------------------------------------------------------

data Distances = Distances [Distance]
 
instance GUIValue [Distance] where
        cdefault = []
        toGUIValue v  = GUIVALUE HaskellTk (show (Distances v))
        maybeGUIValue (GUIVALUE _ s)     = 
                case [x | (Distances x,t) <- reads s, ("","") <- lex t] of
                        [x] -> Just x
                        _   -> Nothing  

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


-- --------------------------------------------------------------------------
--  Conversion 
-- --------------------------------------------------------------------------

cm :: Double -> Distance
pp :: Double -> Distance
mm :: Double -> Distance
ic :: Double -> Distance

tocm :: Distance -> Double

cm c = (Distance . round) (c * 35.4)    -- TBD: exact figures
pp i = ic (i / 72)
mm i = cm (i / 10)
ic i = (Distance . round) (i * 90.0)

tocm (Distance p) = (fromInt p) / 35.4  
toinch (Distance p) = (fromInt p) / 90.0
