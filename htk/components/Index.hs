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

module Index (

  EndOfText(..),
  HasIndex(..),

  BaseIndex(..),
  Pixels(..),
  First(..),
  Last(..),

) where

import Core
import Geometry
import ExtendedPrelude
import Char


-- -----------------------------------------------------------------------
-- HasIndex
-- -----------------------------------------------------------------------

class HasIndex w i b where
  getBaseIndex :: w -> i -> IO b


-- -----------------------------------------------------------------------
-- Index: End of text
-- -----------------------------------------------------------------------

data EndOfText = EndOfText deriving Eq

instance Show EndOfText where
  showsPrec d _ r = "end" ++ r 

instance Read EndOfText where
  readsPrec p str = 
    case str of ('e':'n':'d':xs) -> [(EndOfText,xs)]
                _ -> []

instance GUIValue EndOfText where
  cdefault = EndOfText


-- -----------------------------------------------------------------------
-- Index: Pixels i.e. @x,y for listbox and text widgets
-- -----------------------------------------------------------------------

data Pixels = Pixels Distance Distance

instance Show Pixels where
   showsPrec d (Pixels x y) r = "@" ++ show x ++ "," ++ show y ++ r
        

-- -----------------------------------------------------------------------
-- Index: First, for entry and text widgets
-- -----------------------------------------------------------------------

data First = First 

instance Show First where
   showsPrec d _ r = "first" ++ r
        

-- -----------------------------------------------------------------------
-- Index: Last, for entry and text widgets
-- -----------------------------------------------------------------------

data Last = Last 

instance Show Last where
   showsPrec d _ r = "first" ++ r
        

-- -----------------------------------------------------------------------
-- BaseIndex
-- -----------------------------------------------------------------------

data BaseIndex =
          IndexNo Int                   -- entries
        | IndexPos Position             -- text widgets
        | IndexText String              -- listbox'es, "end" etc.

instance GUIValue BaseIndex where
        cdefault = IndexNo 0

instance Show BaseIndex where
   showsPrec d c r = cshow c ++ r where
        cshow (IndexNo i) = show i
        cshow (IndexPos (x,y)) = show x ++ "." ++ show y
        cshow (IndexText s) = s

instance Read BaseIndex where
    readsPrec p str = [(cread str,[])] where
        cread (s @ (x:l)) | isDigit x =
                case map read (split (== '.') s) of
                        [Distance i] -> IndexNo i
                        [x,y] -> IndexPos (x,y)
                        _ -> error "illegal index specification"
        cread s = IndexText s
