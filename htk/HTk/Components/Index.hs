{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module exports basic types and classes on index positions, e.g.
-- inside an editor or entry widget.
module HTk.Components.Index (

  EndOfText(..),
  HasIndex(..),

  BaseIndex(..),
  Pixels(..),
  First(..),
  Last(..),

) where

import HTk.Kernel.Core
import HTk.Kernel.Geometry
import Util.ExtendedPrelude
import Data.Char


-- -----------------------------------------------------------------------
-- HasIndex
-- -----------------------------------------------------------------------

-- | Internal.
class HasIndex w i b where
  getBaseIndex :: w -> i -> IO b


-- -----------------------------------------------------------------------
-- Index: End of text
-- -----------------------------------------------------------------------

-- | The @EndOfText@ datatype - a handle indexing the last
-- position inside the concerned widget.
data EndOfText = EndOfText deriving Eq

-- | Internal.
instance Show EndOfText where
  showsPrec d _ r = "end" ++ r

-- | Internal.
instance Read EndOfText where
  readsPrec p str =
    case str of ('e':'n':'d':xs) -> [(EndOfText,xs)]
                _ -> []

-- | Internal.
instance GUIValue EndOfText where
  cdefault = EndOfText


-- -----------------------------------------------------------------------
-- Index: Pixels i.e. @x,y for listbox and text widgets
-- -----------------------------------------------------------------------

-- | The @Pixels@ datatype - a handle indexing a position inside
-- a widget with its coordinates.
data Pixels = Pixels Distance Distance

-- | Internal.
instance Show Pixels where
   showsPrec d (Pixels x y) r = "@" ++ show x ++ "," ++ show y ++ r


-- -----------------------------------------------------------------------
-- Index: First, for entry and text widgets
-- -----------------------------------------------------------------------

-- | The @First@ datatype - a handle indexing the first entry
-- e.g. inside a listbox.
data First = First

-- | Internal.
instance Show First where
   showsPrec d _ r = "first" ++ r


-- -----------------------------------------------------------------------
-- Index: Last, for entry and text widgets
-- -----------------------------------------------------------------------

-- | The @Last@ datatype - a handle indexing the last entry
-- e.g. inside a listbox.
data Last = Last

-- | Internal.
instance Show Last where
   showsPrec d _ r = "first" ++ r


-- -----------------------------------------------------------------------
-- BaseIndex
-- -----------------------------------------------------------------------

-- | The @BaseIndex@ datatype - an index handle specified by
-- an index number, an index position (line, char) or an index text (see
-- text marks).
data BaseIndex =
          IndexNo Int                   -- ^ entries, listboxes
        | IndexPos Position             -- ^ text widgets
        | IndexText String              -- ^ listboxes, "end" etc.

-- | Internal.
instance GUIValue BaseIndex where
  cdefault = IndexNo 0

-- | Internal.
instance Show BaseIndex where
   showsPrec d c r = cshow c ++ r where
        cshow (IndexNo i) = show i
        cshow (IndexPos (x,y)) = show x ++ "." ++ show y
        cshow (IndexText s) = s

-- | Internal.
instance Read BaseIndex where
    readsPrec p str = [(cread str,[])] where
        cread (s @ (x:l)) | isDigit x =
                case map read (simpleSplit (== '.') s) of
                        [Distance i] -> IndexNo i
                        [x,y] -> IndexPos (x,y)
                        _ -> error "illegal index specification"
        cread s = IndexText s
