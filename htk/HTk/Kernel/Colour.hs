{-# LANGUAGE FlexibleInstances #-}

-- | Basic types and classes for coloured resources.
module HTk.Kernel.Colour (

  ColourDesignator(..),
  Colour(..)

) where

import HTk.Kernel.GUIValue
import Data.Char


-- -----------------------------------------------------------------------
-- Colour Designator
-- -----------------------------------------------------------------------

-- | Datatypes that describe a colour instantiate the
-- @class ColourDesignator@.
class ColourDesignator c where
  toColour :: c -> Colour


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

-- | A colour itself describes a colour.
instance ColourDesignator Colour where
  -- Internal.
  toColour = id

-- | Strings like \"red\", \"blue\" etc. decribe colours.
instance ColourDesignator [Char] where
  -- Internal.
  toColour = Colour

-- | A tuple of rgb values describes a colour.
instance ColourDesignator (Int,Int,Int) where
  -- Internal.
  toColour (r,g,b) = Colour (rgb r g b)

-- | A tuple of rgb values describes a colour.
instance ColourDesignator (Double,Double,Double) where
  -- Internal.
  toColour (r,g,b) = Colour (rgb (iround r) (iround g) (iround b))
                     where iround :: Double -> Int
                           iround x = round x


-- -----------------------------------------------------------------------
-- datatype
-- -----------------------------------------------------------------------

-- | The @Colour@ datatype.
newtype Colour = Colour String

-- | Internal.
instance GUIValue Colour where
  -- Internal.
  cdefault = Colour "grey"

-- | Internal.
instance Read Colour where
   -- Internal.
   readsPrec p b =
     case dropWhile (isSpace) b of
        xs -> [(Colour (takeWhile (/= ' ') xs),"")]

-- | Internal.
instance Show Colour where
   -- Internal.
   showsPrec d (Colour p) r = p ++ r


-- -----------------------------------------------------------------------
-- Colour Codes
-- -----------------------------------------------------------------------

rgb :: Int -> Int -> Int -> String
rgb r g b = "#" ++ concat (map (hex 2 "") [r,g,b]) where
  hex 0 rs _ = rs
  hex t rs 0 = hex (t-1) ('0':rs) 0
  hex t rs i = let m = mod i 16
               in hex (t-1)((chr (48+m+7*(div m 10))):rs)(div i 16)

{- this function is borrowed from the implementation of tkGofer -}
