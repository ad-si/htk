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
-- Basic types and classes for coloured resources.
module Colour (

  ColourDesignator(..),
  Colour(..)      

) where

import GUIValue
import Char
import Debug(debug)


-- -----------------------------------------------------------------------
-- Colour Designator
-- -----------------------------------------------------------------------

---
-- Datatypes that describe a colour instantiate the
-- <code>class ColourDesignator</code>.
class ColourDesignator c where
  toColour :: c -> Colour


-- -----------------------------------------------------------------------
-- instances 
-- -----------------------------------------------------------------------

---
-- A colour itself describes a colour.
instance ColourDesignator Colour where
---
-- Internal.
  toColour = id

---
-- Strings like "red", "blue" etc. decribe colours.
instance ColourDesignator [Char] where
---
-- Internal.
  toColour = Colour

---
-- A tuple of rgb values describes a colour.
instance ColourDesignator (Int,Int,Int) where
---
-- Internal.
  toColour (r,g,b) = Colour (rgb r g b)

---
-- A tuple of rgb values describes a colour.
instance ColourDesignator (Double,Double,Double) where
---
-- Internal.
  toColour (r,g,b) = Colour (rgb (iround r) (iround g) (iround b))
                     where iround :: Double -> Int
                           iround x = round x


-- -----------------------------------------------------------------------
-- datatype
-- -----------------------------------------------------------------------

---
-- The <code>Colour</code> datatype.
newtype Colour = Colour String 

---
-- Internal.
instance GUIValue Colour where
---
-- Internal.
  cdefault = Colour "grey"
        
---
-- Internal.
instance Read Colour where
---
-- Internal.
   readsPrec p b =
     case dropWhile (isSpace) b of
        xs -> [(Colour (takeWhile (/= ' ') xs),"")]

---
-- Internal.
instance Show Colour where
---
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
