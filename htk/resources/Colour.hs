{- #########################################################################

MODULE        : Colour
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Colour specifications


   ######################################################################### -}


module Colour (
        ColourDesignator(..),
        Colour(..)      
        ) where

import GUIValue
import Char
import Debug(debug)

-- --------------------------------------------------------------------------
--  Colour Designator 
-- --------------------------------------------------------------------------

class ColourDesignator c where
        toColour :: c -> Colour


-- --------------------------------------------------------------------------
--  Instances 
-- --------------------------------------------------------------------------

instance ColourDesignator Colour where
        toColour = id

instance ColourDesignator [Char] where
        toColour = Colour

instance ColourDesignator (Int,Int,Int) where
        toColour (r,g,b) = Colour (rgb r g b)

instance ColourDesignator (Double,Double,Double) where
        toColour (r,g,b) = Colour (rgb (iround r) (iround g) (iround b))
                                where iround :: Double -> Int
                                      iround x = round x


-- --------------------------------------------------------------------------
--  Colour 
-- --------------------------------------------------------------------------

newtype Colour = Colour String 

instance GUIValue Colour where
        cdefault = Colour "grey"
        
instance Read Colour where
   readsPrec p b =
     case dropWhile (isSpace) b of
        xs -> [(Colour (takeWhile (/= ' ') xs),"")]

instance Show Colour where
   showsPrec d (Colour p) r = p ++ r


-- --------------------------------------------------------------------------
--  Colour Codes
-- --------------------------------------------------------------------------

rgb :: Int -> Int -> Int -> String
rgb r g b = "#" ++ concat (map (hex 2 "") [r,g,b]) where
  hex 0 rs _ = rs
  hex t rs 0 = hex (t-1) ('0':rs) 0
  hex t rs i = let m = mod i 16
               in hex (t-1)((chr (48+m+7*(div m 10))):rs)(div i 16)

{- this function is borrowed from the implementation of tkGofer -}
