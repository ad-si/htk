-- | This module contains functions for converting to and from the UTF8
-- representations for Strings.
module Util.UTF8(
   toUTF8,
      -- :: String -> String
      -- Converts a String (whose characters must all have codes <2^31) into
      -- its UTF8 representation.
   fromUTF8WE,
      -- :: Monad m => String -> m String
      -- Converts a UTF8 representation of a String back into the String,
      -- catching all possible format errors.
      --
      -- Example: With the Haskell module Control.Monad.Error, you can
      -- instance this as
      -- (fromUTF8WE :: String -> Either String String)
      -- to get a conversion function which either succeeds (Right) or
      -- returns an error message (Left).
   ) where

import Data.Char
import Data.List

import Data.Bits
import Data.Word
import Control.Monad.Except () -- needed for instance Monad (Either String)

import Util.Computation

-- --------------------------------------------------------------------------
-- Encoding
-- --------------------------------------------------------------------------

-- | Converts a String into its UTF8 representation.
toUTF8 :: Enum byte => String -> [byte]
toUTF8 [] = []
toUTF8 (x:xs) =
   let
      xs1 = toUTF8 xs
      ox = ord x

      mkUTF8 x0 xs0 xmask0 xmax0 =
         let
            xbot = 0x80 .|. (x0 .&. 0x3f)
            x1 = x0 `shiftR` 6
            xs1 = toEnum xbot : xs0
         in
            if x1 < xmax0
              then
                 toEnum (xmask0 .|. x1) : xs1
              else
                 let
                    xmask1 = xmask0 .|. xmax0
                    xmax1 = xmax0 `shiftR` 1
                 in
                    mkUTF8 x1 xs1 xmask1 xmax1
   in
      if ox <= 0x7f
         then
            toEnum ox : xs1
         else
           if ox `shiftR` 31 /= 0
              then
                 error ("Huge character with code " ++ show ox ++
                    " detected in string being converted to UTF8.")
              else
                 mkUTF8 ox xs1 0xc0 0x20

{-# SPECIALIZE toUTF8 :: String -> [Char] #-}
{-# SPECIALIZE toUTF8 :: String -> [Word8] #-}

-- | Converts a UTF8 representation of a String back into the String,
-- catching all possible format errors.
--
-- Example: With the Haskell module Control.Monad.Error, you can
-- instance this as
-- (fromUTF8WE :: String -> Either String String)
-- to get a conversion function which either succeeds (Right) or
-- returns an error message (Left).
fromUTF8WE :: (Enum byte,Monad m) => [byte] -> m String
fromUTF8WE [] = return []
fromUTF8WE (x0 : xs0) =
   let
      ox = fromEnum x0
   in
      case topZero8 ox of
         7 ->
            do
               xs1 <- fromUTF8WE xs0
               return (chr ox : xs1)
         6 ->
            fail "UTF8 escape sequence starts 10xxxxxx"
         0 ->
            fail "UTF8 escape sequence starts 11111110"
         -1 ->
            fail "UTF8 escape sequence starts 11111111"
         n ->
            let
               r = 6 - n -- number of 6-bit pieces
               xtop = ox .&. ones n

               minx =
                  bit (
                     if r == 1
                        then
                           7
                        else
                           5*r + 1
                     )

               mkx [] _ _ =
                  fail "UTF8 string ends in middle of escape sequence"
               mkx (ch : xs1) x0 count0 =
                  do
                     let
                        och = fromEnum ch
                     if och .&. 0x80 /= 0x80
                        then
                           fail ("UTF8 escape sequence contains continuing "
                              ++ "character not of form 10xxxxxx")
                        else
                           return ()
                     let
                        xbot = och .&. 0x3f
                        x1 = (x0 `shiftL` 6) .|. xbot
                        count1 = count0 - 1
                     if count1 == 0
                        then
                           return (x1,xs1)
                        else
                           mkx xs1 x1 count1
            in
               do
                  (x,xs1) <- mkx xs0 xtop r
                  if x < minx
                     then
                        fail ("UTF8 escape sequence contains character not "
                           ++ "optimally encoded")
                     else
                        do
                           xs2 <- fromUTF8WE xs1
                           return (toEnum x : xs2)

{-# SPECIALIZE fromUTF8WE :: String -> WithError String #-}
{-# SPECIALIZE fromUTF8WE :: [Word8] -> WithError String #-}
{-# SPECIALIZE fromUTF8WE :: String -> Either String String #-}
{-# SPECIALIZE fromUTF8WE :: [Word8] -> Either String String #-}


-- --------------------------------------------------------------------------
-- Binary utilities
-- --------------------------------------------------------------------------

-- | return the number of the top bit which is zero, or -1 if they
-- are all zero, for a number between 0 and 255.
topZero8 :: Int -> Int
topZero8 i =
   case
      (findIndex not
         (map
            (\ bn -> testBit i bn)
            [7,6..0]
            ))
      of
         Just n -> 7 - n
         Nothing -> -1

-- | (ones i) is number with binary representation 1 written i times.
ones :: Int -> Int
ones i = bit i - 1
