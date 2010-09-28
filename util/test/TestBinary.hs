{- Module containing test functions for Binary utilites. -}
module Main where

import Data.Word

import Util.Bytes
import Util.Binary
import Util.BinaryInstances

main =
   do
      testb ()
      testb 'c'
      testb '\500'
      testb (1 :: Int)
      testb (0 :: Int)
      testb (125 :: Int)
      testb (130 :: Int)
      testb (900 :: Int)
      testb (99999999999999999999992345413451324214 :: Integer)
      testb (-3333 :: Int)
      testb (-1 :: Int)
      testb (-55555555555555555555556666 :: Integer)
      testb (1 :: Word)
      testb (0 :: Word)
      testb (125 :: Word)
      testb (130 :: Word)
      testb (900 :: Word)
      testb "foobarbaz\0"
      testb ""
      testb ([99,3,-3,333] :: [Int])
      testb (Left 3 :: Either Int Bool)
      testb (Right False :: Either Int Bool)
      testb (Right True :: Either Int Bool)

testb :: (Show a,Eq a,HasBinary a StateBinArea) => a -> IO ()
testb a =
   do
      (bl @ (b,l)) <- writeToBytes a
      a2 <- readFromBytes bl
      bytesFree b

      if (a == a2)
         then
            return ()
         else
            do
               putStrLn ("Match error: " ++ show a ++ "," ++ show a2)
