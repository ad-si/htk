module Main where

import AtomString
import CodedValue
import ICStringLen
import ExtendedPrelude

main :: IO ()
main = 
   do
      let
         icsl :: ICStringLen
         icsl = fromString "foo"

         s :: String
         s = "bar"

      icsl2 <- testb icsl
      b <- eqIO icsl icsl2
      putStrLn (show b)
      testb [(icsl,s)]
      testb [(s,icsl)]
      testb ([] :: [(String,ICStringLen)])
      return ()
         

testb :: (HasCodedValue a) => a -> IO a
testb a1 =
   do
      b <- doEncodeIO a1 (error "vv")
      doDecodeIO b (error "vv2")
