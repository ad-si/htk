module Main(main) where

import Registry

import HTk

import GetAttributesType
import AttributesType
import Registrations

main :: IO ()
main =
   do
      withdrawWish

      doRegistrations
      attributesTypeOpt <- getAttributesType
      case attributesTypeOpt of
         Nothing -> putStrLn "AttributesType entry cancelled"
         Just attributesType ->
            do
               let
                  view = error "I hope this view will not be used"
               attributesOpt <- inputAttributes view attributesType Nothing
               case attributesOpt of
                  Nothing -> putStrLn "Attributes entry cancelled"
                  Just attributes ->
                     do
                        (keys :: [String]) <- listKeys attributes
                        putStrLn ("Keys read: "++show keys)
      cleanupWish

