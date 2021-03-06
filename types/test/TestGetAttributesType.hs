module Main(main) where

import Util.Registry

import HTk.Toplevel.HTk

import Types.GetAttributesType
import Types.AttributesType
import Types.Registrations

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

