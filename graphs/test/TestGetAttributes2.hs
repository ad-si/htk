{- TestGetAttributes2 tests GetAttributes.hs.
   This is like TestGetAttributes except we do it over and over again,
   and force GHC to garbage-collect each time.  The purpose of which
   is to see whether HTk is causing the "Blocked on Dead MVar" messages
   I'm getting rather a lot of now . . . -}
module Main(main) where

import Util.Registry

import System.Mem

import Graphs.GetAttributes

main :: IO ()
main =
   do
      get1round
      performGC
      main

get1round :: IO ()
get1round =
   do
      nodeTypeRegistry <- newRegistry

      nodeAttributes1 <- getNodeAttributes nodeTypeRegistry
      putStrLn (show (nodeAttributes1 :: (Maybe (NodeAttributes String))))

      nodeTypeAttributes <- getNodeTypeAttributes
      putStrLn (show (nodeTypeAttributes :: (Maybe (NodeTypeAttributes ()))))

      setValue nodeTypeRegistry "Foo" "foo type"
      setValue nodeTypeRegistry "Bah" "bah type"

      nodeAttributes2 <- getNodeAttributes nodeTypeRegistry
      putStrLn (show (nodeAttributes2 :: (Maybe (NodeAttributes String))))

      arcTypeAttributes <- getArcTypeAttributes
      putStrLn (show (arcTypeAttributes :: (Maybe (ArcTypeAttributes))))

      arcTypeRegistry <- newRegistry

      setValue arcTypeRegistry "Woo" "woo type"
      setValue arcTypeRegistry "Waz" "waz type"

      arcAttributes1 <- getArcAttributes arcTypeRegistry
      putStrLn (show (arcAttributes1 :: (Maybe (ArcAttributes String))))



