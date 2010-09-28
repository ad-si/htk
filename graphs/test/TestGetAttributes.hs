{- TestGetAttributes tests GetAttributes.hs -}
module Main(main) where

import Util.Registry

import Reactor.InfoBus

import Graphs.GetAttributes

main =
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

      shutdown


