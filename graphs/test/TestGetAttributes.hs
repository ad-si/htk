{- TestGetAttributes tests GetAttributes.hs -}
module Main(main) where

import Registry

import InfoBus

import GetAttributes

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
      shutdown