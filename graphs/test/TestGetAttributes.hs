{- TestGetAttributes tests GetAttributes.hs -}
module Main(main) where

import InfoBus

import GetAttributes

main =
   do
      nodeTypeAttributes <- getNodeTypeAttributes
      putStrLn (show nodeTypeAttributes)
      shutdown