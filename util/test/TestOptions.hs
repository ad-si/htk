{- Test WBFiles.hs -}
module Main(main) where

import Util.WBFiles


testGet :: Show a => String -> IO a -> IO ()
testGet name action =
   do
      value <- action
      putStrLn (name++" "++(show value))

main =
   do
      testGet "wishPath" getWishPath
      testGet "daVinciPath" getDaVinciPath
      testGet "top" getTOP
      testGet "port" getPort
      testGet "daVinciIcons" getDaVinciIcons
      testGet "server" getServer
      testGet "gnuclient" getGnuClientPath
      testGet "MMiSS DTD" getMMiSSDTD
