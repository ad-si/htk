{- Test WBFiles.hs -}
module Main(main) where

import WBFiles


testGet :: Show a => String -> IO a -> IO ()
testGet name action =
   do
      value <- action
      putStrLn (name++" "++(show value))

main = 
   do
      testGet "wishPath" getWishPath
      testGet "cvsPath" getCVSPath
      testGet "daVinciPath" getDaVinciPath
      testGet "top" getTOP
      testGet "port" getPort
      testGet "daVinciIcons" getDaVinciIcons
      testGet "CVSROOT" getCVSROOT
      testGet "backupDir" getBackupDir
      testGet "server" getServer