{- This module contains the code which actually processes and
   distributes XML requests. -}
module MMiSSDoXml(
   doXml, -- :: Handle -> IO ()
   ) where

import IO

import PasswordFile

doXml :: Handle -> User -> IO ()
doXml handle user =
   do
      hPutStrLn handle "Not implemented yet!"
      hClose handle