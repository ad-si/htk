{- Simple program for testing server -}
module Main(main) where

import IO

import BinaryAll
import Messages

import Control.Concurrent
import Network


main :: IO ()
main = 
   do
      user <- textQuery "User?"
      password <- textQuery "Password?"
      handle <- connectTo "localhost" (PortNumber 11396)
      writeString handle "MMiSS-XML"
      writeString handle user
      writeString handle password
      hFlush handle
      forkIO (display handle)
      sendIn handle

display :: Handle -> IO ()
display handle =
   do
      line <- hGetLine handle
      putStr(line++"\n")
      display handle

sendIn :: Handle -> IO ()
sendIn  handle =
   do
      line <- getLine 
      hPutStrLn handle line
      hFlush handle
      sendIn handle
 


-- ----------------------------------------------------------------------------
-- Functions for writing Strings during login.
-- (stolen from MainMMiSSServer)
-- ----------------------------------------------------------------------------


writeString :: Handle -> String -> IO ()
writeString handle str =
   let
      str2 =
         if length str > maxLen
            then 
               (take (maxLen - 3) str) ++ "..."
            else
               str
   in
      hWrite handle str2 

maxLen :: Int
maxLen = 127