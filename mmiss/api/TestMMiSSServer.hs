{- Simple program for testing server -}
module Main(main) where

import IO

import BinaryAll
import Messages
import Computation
import ICStringLen
import AtomString
import WBFiles

import Control.Concurrent
import Network

import MMiSSAPIBlock


main :: IO ()
main = 
   do
      user <- textQuery "User?"
      password <- textQuery "Password?"
      port <- getXMLPort
      handle <- connectTo "localhost" (PortNumber (fromIntegral port))
      hWrite handle "MMiSS-XML"
      hWrite handle user
      hWrite handle password
      hFlush handle
      ack <- hRead handle
      putStrLn ack
      forkIO (display handle)
      sendIn handle
{-
      forever (
         do
            request <- getLine
            hWrite handle request
            hFlush handle
            response <- hRead handle
            putStrLn response
         )
   -}

display :: Handle -> IO ()
display handle =
   do
      block <- readBlock handle
      let
         (Just (BlockData {blockType = 0,blockText = icsl})) 
            = lookupBlockData block 0
      putStr (toString icsl)
      hFlush stdout
      display handle

sendIn :: Handle -> IO ()
sendIn  handle =
   do
      line <- getLine
      let
         (block,0) = addBlockData emptyBlock 
            (BlockData {blockType = 0,blockText = fromString line})
      writeBlock handle block
      hFlush handle
      sendIn handle
 


