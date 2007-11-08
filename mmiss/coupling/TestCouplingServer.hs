{- Simple program for testing server -}
module Main(main) where

import IO
import Maybe

import BinaryAll
import Messages
import Computation
import ICStringLen
import AtomString
import WBFiles

import Control.Concurrent
import Network

-- import MMiSSAPIBlock


main :: IO ()
main =
--      user <- textQuery "User?"
--      password <- textQuery "Password?"
   do
      parseArgumentsRequiring ["couplingPort"]
      port <- getCouplingPort
      serverOpt <- getServer
      handle <- connectTo (fromMaybe "localhost" serverOpt)
         (PortNumber (fromIntegral port))
      serverOpt <- getServer
      hSetBuffering handle LineBuffering
      hPutStrLn handle "MMiSS-SVN"
      hPutStrLn handle "test01"
      hPutStrLn handle "test01"
      hFlush handle
      ack <- hGetLine handle
      putStrLn ack
      hFlush stdout
      forkIO (display handle)
      sendIn handle

display :: Handle -> IO ()
display handle =
   do
      block <- hGetLine handle
      putStrLn block
      hFlush stdout
      display handle

sendIn :: Handle -> IO ()
sendIn  handle =
   do
      line <- getLine
      hPutStrLn handle line
      sendIn handle


{--
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
--}

{--
readString :: Handle -> IO (WithError String)
readString = hReadLtd (maxLen + 1)

writeString :: Handle -> String -> IO ()
writeString handle str = hWrite handle str

maxLen :: Int
maxLen = 127
--}

{--
      hSetBuffering handle LineBuffering
--      hWrite handle "MMiSS-SVN"
--      hWrite handle "test01"
--      hWrite handle "test01"
      hPutStrLn handle "MMiSS-SVN"
      hPutStrLn handle "test01"
      hPutStrLn handle "test01"
      ack <- hGetLine handle
      putStrLn ack
      hFlush stdout
      hPutStrLn handle "Root/AlgSpec"
      hPutStrLn handle "Hallo/Test"
      forkIO (display handle)
      sendIn handle


display :: Handle -> IO ()
display handle =
   do
      block <- hGetLine handle
--      let
--         (Just (BlockData {blockType = 0,blockText = icsl}))
--            = lookupBlockData block 0
--      putStr (toString icsl)
      putStrLn block
      hFlush stdout
      display handle

sendIn :: Handle -> IO ()
sendIn  handle =
   do
      line <- getLine
--      let
--         (block,0) = addBlockData emptyBlock
--            (BlockData {blockType = 0,blockText = fromString line})
--      writeBlock handle block
      hPutStrLn handle line
      hFlush handle
      sendIn handle

--}
