{- This is the bare-bones interface which actually calls the wish program.
   NB - we separate this out, because for Windows it will have to be implemented
   in a different way. 

   NB.  The Windows interface relies on callWish only ever being used once in
   a program, because it relies on storing crucial information in static data.
   -}
module CallWish(
   CalledWish, -- Type representing wish instance
   callWish, -- :: IO CalledWish
   sendCalledWish, -- :: CalledWish -> CStringLen -> IO ()
      -- sendCalledWish is given the data as a CString.
   readCalledWish, -- :: CalledWish -> IO String
   destroyCalledWish, -- :: CalledWish -> IO ()
   ) where

import CString

import WBFiles

import Destructible

import ChildProcess

newtype CalledWish = CalledWish ChildProcess

callWish :: IO CalledWish
callWish = 
   do
      wishPath <- getWishPath
      childProcess <- newChildProcess wishPath [linemode True]
      return (CalledWish childProcess)

sendCalledWish :: CalledWish -> CStringLen -> IO ()
sendCalledWish (CalledWish childProcess) cStringLen =
   sendMsgRaw childProcess cStringLen

readCalledWish :: CalledWish -> IO String
readCalledWish (CalledWish childProcess) = readMsg childProcess

destroyCalledWish :: CalledWish -> IO ()
destroyCalledWish (CalledWish childProcess) = destroy childProcess
   