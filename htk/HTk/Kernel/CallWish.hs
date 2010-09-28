-- | This is the bare-bones interface which actually calls the wish program.
module HTk.Kernel.CallWish(
   CalledWish, -- Type representing wish instance
   callWish, -- :: IO CalledWish
      -- callWish is used just once, to start the new wish.
   sendCalledWish, -- :: CalledWish -> CStringLen -> IO ()
      -- sendCalledWish is given the data as a CString.
   readCalledWish, -- :: CalledWish -> IO String
   destroyCalledWish, -- :: CalledWish -> IO ()
   ) where

import Foreign.C.String

import Util.WBFiles

import Events.Destructible

import Posixutil.ChildProcess

newtype CalledWish = CalledWish ChildProcess

callWish :: IO CalledWish
callWish =
   do
      wishPath <- getWishPath
      childProcess <- newChildProcess wishPath [
         linemode True,
         challengeResponse challengeResponsePair,
         toolName "wish"
         ]
      return (CalledWish childProcess)

challengeResponsePair :: (String,String)
challengeResponsePair = ("fconfigure stdout -translation lf;if {[info command button] == \"button\"} {puts \"This is wish  \"} else {puts \"Is this tclsh?\"}","This is wish  \n")

sendCalledWish :: CalledWish -> CStringLen -> IO ()
sendCalledWish (CalledWish childProcess) cStringLen =
   sendMsgRaw childProcess cStringLen

readCalledWish :: CalledWish -> IO String
readCalledWish (CalledWish childProcess) = readMsg childProcess

destroyCalledWish :: CalledWish -> IO ()
destroyCalledWish (CalledWish childProcess) = destroy childProcess



