{- FileEV contains an encapsulation of text IO in which
   reading from the file is done by an event.
   -}
module FileEV(
   HandleEV,
   readFileEV, -- :: HandleEV -> EV String
   writeFileEV,  -- :: HandleEV -> String -> IO ()
   closeFileEV,  -- :: HandleEV -> IO ()
   makeFileEV  -- :: Handle -> IO HandleEV
   ) where

import IO
import Concurrent
-- import Posix
-- import PosixUtil

import Computation
import Debug(debug,(@:))

import Thread
import Channels
import EV

data HandleEV = HandleEV (EV String) Handle ThreadID
-- The first argument is the source of data from the file.
-- The last argument is the id of the thread converting input
-- to a channel.

readFileEV :: HandleEV -> EV String
readFileEV (HandleEV ev _ _ ) = ev

writeFileEV :: HandleEV -> String -> IO ()
writeFileEV (HandleEV _ handle _ ) str = hPutStr handle (str++"\n")

closeFileEV :: HandleEV -> IO ()
closeFileEV (HandleEV _ handle threadId) =
   do
      hClose handle
      killThread threadId

makeFileEV :: Handle -> IO HandleEV
makeFileEV handle =
   do
      readChannel <- newChannel  -- read from handle
      let
         toForkRead = -- convert output from the Handle
            do
               line <- hGetLine handle
               sendIO readChannel line
               toForkRead

      readThread <- forkIO (goesQuietly toForkRead)

      return (HandleEV (receive readChannel) handle readThread)

               


