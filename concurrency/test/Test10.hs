{- Attempt to test message queues and their interaction with poll. -}
module Main(main) where

import Selective

showContents :: Show a => MsgQueue a -> IO ()
showContents queue =
   do
      next <- poll(receive queue) 
      case next of
         Nothing -> return ()
         Just a ->
            do
               putStr (show a)
               putStr "\n"
               showContents queue

main =
   do
      queue <- (newMsgQueue :: IO (MsgQueue Int)) 
      sendIO queue 1
      sendIO queue 2
      sendIO queue 3
      showContents queue
      sendIO queue 1
      sendIO queue 2
      sendIO queue 3
      showContents queue



