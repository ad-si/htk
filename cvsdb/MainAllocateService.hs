{- This is a server program running the AllocateServer and the
   EchoServer. -}
module Main(main) where

import Server
import EchoService
import AllocateService

main = runServer [allocateServiceWrapped,echoServiceWrapped]