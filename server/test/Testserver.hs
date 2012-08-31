module Main(main) where

import Server.Server
import Server.EchoService

main = runServer [echoServiceWrapped]







