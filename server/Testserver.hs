module Main(main) where

import IO

import Server
import EchoService

main = runServer (11393::Int) [echoServiceWrapped]
      







