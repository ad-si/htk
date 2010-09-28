module Main(main) where

import IO

import Server.Server
import Server.EchoService

main = runServer [echoServiceWrapped]







