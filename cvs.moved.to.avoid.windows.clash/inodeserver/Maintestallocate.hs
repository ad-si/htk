{- testallocate is a tiny telnet-like program for testing the
   allocator -}
module Main(main) where

import IO

import BSD
import SocketEV
import Selective
import Debug(debug)

main =
   do
      host <- BSD.getHostName
      handle <- connect host (11394::Int)
      let
         echo =
            do
               toSend <- getLine
               hPutStrLn handle toSend
               result <- hGetLine handle
               putStr (result ++ "\n")
               debug(toSend,result)
               echo
      echo
