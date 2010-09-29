{- This program runs a server for all the UniForM services. -}
module Main(main) where

import Server.Server

import SimpleDB.Service

main :: IO ()
main =
   do
      services <- mkSimpleDBServices
      runServer services
