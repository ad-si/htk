{- This module implements a server that just provides SimpleDB. -}
module Main(main) where

import Server
import SimpleDBService

main = runServer [simpleDBServiceWrapped]
