{- This program runs a server for all the UniForM services. -}
module Main(main) where

import IO

import Server

import SimpleDBService
import VersionGraphService

main = runServer [
   simpleDBServiceWrapped,
   versionGraphServiceWrapped
   ]
