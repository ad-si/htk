-- Test module.  Designed to be used from ghci.
-- This goes via the server (SimpleDBServer) which should be running.
module SimpleDBServerTest2 where

import SimpleDB

getActs =
   do
      repository <- initialise
      
      let
         n = newLocation repository
         c s l = 
            do
               objectSource <- importString s
               commit repository objectSource l Nothing
         r l v = retrieveString repository l v
         lv l = listVersions repository l
      return (n,c,r,lv)

     
               

