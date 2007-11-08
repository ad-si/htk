-- Test module.  Designed to be used from ghci.
module SimpleDBServerTest where

import SimpleDBServer

getActs =
   do
      s <- initialiseSimpleDB

      let
         q = querySimpleDB s
         n =
            do
               (IsLocation l) <- q NewLocation
               return l
         c s l =
            do
               (IsObjectVersion v) <- q (Commit s l Nothing)
               return v
         r l v =
            do
               (IsContents s) <- q (Retrieve l v)
               return s
         lv l =
            do
               (IsObjectVersions vs) <- q (ListVersions l)
               return vs
      return (backupSimpleDB s,n,c,r,lv)




