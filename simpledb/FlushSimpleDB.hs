-- | This module flushes SimpleDB, so that the information is
-- stored to disk.  Thus it should be used at the end of a commit.
module FlushSimpleDB(
   flushSimpleDB,
   ) where

import Monad

import SimpleDBTypes
import BDBOps
import LocationAllocation
import VersionAllocation

flushSimpleDB :: SimpleDB -> TXN -> IO ()
flushSimpleDB simpleDB txn =
   do
      flushLocation simpleDB txn
      flushVersion simpleDB txn
      endTransaction txn
      mapM_ (\ dbField -> flushBDB (dbField simpleDB))
         [miscDB,versionDB,securityDB,keyDB,dataDB]
