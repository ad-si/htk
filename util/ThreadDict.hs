-- | This module implements per-thread variables 
module ThreadDict(
   ThreadDict, -- contains all the thread variables
   newThreadDict, -- :: IO (ThreadDict a)
   writeThreadDict, -- :: ThreadDict a -> a -> IO ()
   readThreadDict, -- :: ThreadDict a -> IO (Maybe a)
   modifyThreadDict, -- :: ThreadDict a -> (Maybe a -> IO (Maybe a,b)) -> IO b
   ) where

import Data.HashTable
import Control.Concurrent

import Thread

-- -------------------------------------------------------------------------
-- Data types
-- -------------------------------------------------------------------------

newtype ThreadDict a = ThreadDict (HashTable ThreadId a)

-- -------------------------------------------------------------------------
-- Functions
-- -------------------------------------------------------------------------

newThreadDict :: IO (ThreadDict a)
newThreadDict = 
   do
      table <- new (==) hashThreadId
      return (ThreadDict table)

writeThreadDict :: ThreadDict a -> a -> IO ()
writeThreadDict (ThreadDict table) a =
   do
      ti <- myThreadId
      insert table ti a

readThreadDict :: ThreadDict a -> IO (Maybe a)
readThreadDict (ThreadDict table) =
   do
      ti <- myThreadId
      Data.HashTable.lookup table ti

modifyThreadDict :: ThreadDict a -> (Maybe a -> IO (Maybe a,b)) -> IO b
modifyThreadDict (ThreadDict table) updateFn =
   do
      ti <- myThreadId
      aOpt0 <- Data.HashTable.lookup table ti
      (aOpt1,b) <- updateFn aOpt0
      case aOpt1 of
         Nothing -> delete table ti
         Just a -> insert table ti a
      return b