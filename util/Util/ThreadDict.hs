-- | This module implements per-thread variables
module Util.ThreadDict(
   ThreadDict, -- contains all the thread variables
   newThreadDict, -- :: IO (ThreadDict a)
   writeThreadDict, -- :: ThreadDict a -> a -> IO ()
   readThreadDict, -- :: ThreadDict a -> IO (Maybe a)
   modifyThreadDict, -- :: ThreadDict a -> (Maybe a -> IO (Maybe a,b)) -> IO b
   ) where

import Control.Concurrent

import qualified Data.Map as Map
import Data.IORef

-- -------------------------------------------------------------------------
-- Data types
-- -------------------------------------------------------------------------

newtype ThreadDict a = ThreadDict (IORef (Map.Map ThreadId a))

-- -------------------------------------------------------------------------
-- Functions
-- -------------------------------------------------------------------------

newThreadDict :: IO (ThreadDict a)
newThreadDict = do
  m <- newIORef Map.empty
  return (ThreadDict m)

writeThreadDict :: ThreadDict a -> a -> IO ()
writeThreadDict (ThreadDict table) a =
   do
      ti <- myThreadId
      atomicModifyIORef table $ \ m -> (Map.insert ti a m, ())

readThreadDict :: ThreadDict a -> IO (Maybe a)
readThreadDict (ThreadDict table) =
   do
      ti <- myThreadId
      m <- readIORef table
      return $ Map.lookup ti m

modifyThreadDict :: ThreadDict a -> (Maybe a -> IO (Maybe a, b)) -> IO b
modifyThreadDict (ThreadDict table) updateFn =
   do
      ti <- myThreadId
      m <- readIORef table
      (aOpt1, b) <- updateFn $ Map.lookup ti m
      atomicModifyIORef table $ \ im -> ((case aOpt1 of
            Nothing -> Map.delete ti
            Just a -> Map.insert ti a) im, b)
