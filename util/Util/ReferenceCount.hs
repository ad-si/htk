-- | A simple reference counter
module Util.ReferenceCount(
   RefCount,

   newRefCount, -- :: IO RefCount
      -- new ref count with no links.
   newLinkedRefCount, -- :: IO RefCount
      -- new ref count with one link (that being what is normally wanted).
   addRef, -- :: RefCount -> IO ()
   remRef, -- :: RefCount -> IO Bool
      -- returns True if we reach 0.
   ) where

import Control.Concurrent.MVar

newtype RefCount = RefCount (MVar Int)


newRefCount :: IO RefCount
newRefCount =
   do
      mVar <- newMVar 0
      return (RefCount mVar)

newLinkedRefCount :: IO RefCount
newLinkedRefCount =
   do
      mVar <- newMVar 1
      return (RefCount mVar)

addRef :: RefCount -> IO ()
addRef (RefCount mVar) = modifyMVar_ mVar (return . (+1))

remRef :: RefCount -> IO Bool
remRef (RefCount mVar) = modifyMVar mVar (\ count0 ->
   let
      count1 = count0 - 1
   in
      return (count1,count1 == 0)
   )
