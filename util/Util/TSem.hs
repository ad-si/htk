-- | A TSem is an unusual sort of lock in that it only protects the same thread
-- from acquiring it twice.  Different threads may acquire the same TSem
-- without problems.
--
-- The purpose of this is to allow computations which potentially would
-- loop forever by calling themselves to instead fail gracefully.  To
-- aid in this process, we also include in each TSem a String.  When we
-- attempt to acquire a TSem which is already acquired, we instead return
-- the String for this TSem and the TSems acquired within this one.
module Util.TSem(
   TSem,
   newTSem, -- :: IO String -> IO TSem
   synchronizeTSem, -- TSem -> IO a -> IO (Either [String] a)
   ) where

import System.IO.Unsafe
import Control.Exception

import Util.Object
import Util.ExtendedPrelude

import Util.ThreadDict

-- ---------------------------------------------------------------------------
-- Datatypes
-- ---------------------------------------------------------------------------

data TSem = TSem {
   oId :: ObjectID,
   label :: IO String
   }

newtype ThreadInfo = ThreadInfo [TSem]
   -- Information we keep per thread.

-- ---------------------------------------------------------------------------
-- The global dictionary of ThreadInfo
-- ---------------------------------------------------------------------------

threadInfoDict :: ThreadDict ThreadInfo
threadInfoDict = unsafePerformIO newThreadDict
{-# NOINLINE threadInfoDict #-}

-- ---------------------------------------------------------------------------
-- The functions
-- ---------------------------------------------------------------------------

newTSem :: IO String -> IO TSem
newTSem label =
   do
      oId <- newObject
      return (TSem {oId = oId,label = label})

synchronizeTSem :: TSem -> IO a -> IO (Either [String] a)
synchronizeTSem tSem act =
   do
      strActsOpt <- tryAcquire tSem
      case strActsOpt of
         Nothing -> finally
            (do
                a <- act
                return (Right a)
            )
            (release tSem)
         Just strActs ->
            do
               strs <- mapM id strActs
               return (Left strs)

tryAcquire :: TSem -> IO (Maybe [IO String])
-- if unsuccessful return the labels of all TSems held by this thread.
tryAcquire tSem =
   modifyThreadDict threadInfoDict
      (\ threadInfoOpt ->
         return (case threadInfoOpt of
            Nothing -> (Just (ThreadInfo [tSem]),Nothing)
            Just (ThreadInfo tSems) ->
               case splitToElem (\ tSem2 -> oId tSem2 == oId tSem) tSems
                     of
                  Nothing -> -- not already locked
                     (Just (ThreadInfo (tSem : tSems)),Nothing)
                  Just (tSems,_) -> -- already locked
                     (threadInfoOpt,Just (map label (tSem : reverse tSems)))
            )
         )

release :: TSem -> IO ()
release tSem =
   modifyThreadDict threadInfoDict
      (\ threadInfoOpt ->
         case threadInfoOpt of
            (Just (ThreadInfo (tSem2 : tSems)))
               | oId tSem2 == oId tSem
               ->
               return (
                  case tSems of
                     [] -> Nothing
                     _ -> Just (ThreadInfo tSems)
                  , ())
            _ -> error "TSem -- improperly nested release"
         )
