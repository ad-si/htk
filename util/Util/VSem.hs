-- | | Implements locks which can be locked "globally" or "locally".
--   A global lock prevents any other lock; a local lock allows other local
--   locks.
--
--   There are some subtle decisions to be made about when to give preference
--   to local, and when to global, locks.  There are two important cases:
--   (1) When we free a global lock, and there is another queued global lock,
--       we take that global lock (or the earliest for which someone is
--       waiting, if there's a choice), irrespective of whether anyone is
--       waiting for a local lock.
--   (2) When at least one local lock is held, we allow people to acquire
--       further local locks, even if there are queued global locks.
--
--   A bad consequence of (2) is that a global lock can be indefinitely not
--   satisfied by a carefully-timed sequence of finite local locks:
--
--   local locks : --- --- --- --- . . .
--                   --- --- ---   . . .
--   no global lock can be acquired at all.
--
--   However the alternative, of not permitting any fresh local locks when
--   a global lock is queued, is worse (in my opinion), since if a thread
--   attempts to acquire two local locks, one inside the other, and another
--   attempts to acquire a global lock, the whole thing can deadlock.
--
--   Thread 1  : acquire local lock                    attempt to acquire second local lock => DEADLOCK.
--   Thread 2  :                   wait for global lock
--
--   We could deal with this partially by allowing local locks for free
--   to a thread which already holds one, but this is more complicated and
--   I suspect theoretically dodgy.
--
--   A consequence of this decision is that threads should avoid creating
--   automated repeated sequences of local locks on the same VSem.
module Util.VSem(
   VSem,
   newVSem,

   synchronizeLocal,
   synchronizeGlobal,

   acquireLocal, -- :: VSem -> IO ()
   releaseLocal, -- :: VSem -> IO ()
   ) where

import Control.Concurrent
import Control.Exception

import Util.Computation
import Util.Queue

data VSemState = VSemState {
   queuedGlobals :: Queue (MVar ()),
   queuedLocals :: [MVar ()],
   nLocalLocks :: Int
      -- ^ -1 if the vSem is globally locked, otherwise the number of local
      -- locks.
   }

-- | A lock which can be globally or locally locked.
-- At any time, a @VSem@ is either globally locked once, or locally locked
-- zero or more times.  Global locks always take priority over local locks.
newtype VSem = VSem (MVar VSemState)

-- | Creates a 'VSem'.
newVSem :: IO VSem
newVSem =
   do
      mVar <- newMVar (VSemState {
         queuedGlobals = emptyQ,
         queuedLocals = [],
         nLocalLocks = 0
         })
      return (VSem mVar)

-- | Perform an action while locking a 'VSem' locally.
synchronizeLocal :: VSem -> IO b -> IO b
synchronizeLocal vSem act =
   do
      acquireLocal vSem
      finally act (releaseLocal vSem)

-- | Perform an action while locking a 'VSem' globally.
synchronizeGlobal :: VSem -> IO b -> IO b
synchronizeGlobal vSem act =
   do
      acquireGlobal vSem
      finally act (releaseGlobal vSem)

vSemAct :: VSem -> (VSemState -> IO (VSemState,b)) -> IO b
vSemAct (VSem mVar) update =
   modifyMVar mVar update

-- | Acquire a local lock on a 'VSem'
acquireLocal :: VSem -> IO ()
acquireLocal vSem =
   do
      act <- vSemAct vSem (\ vSemState ->
         if nLocalLocks vSemState <0
            then
               do
                  mVar <- newEmptyMVar
                  return (vSemState {
                     queuedLocals = mVar : queuedLocals vSemState},
                     takeMVar mVar
                     )
            else
               return (vSemState {
                  nLocalLocks = nLocalLocks vSemState + 1},
                  done)
         )
      act


-- | Release a local lock on a 'VSem'
releaseLocal :: VSem -> IO ()
releaseLocal vSem =
   vSemAct vSem (\ vSemState ->
      do
         let
            nLocalLocks0 = nLocalLocks vSemState
            nLocalLocks1 = nLocalLocks0 - 1
         case (nLocalLocks1,removeQ (queuedGlobals vSemState)) of
            (0,Just (mVar,queuedGlobals1)) ->
               do
                  putMVar mVar ()
                  return (vSemState {nLocalLocks = -1,
                     queuedGlobals = queuedGlobals1
                     },())
            _ -> return (vSemState {nLocalLocks = nLocalLocks1},())
      )


-- | Acquire a global lock on a 'VSem'
acquireGlobal :: VSem -> IO ()
acquireGlobal vSem =
   do
      act <- vSemAct vSem (\ vSemState ->
         do
            let
               nLocalLocks0 = nLocalLocks vSemState
            if nLocalLocks0 == 0
               then
                  return (vSemState {nLocalLocks = -1},done)
               else
                  do
                     mVar <- newEmptyMVar
                     return (vSemState {
                        queuedGlobals
                           = insertQ (queuedGlobals vSemState) mVar},
                        takeMVar mVar
                        )
         )
      act


-- | Release a global lock on a 'VSem'
releaseGlobal :: VSem -> IO ()
releaseGlobal vSem =
   vSemAct vSem (\ vSemState ->
      case (removeQ (queuedGlobals vSemState),queuedLocals vSemState) of
         (Just (mVar,queuedGlobals1),_) ->
            do
              putMVar mVar ()
              return (vSemState {queuedGlobals = queuedGlobals1},())
         (Nothing,queuedLocals0) ->
            do
              mapM_ (\ mVar -> putMVar mVar ()) queuedLocals0
              return (vSemState {queuedLocals = [],
                 nLocalLocks = length queuedLocals0},())
      )


