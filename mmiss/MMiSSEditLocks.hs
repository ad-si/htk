{- The functions in this module are used for monitoring the edit locks
   on objects. 

   Each acquireXXXX function attempts to acquire the locks for the
   given LockLocations.  If unsuccessful, nothing is done; if successful
   an action is returned to release all the locks; also is returned a LockSet.

   The LockSet contains a set of currently acquired locks.  This can be
   given again to the acquireMultipleLocks function, which will then succeed
   even if the LockLocations it is given correspond to an edit lock already
   locked, provided all such edit locks are in the LockSet.
   -}
module MMiSSEditLocks(
   acquireLock,
      -- :: View -> LockLocation -> IO (WithError (IO (),LockSet))
   acquireMultipleLocks,
      -- :: View -> LockSet -> [LockLocation] -> IO (WithError (IO (),LockSet))

   LockLocation, 
      -- alias for (MMiSSObject,MMiSSVariantSpec)

   LockSet,
   emptyLockSet, -- :: LockSet
   ) where

import Maybe
import List

import Computation

import BSem

import LinkManager
import View

import MMiSSVariant
import MMiSSVariantObject
import MMiSSObjectType

-- -------------------------------------------------------------------------
-- Datatypes
-- -------------------------------------------------------------------------

newtype LockSet = LockSet [BSem]

type LockLocation = (MMiSSObject,MMiSSVariantSpec)

-- -------------------------------------------------------------------------
-- Functions
-- -------------------------------------------------------------------------

acquireLock :: View -> LockLocation -> IO (WithError (IO (),LockSet))
acquireLock view lockLocation =
   acquireMultipleLocks view emptyLockSet [lockLocation]

acquireMultipleLocks 
   :: View -> LockSet -> [LockLocation] -> IO (WithError (IO (),LockSet))
acquireMultipleLocks view (LockSet bSems0) lockLocations =
   do
      (lockLocations2Opt :: [Maybe LockLocation2])
         <- mapM 
            (\ (object,variantSpec) ->
                do
                   variableOpt <- lookupVariantObjectExact
                      (variantObject object) variantSpec
                   return (fmap
                      (\ variable -> (editLock variable,object,variantSpec))
                      variableOpt
                      )
                )  
            lockLocations

      let
         lockLocations2 :: [LockLocation2]
         lockLocations2 = catMaybes lockLocations2Opt

         getBSem :: LockLocation2 -> IO BSem 
         getBSem (bSem,_,_) = return bSem

         getMessIfError :: LockLocation2 -> IO (Maybe String)
         getMessIfError (bSem,object,variantSpec) =
            case find (== bSem) bSems0 of
               Just _ -> return Nothing
               Nothing ->
                  do
                     fullName <- getFullName view object
                     return (Just (
                        fullName
                        ++ " (" ++ show variantSpec ++ ") "
                        ++ "is already being edited"))

      
      releaseActWE 
         <- tryAcquireBSemsWithError1 getBSem getMessIfError lockLocations2

      let
         lockSet1 
            = LockSet (bSems0 ++ (map (\ (bSem,_,_) -> bSem) lockLocations2))

      return (fmap (\ releaseAct -> (releaseAct,lockSet1)) releaseActWE)

type LockLocation2 = (BSem,MMiSSObject,MMiSSVariantSpec)

emptyLockSet :: LockSet
emptyLockSet = LockSet []
