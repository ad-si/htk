-- | This module handles the allocation of 'Location' or 'PrimitiveLocation's. 
module LocationAllocation(
   initLocations,
   getNextLocation,
   flushLocation,
   specialLocation1,
   specialLocation2,

   isOpenLocation,
   closeLocations,
   forgetUsersLocations,
   ) where

import Monad
import Maybe

import Data.IORef
import DeprecatedFiniteMap

import Computation(done)

import PasswordFile(User)

import BDBOps
import BDBExtras
import SimpleDBTypes
import SecurityManagement
import PrimitiveLocation

-- | Two locations, 'specialLocation1 and 'specialLocation2, are preallocated
-- and will never be returned by 'getNextLocation'.  
-- (The types package uses 'specialLocation1' for general 'ViewType.View'
-- information, and 'specialLocation2' for the top folder.)
specialLocation1,specialLocation2 :: Location
specialLocation1 = Location 1
specialLocation2 = Location 2
  
-- | Read the BDB (the miscDB) to come up with the nextLocation ref.
initLocations :: BDB -> IO (IORef Location)
initLocations bdb =
   do
      location <- getObject bdb 2
      newIORef location

-- | Allocate a Location.
getNextLocation :: SimpleDB -> User -> IO Location
getNextLocation simpleDB user = 
   do
      location <- atomicModifyIORef (nextLocation simpleDB)
         (\ location -> (succ location,location))
      let
         pLocation = toPrimitiveLocation location

      openLocation simpleDB user pLocation

      setPermissions1 simpleDB (toPrimitiveLocation location) []
      return location

-- | Write the current location data to the BDB.
-- (This needs to be done during every successful commit.)
flushLocation :: SimpleDB -> TXN -> IO ()
flushLocation simpleDB txn =
   do
      nextLocation <- readIORef (nextLocation simpleDB)
      setObjectHere1 (miscDB simpleDB) 2 txn nextLocation

-- ----------------------------------------------------------------------
-- Functions for managing the open locations
-- ----------------------------------------------------------------------

openLocation :: SimpleDB -> User -> PrimitiveLocation -> IO ()
openLocation simpleDB user primitiveLocation =
   atomicModifyIORef (openLocations simpleDB)
      (\ fm0 -> (addToFM fm0 primitiveLocation user,()))

-- | Show if a location is open and belongs to the user.
isOpenLocation :: SimpleDB -> User -> PrimitiveLocation -> IO Bool
isOpenLocation simpleDB user primitiveLocation =
   do
      fm <- readIORef (openLocations simpleDB)
      return (lookupFM fm primitiveLocation == Just user)

-- | Mark a location as committed
closeLocations :: SimpleDB -> User -> [PrimitiveLocation] -> IO ()
closeLocations simpleDB user pLocations0 =
   atomicModifyIORef (openLocations simpleDB)
      (\ fm0 ->
         let
            pLocations1 :: [PrimitiveLocation]
            pLocations1 = filter
               (\ pLocation -> lookupFM fm0 pLocation == Just user)
               pLocations0
         in
            (delListFromFM fm0 pLocations1,())
         )

-- | Forget all locations belonging to a user and free their memory.
forgetUsersLocations :: SimpleDB -> User -> IO ()
forgetUsersLocations simpleDB user0 =
   do
      l <- atomicModifyIORef (openLocations simpleDB)
         (\ fm0 ->
            let
               old :: [(PrimitiveLocation,User)]
               old = fmToList fm0
              
               toDelete :: [PrimitiveLocation]
               toDelete = mapMaybe
                  (\ (ov,user1) -> 
                     if user1 == user0
                        then
                           Just ov
                        else
                           Nothing
                     )
                  old

               fm1 = foldl
                  (\ fm ov -> delFromFM fm ov)
                  fm0
                  toDelete
            in
               (fm1,sizeFM fm1)
            )
      
      seq l done


