-- | This module handles the allocation of 'Location'/'PrimitiveLocation's.  
module LocationAllocation(
   initLocations,
   getNextLocation,
   flushLocation,
   specialLocation1,
   specialLocation2,
   ) where

import Data.IORef

import BDBOps
import BDBExtras
import SimpleDBTypes

-- | Two locations, 'specialLocation1 and 'specialLocation2, are preallocated
-- and will never be returned by 'getNextLocation'.  
-- (The types package uses 'specialLocation1' for general 'ViewType.View'
-- information, and 'specialLocation2' for the top folder.)
specialLocation1,specialLocation2 :: Location
specialLocation1 = Location 0
specialLocation2 = Location 1
  
-- | Read the BDB (the miscDB) to come up with the nextLocation ref.
initLocations :: BDB -> IO (IORef Location)
initLocations bdb =X
   do
      location <- getObject bdb 1
      newIORef location

-- | Allocate a Location.
getNextLocation :: SimpleDB -> IO Location
getNextLocation simpleDB = 
   atomicModifyIORef (nextLocation simpleDB)
      (\ location -> (succ location,location))

-- | Write the current location data to the BDB.
-- (This needs to be done during every successful commit.)
flushLocation :: SimpleDB -> TXN -> IO ()
flushLocation simpleDB txn =
   do
      nextLocation <- readIORef (nextLocation simpleDB)
      setObjectHere1 (miscDB simpleDB) 1 txn nextLocation
      
