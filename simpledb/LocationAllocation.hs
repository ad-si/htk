-- | This module handles the allocation of 'Location'/'PrimitiveLocation's.  
module LocationAllocation(
   initLocations,
   getNextLocation,
   getRedirectLocations,
   flushLocation,
   specialLocation1,
   specialLocation2,
   ) where

import Maybe
import Monad

import Data.IORef
import Data.FiniteMap

import DeepSeq
import Computation(done)
import ExtendedPrelude(catchAllExceps)

import TopSort

import BDBOps
import BDBExtras
import ServerErrors
import SimpleDBTypes
import SecurityManagement
import VersionData
import PrimitiveLocation
import VersionInfo(ObjectVersion)

-- | Two locations, 'specialLocation1 and 'specialLocation2, are preallocated
-- and will never be returned by 'getNextLocation'.  
-- (The types package uses 'specialLocation1' for general 'ViewType.View'
-- information, and 'specialLocation2' for the top folder.)
specialLocation1,specialLocation2 :: Location
specialLocation1 = Location 0
specialLocation2 = Location 1
  
-- | Read the BDB (the miscDB) to come up with the nextLocation ref.
initLocations :: BDB -> IO (IORef Location)
initLocations bdb =
   do
      location <- getObject bdb 1
      newIORef location

-- | Allocate a Location, setting its parent location (and taking care
-- we don't introduce cycles).
getNextLocation :: SimpleDB -> Maybe (ObjectVersion,Location) -> IO Location
getNextLocation simpleDB parentOpt = 
   do
      parentPrimitiveLocationOpt <- case parentOpt of
         Nothing -> return Nothing
         Just (version,parentLocation) ->
            do
               versionData <- getVersionData simpleDB version
               let
                  parentPrimitiveLocation 
                     = retrievePrimitiveLocation versionData parentLocation
               return (Just parentPrimitiveLocation)
      getNextLocation1 simpleDB parentPrimitiveLocationOpt


getNextLocation1 :: SimpleDB -> Maybe PrimitiveLocation -> IO Location
getNextLocation1 simpleDB parentPrimitiveLocationOpt = 
   do
      location <- 
         atomicModifyIORef (nextLocation simpleDB)
            (\ location -> (succ location,location))

      let
         primitiveLocation = toPrimitiveLocation location

      -- Check that the parent location has already got security data.
      -- This is the ultimate check which prevents the creation of
      -- cycles in the parent links.
      case parentPrimitiveLocationOpt of
         Nothing -> done
         Just parentPrimitiveLocation ->
            do
               getSecurityData simpleDB parentPrimitiveLocation
               done

      let
         securityData = trivialSecurityData parentPrimitiveLocationOpt 
      setSecurityData simpleDB primitiveLocation securityData
      return location

-- | Allocate a block of locations corresponding to redirects,
-- taking care we don't introduce redirects.
-- NB.  The output list isn't necessarily in the same order as the
-- input list.
getRedirectLocations :: SimpleDB -> Maybe ObjectVersion 
   -> [(Location,Maybe Location)] -> IO [(Location,PrimitiveLocation)]
getRedirectLocations simpleDB parentVersionOpt redirects =
   do
      let
         simpleCases0 :: [Location]
         simpleCases0 = mapMaybe
            (\ (location,parentOpt) -> case parentOpt of
               Just _ -> Nothing
               Nothing -> Just location)
            redirects

      (simpleCases1 :: [(Location,PrimitiveLocation)]) <- mapM
         (\ location ->
            do
               locationOut <- getNextLocation simpleDB Nothing
               return (location,toPrimitiveLocation locationOut)
            )
         simpleCases0

      let
         complexCases0 :: [(Location,Location)]
         complexCases0 = mapMaybe
            (\ (location,parentOpt) -> case parentOpt of
               Nothing -> Nothing
               Just parent -> Just (location,parent)
               ) 
            redirects

         -- NB.  casesSorted is in reverse order, with child versions
         -- before their parents.
         casesSorted0 :: [Location]
         casesSorted0 = topSort complexCases0

      -- Check for cycles during sorting.
      checked <- catchAllExceps (deepSeq casesSorted0 done)
      case checked of
         Left errorMess -> throwError MiscError
            ("Cycle in redirect parents? " ++ errorMess)
            -- this is unfriendly but it shouldn't happen unless the workbench
            -- is bugged
         Right () -> done

      -- now do the allocation
      (oldRedirects :: Location -> IO PrimitiveLocation)
         <- case parentVersionOpt of
            Just parentVersion ->
               do
                  versionData <- getVersionData simpleDB parentVersion
                  let
                     oldRedirects location 
                        = return (retrievePrimitiveLocation versionData 
                           location)
                  return oldRedirects
            Nothing -> return (throwError MiscError
               "Commit with no parents has redirect with non-existent parent")
 


      let
         casesSorted1 :: [Location]
         casesSorted1 = reverse casesSorted0

         parentMap :: FiniteMap Location Location
         parentMap = listToFM complexCases0

         doLocations 
            :: FiniteMap Location PrimitiveLocation 
               -- map of locations allocated so far
            -> [Location]
               -- locations to do 
            -> IO [(Location,PrimitiveLocation)]
         doLocations fm0 locations = case locations of
            [] -> return (fmToList fm0)
            (location:locations1) ->
               do
                  let
                     parentLocation :: Location
                     Just parentLocation = lookupFM parentMap location

                  (parentPrimitiveLocation :: PrimitiveLocation) <- 
                     case lookupFM fm0 parentLocation of
                        Just pLocation -> return pLocation
                        Nothing -> oldRedirects parentLocation

                  locationOut <- getNextLocation1 simpleDB 
                     (Just parentPrimitiveLocation)
                  let
                     fm1 = addToFM fm0 location 
                        (toPrimitiveLocation locationOut)

                  doLocations fm1 locations1

      complexCases1 <- doLocations emptyFM casesSorted1
      return (simpleCases1 ++ complexCases1)

-- | Write the current location data to the BDB.
-- (This needs to be done during every successful commit.)
flushLocation :: SimpleDB -> TXN -> IO ()
flushLocation simpleDB txn =
   do
      nextLocation <- readIORef (nextLocation simpleDB)
      setObjectHere1 (miscDB simpleDB) 1 txn nextLocation
      
