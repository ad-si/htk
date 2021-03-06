-- | This module contains code for manipulating the 'PrimitiveLocation'.
-- Each 'Location' in a version has a corresponding 'PrimitiveLocation'.
-- 'PrimitiveLocations' contain an Integer, just like 'Locations', and
-- the Integer in a 'Location' will be exactly the same
-- as that for the corresponding 'PrimitiveLocation', *except* for
-- versions copied from another server.  In those cases, the locations
-- on the original server are preserved, but get mapped to new
-- 'PrimitiveLocations'.  The map is indicated by the "redirects"
-- field in 'VersionData', which lists all cases where the integers are not
-- the same.
--
-- The 'PrimitiveLocation', in turns, maps to a 'BDBKey', which indexes
-- this version's version of an object.  This map is given by the
-- 'objectDictionary' part of 'VersionData'.
module SimpleDB.PrimitiveLocation(
   locationsSame,
   retrievePrimitiveLocation,
   retrievePrimitiveLocation1,
   retrieveLocationKeyOpt,
   toPrimitiveLocation,
   retrieveKey,
   retrieveKey1,
   retrieveKeyOpt,
   retrieveLocation,
   getLocations,
   ) where

import Data.Maybe

import qualified Data.Map as Map

import Util.ExtendedPrelude

import SimpleDB.Types
import SimpleDB.ServerErrors
import SimpleDB.BDBOps

locationsSame :: Location -> PrimitiveLocation -> Bool
locationsSame (Location loc1) (PrimitiveLocation loc2) = loc1 == loc2

-- | Get the 'PrimitiveLocation' corresponding to a given 'Location'.
retrievePrimitiveLocation :: VersionData -> Location -> PrimitiveLocation
retrievePrimitiveLocation versionData =
   retrievePrimitiveLocation1 (redirects versionData)

-- | Get the 'PrimitiveLocation' corresponding to a given 'Location',
-- when we know there are no redirects.
toPrimitiveLocation :: Location -> PrimitiveLocation
toPrimitiveLocation (Location lNo) = PrimitiveLocation lNo

-- | Get the 'PrimitiveLocation' corresponding to a given 'Location',
-- given the 'redirects' map from the 'VersionData'.

retrievePrimitiveLocation1 :: Map.Map Location PrimitiveLocation
    -> Location -> PrimitiveLocation
retrievePrimitiveLocation1 map location =
   Map.findWithDefault (toPrimitiveLocation location) location map

-- | Get the 'BDBKey' for a 'Location' in a particular version.
retrieveLocationKeyOpt :: VersionData -> Location -> Maybe BDBKey
retrieveLocationKeyOpt versionData location =
   let
      pLocation = retrievePrimitiveLocation versionData location
   in
      retrieveKeyOpt versionData pLocation

-- | Get the 'BDBKey' for a 'PrimitiveLocation' in a particular version.
retrieveKey :: VersionData -> PrimitiveLocation -> IO BDBKey
retrieveKey versionData = retrieveKey1 (objectDictionary versionData)

-- | Get the 'BDBKey' for a 'PrimitiveLocation' in a particular version,
-- or 'Nothing' if it doesn't exist.
retrieveKeyOpt :: VersionData -> PrimitiveLocation -> Maybe BDBKey
retrieveKeyOpt versionData = retrieveKeyOpt1 (objectDictionary versionData)

retrieveKey1 :: Map.Map PrimitiveLocation BDBKey
   -> PrimitiveLocation -> IO BDBKey
retrieveKey1 fm primitiveLocation =
   case retrieveKeyOpt1 fm primitiveLocation of
      Just bdbKey -> return bdbKey
      Nothing -> throwError NotFoundError (
         "Version does not contain location "
         ++ show guessedLocation)
   where
      guessedLocation = Location loc
      PrimitiveLocation loc = primitiveLocation

retrieveKeyOpt1 :: Map.Map PrimitiveLocation BDBKey
   -> PrimitiveLocation -> Maybe BDBKey
retrieveKeyOpt1 fm primitiveLocation =
   Map.lookup primitiveLocation fm

-- | Reverse the redirects to extract the 'Location' for a particular
-- 'PrimitiveLocation'
-- We don't expect this to be particularly efficient, but as it
-- is only used during 'GetParentLocation', I don't think that matters
-- much.
retrieveLocation :: VersionData -> PrimitiveLocation -> Location
retrieveLocation versionData (pLocation @ (PrimitiveLocation lNo)) =
   let
      redirects1 :: [(Location,PrimitiveLocation)]
      redirects1 = Map.toList (redirects versionData)

      redirectedOpt :: Maybe Location
      redirectedOpt = findJust
         (\ (location,pLocation2) -> if pLocation2 == pLocation
            then
               Just location
            else
               Nothing
            )
         redirects1
   in
      fromMaybe (Location lNo) redirectedOpt

-- | Retrieve all Locations accessible to a VersionData.
getLocations :: VersionData -> [Location]
getLocations versionData =
   let
      redirectLocs = Map.keys (redirects versionData)
      redirectPrimLocs = Map.elems (redirects versionData)

      objectDictMinusRedirects =
         foldl
            (\ map0 primLoc -> Map.delete primLoc map0)
            (objectDictionary versionData)
            redirectPrimLocs

      otherPrimLocs = Map.keys objectDictMinusRedirects
      otherLocs = map
         (\ (PrimitiveLocation locNo) -> Location locNo)
         otherPrimLocs
   in
      redirectLocs ++ otherLocs
