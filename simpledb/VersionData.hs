-- | VersionData operations
module VersionData(
   getVersionData,
   modifyVersionData,
   createVersionData,
   ) where

import Maybe
import Monad

import Data.IORef
import Data.FiniteMap

import VersionInfo
import SimpleDBTypes
import ServerErrors
import BDBOps
import BDBExtras
import PrimitiveLocation

-- -------------------------------------------------------------------
-- Accessing version data
-- -------------------------------------------------------------------

getVersionData :: SimpleDB -> ObjectVersion -> IO VersionData
getVersionData simpleDB objectVersion =
   do
      fm <- readIORef (versionData simpleDB)
      getVersionData0 fm objectVersion

getVersionData0 :: FiniteMap ObjectVersion VersionData -> ObjectVersion 
   -> IO VersionData
getVersionData0 fm objectVersion =
   case lookupFM fm objectVersion of
      Just versionData -> return versionData
      Nothing -> throwError NotFoundError
         ("Unknown version " ++ show objectVersion)
   

-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- | Read the VersionData from the versionDB database.
createVersionData :: BDB -> IO (FiniteMap ObjectVersion VersionData)
createVersionData bdb =
   do
      cursor <- mkCursor bdb
      let
         getVersionData :: FiniteMap ObjectVersion VersionData 
            -> IO (FiniteMap ObjectVersion VersionData)
         getVersionData fm0 =
            do
               nextDataOpt <- getObjectAtCursor cursor
               case nextDataOpt of
                  Nothing -> return fm0
                  Just (bdbKey,frozenVersion :: FrozenVersion) ->
                     do
                        let
                           objectVersion1 = ObjectVersion
                              (fromIntegral bdbKey)
                        fm1 <- updateVersionData fm0 objectVersion1 
                           frozenVersion
                        getVersionData fm1
      getVersionData emptyFM

-- --------------------------------------------------------------------  
-- --------------------------------------------------------------------  

-- | Update VersionData, and simultaneously update the database.
-- NB.  This does not change the versionData field of 'SimpleDB',
-- which should be altered by the caller when or if the commit is successful. 
modifyVersionData :: SimpleDB -> ObjectVersion -> FrozenVersion -> TXN 
   -> IO (FiniteMap ObjectVersion VersionData)
modifyVersionData simpleDB (objectVersion @ (ObjectVersion ovN)) 
      frozenVersion txn =
   do
      let
         bdbKey = fromIntegral ovN
      setObjectHere1 (versionDB simpleDB) bdbKey txn frozenVersion
      versionFM0 <- readIORef (versionData simpleDB)
      updateVersionData versionFM0 objectVersion frozenVersion


-- -------------------------------------------------------------------
-- Updating the VersionData map.
-- -------------------------------------------------------------------

updateVersionData :: 
   FiniteMap ObjectVersion VersionData
   -> ObjectVersion 
   -> FrozenVersion
   -> IO (FiniteMap ObjectVersion VersionData)
updateVersionData fm thisObjectVersion frozenVersion =
   do
      let
         lookupVersion :: ObjectVersion -> IO VersionData
         lookupVersion = getVersionData0 fm

      (parentVersionDataOpt :: Maybe VersionData) 
          <- case parent' frozenVersion of
             Nothing -> return Nothing
             Just parentVersion -> 
                do
                   versionData <- lookupVersion parentVersion
                   return (Just versionData)

 
      let
         parentObjectDictionary :: FiniteMap PrimitiveLocation BDBKey
         parentRedirects :: FiniteMap Location PrimitiveLocation
         (parentObjectDictionary,parentRedirects) = 
            case parentVersionDataOpt of
               Just versionData -> 
                  (objectDictionary versionData,redirects versionData)
               Nothing -> (emptyFM,emptyFM)

         addRedirect ::
            FiniteMap Location PrimitiveLocation
            ->  (Location,Either ObjectVersion PrimitiveLocation)
            -> IO (FiniteMap Location PrimitiveLocation)
         addRedirect fm0 (location,redirectSource) =
            do        
               primitiveLocation <- case redirectSource of
                  Left objectVersion ->
                     do
                        fromVersionData <- lookupVersion objectVersion
                        return (
                           retrievePrimitiveLocation fromVersionData location)
                  Right primitiveLocation -> return primitiveLocation
               let
                  fm1 = if locationsSame location primitiveLocation
                     then
                        fm0
                     else
                        addToFM fm0 location primitiveLocation
               return fm1

      (thisRedirects :: FiniteMap Location PrimitiveLocation) 
         <- foldM addRedirect parentRedirects (redirects' frozenVersion)

      let
         addObjectKey 
            :: FiniteMap PrimitiveLocation BDBKey
            -> (Location,Either BDBKey (Location,ObjectVersion))
            -> IO (FiniteMap PrimitiveLocation BDBKey)
         addObjectKey fm0 (location,keySource) =
            do
               bdbKey <- case keySource of
                  Left bdbKey -> return bdbKey
                  Right (location1,objectVersion1) ->
                     do
                        versionData1 <- lookupVersion objectVersion1
                        let
                           primitiveLocation1 = retrievePrimitiveLocation
                              versionData1 location1
                        retrieveKey versionData1 primitiveLocation1
               let
                  primitiveLocation = retrievePrimitiveLocation1
                     thisRedirects location
               return (addToFM fm0 primitiveLocation bdbKey)
      (thisObjectDictionary :: FiniteMap PrimitiveLocation BDBKey)
         <- foldM addObjectKey parentObjectDictionary 
            (objectChanges frozenVersion)
      let
         thisVersionData = VersionData {
            parent = parent' frozenVersion,
            objectDictionary = thisObjectDictionary,
            redirects = thisRedirects
            }

      return (addToFM fm thisObjectVersion thisVersionData)

