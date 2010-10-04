-- | VersionData operations
module SimpleDB.VersionData(
   getVersionData,
   modifyVersionData,
   createVersionData,
   ) where

import Data.Maybe
import Control.Monad

import Data.IORef
import qualified Data.Map as Map

import Util.Computation(done)

import Graphs.FindCycle

import SimpleDB.VersionInfo
import SimpleDB.Types
import SimpleDB.ServerErrors
import SimpleDB.BDBOps
import SimpleDB.BDBExtras
import SimpleDB.PrimitiveLocation

-- -------------------------------------------------------------------
-- Accessing version data
-- -------------------------------------------------------------------

getVersionData :: SimpleDB -> ObjectVersion -> IO VersionData
getVersionData simpleDB objectVersion =
   do
      fm <- readIORef (versionData simpleDB)
      getVersionData0 fm objectVersion

getVersionData0 :: Map.Map ObjectVersion VersionData -> ObjectVersion
   -> IO VersionData
getVersionData0 fm objectVersion =
   case Map.lookup objectVersion fm of
      Just versionData -> return versionData
      Nothing -> throwError NotFoundError
         ("Unknown version " ++ show objectVersion)


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- | Read the VersionData from the versionDB database.
createVersionData :: BDB -> IO (Map.Map ObjectVersion VersionData)
createVersionData bdb =
   do
      cursor <- mkCursor bdb
      let
         getVersionData :: Map.Map ObjectVersion VersionData
            -> IO (Map.Map ObjectVersion VersionData)
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
                        (_,fm1) <- updateVersionData fm0 objectVersion1
                           frozenVersion
                        getVersionData fm1
      fm <- getVersionData Map.empty
      closeCursor cursor
      return fm

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------

-- | Update VersionData.
-- This function returns
-- (1) the computed VersionData; (2) an action to complete the modification
-- when we are sure the commit will be successful.
modifyVersionData :: SimpleDB -> ObjectVersion -> FrozenVersion -> TXN
   -> IO (VersionData,IO ())
modifyVersionData simpleDB (objectVersion @ (ObjectVersion ovN))
      frozenVersion txn =
   do
      let
         bdbKey = fromIntegral ovN
      setObjectHere1 (versionDB simpleDB) bdbKey txn frozenVersion
      versionFM0 <- readIORef (versionData simpleDB)
      (versionData1,versionFM1)
         <- updateVersionData versionFM0 objectVersion frozenVersion
      return (versionData1,writeIORef (versionData simpleDB) versionFM1)


-- -------------------------------------------------------------------
-- Updating the VersionData map.
-- -------------------------------------------------------------------

updateVersionData ::
   Map.Map ObjectVersion VersionData
   -> ObjectVersion
   -> FrozenVersion
   -> IO (VersionData,Map.Map ObjectVersion VersionData)
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
         parentObjectDictionary :: Map.Map PrimitiveLocation BDBKey
         parentRedirects :: Map.Map Location PrimitiveLocation
         (parentObjectDictionary,parentRedirects,parentParentsMap) =
            case parentVersionDataOpt of
               Just versionData ->
                  (objectDictionary versionData,redirects versionData,
                     parentsMap versionData)
               Nothing -> (Map.empty,Map.empty,Map.empty)

         addRedirect ::
            Map.Map Location PrimitiveLocation
            ->  (Location,Either ObjectVersion PrimitiveLocation)
            -> IO (Map.Map Location PrimitiveLocation)
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
                        Map.insert location primitiveLocation fm0
               return fm1

      (thisRedirects :: Map.Map Location PrimitiveLocation)
         <- foldM addRedirect parentRedirects (redirects' frozenVersion)

      let
         addObjectKey
            :: Map.Map PrimitiveLocation BDBKey
            -> (Location,Either BDBKey (ObjectVersion,Location))
            -> IO (Map.Map PrimitiveLocation BDBKey)
         addObjectKey fm0 (location,keySource) =
            do
               bdbKey <- case keySource of
                  Left bdbKey -> return bdbKey
                  Right (objectVersion1,location1) ->
                     do
                        versionData1 <- lookupVersion objectVersion1
                        let
                           primitiveLocation1 = retrievePrimitiveLocation
                              versionData1 location1
                        retrieveKey versionData1 primitiveLocation1
               let
                  primitiveLocation = retrievePrimitiveLocation1
                     thisRedirects location
               return (Map.insert primitiveLocation bdbKey fm0)
      (thisObjectDictionary :: Map.Map PrimitiveLocation BDBKey)
         <- foldM addObjectKey parentObjectDictionary
            (objectChanges frozenVersion)

      (parentChangesMap :: Map.Map Location Location)
         <- foldM
            (\ parentChangesMap0 (object,parent)
               -> case Map.lookup object parentChangesMap0 of
                  Just _ ->
                     throwError MiscError ("Parent changes include "
                        ++ show object ++ " twice")
                  Nothing -> return (Map.insert object parent parentChangesMap0)
               )
            Map.empty
            (parentChanges frozenVersion)

      -- Check for cycles in the parent changes
      let
         newParentFn :: Location -> [Location]
         newParentFn object =
            let
               parent :: Maybe Location
               parent =
                  case Map.lookup object parentChangesMap of
                     Nothing -> Map.lookup object parentParentsMap
                     parent1 -> parent1
            in
               maybeToList parent

      case findCycle (map fst (parentChanges frozenVersion)) newParentFn of
         Nothing -> done
         Just cycle ->
            throwError MiscError ("Cycle " ++ show cycle ++
               " detected in parent locations")

      let
         thisParentsMap = Map.union parentChangesMap parentParentsMap

         thisVersionData = VersionData {
            parent = parent' frozenVersion,
            objectDictionary = thisObjectDictionary,
            redirects = thisRedirects,
            parentsMap = thisParentsMap
            }

      -- Check that all the parents and objects in the parent changes exist.
      mapM_
         (\ (parent,object) ->
            case (retrieveLocationKeyOpt thisVersionData parent,
                  retrieveLocationKeyOpt thisVersionData object) of
               (Just _,Just _) -> done
               _ -> throwError MiscError (
                  "Parent specification " ++ show (parent,object) ++
                     " contains non-existent object")
            )
         (parentChanges frozenVersion)

      return (thisVersionData,Map.insert thisObjectVersion thisVersionData fm)

