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

import Computation(done)

import FindCycle

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
                        (_,fm1) <- updateVersionData fm0 objectVersion1 
                           frozenVersion
                        getVersionData fm1
      fm <- getVersionData emptyFM
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
   FiniteMap ObjectVersion VersionData
   -> ObjectVersion 
   -> FrozenVersion
   -> IO (VersionData,FiniteMap ObjectVersion VersionData)
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
         (parentObjectDictionary,parentRedirects,parentParentsMap) = 
            case parentVersionDataOpt of
               Just versionData -> 
                  (objectDictionary versionData,redirects versionData,
                     SimpleDBTypes.parentsMap versionData)
               Nothing -> (emptyFM,emptyFM,emptyFM)

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
            -> (Location,Either BDBKey (ObjectVersion,Location))
            -> IO (FiniteMap PrimitiveLocation BDBKey)
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
               return (addToFM fm0 primitiveLocation bdbKey)
      (thisObjectDictionary :: FiniteMap PrimitiveLocation BDBKey)
         <- foldM addObjectKey parentObjectDictionary 
            (objectChanges frozenVersion)

      (parentChangesMap :: FiniteMap Location Location) 
         <- foldM
            (\ parentChangesMap0 (object,parent) 
               -> case lookupFM parentChangesMap0 object of
                  Just _ ->
                     throwError MiscError ("Parent changes include " 
                        ++ show object ++ " twice")
                  Nothing -> return (addToFM parentChangesMap0 object parent)
               )
            emptyFM
            (parentChanges frozenVersion)

      -- Check for cycles in the parent changes
      let
         newParentFn :: Location -> [Location]
         newParentFn object = 
            let
               parent :: Maybe Location
               parent =
                  case lookupFM parentChangesMap object of
                     Nothing -> lookupFM parentParentsMap object
                     parent1 -> parent1
            in
               maybeToList parent

      case findCycle (map fst (parentChanges frozenVersion)) newParentFn of
         Nothing -> done
         Just cycle ->
            throwError MiscError ("Cycle " ++ show cycle ++ 
               " detected in parent locations")

      let
         thisParentsMap = plusFM parentParentsMap parentChangesMap

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

      return (thisVersionData,addToFM fm thisObjectVersion thisVersionData)

