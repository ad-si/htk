-- | 'GetDiffs.getDiffs' implements the GetDiffs command.
module SimpleDB.GetDiffs(
   getDiffs
   ) where

import Control.Monad

import qualified Data.Map as Map

import Util.Computation(done)

import Server.PasswordFile(User)

import SimpleDB.BDBOps
import SimpleDB.Types
import SimpleDB.VersionInfo
import SimpleDB.VersionState
import SimpleDB.ServerErrors
import SimpleDB.PrimitiveLocation
import SimpleDB.VersionData
import SimpleDB.Retrieve

-- | Implement 'SimpleDBTypes.GetDiffs'.
-- Security Note.  We enforce the following checks:
--    (1) the user must have read access to all data we send.
--    (2) the versions in the second argument must be ancestors of
--        the version given by the first argument.
-- Thus it is possible for the user to find out if something has
-- changed since an older ancestor version, even without read access
-- to the ancestor version.
getDiffs :: SimpleDB -> User -> ObjectVersion -> [ObjectVersion]
   -> IO ([(Location,Diff)],[(Location,Location)])
getDiffs simpleDB user thisVersion parentVersions =
   do
      mapM_
         (\ parentVersion ->
            do
               isAncestor <- versionIsAncestor (versionState simpleDB)
                  parentVersion thisVersion
               if isAncestor
                  then
                     done
                  else
                     throwError AccessError ("GetDiffs not allowed except "
                        ++ "between a version and its ancestors")
            )
         parentVersions

      thisVersionData <- getVersionData simpleDB thisVersion

      (parentData :: [(ObjectVersion,VersionData)])
         <- mapM
            (\ parentVersion ->
               do
                  parentVersionData <- getVersionData simpleDB parentVersion
                  return (parentVersion,parentVersionData)
               )
            parentVersions


      (diffs1 :: [(Location,Diff)]) <- case parentData of
         [] -> -- we just have to return IsNew for everything
            mapM
               (\ location ->
                  do
                     icsl <- retrieve simpleDB user thisVersion location
                     let
                        diff = IsNew {
                           changed = Left icsl
                           }
                     return (location,diff)
                  )
               (getLocations thisVersionData)
         ((headParentVersion,headParentVersionData):_) ->
            do
               -- Construct a map back from BDBKey
               -- -> (ObjectVersion,Location) for the parents.
               (bdbDict :: Map.Map BDBKey (ObjectVersion,Location)) <- foldM
                  (\ map0 (parentVersion,parentVersionData) ->
                     foldM
                        (\ map0 location ->
                           do
                              let
                                 primitiveLocation = retrievePrimitiveLocation
                                    parentVersionData location
                              bdbKey <- retrieveKey parentVersionData
                                 primitiveLocation
                              return (Map.insert bdbKey (
                                 parentVersion,location) map0)
                           )
                        map0
                        (getLocations parentVersionData)
                     )
                  Map.empty
                  parentData

               let
                  -- Construct a map from each location used in at least
                  -- one parent version to one such parent version.
                  locationMap :: Map.Map Location ObjectVersion
                  locationMap = foldl
                     (\ map0 (parentVersion,parentVersionData) ->
                        foldl
                           (\ map0 location
                              -> Map.insert location parentVersion map0)
                           map0
                           (getLocations parentVersionData)
                        )
                     Map.empty
                     parentData

                  -- Function constructing Diff for a particular item in the
                  -- new version's object dictionary
                  mkDiff :: Location -> BDBKey -> IO Diff
                  mkDiff location key1 =
                     let
                        pLocation1 = retrievePrimitiveLocation
                           headParentVersionData location
                        key2Opt = retrieveKeyOpt headParentVersionData
                           pLocation1
                     in
                        case key2Opt of
                           Just key2 | key1 == key2
                              -> return IsOld
                           _ ->
                              do
                                 changed <- case Map.lookup key1 bdbDict of
                                    Just locVers -> return (Right locVers)
                                    Nothing ->
                                       do
                                          icsl <- retrieve simpleDB
                                             user thisVersion location
                                          return (Left icsl)
                                 case Map.lookup location locationMap of
                                    Nothing ->
                                       return (IsNew {
                                          changed = changed
                                          })
                                    Just parentVersion ->
                                       return (
                                          IsChanged {
                                             existsIn = parentVersion,
                                             changed = changed
                                          })

               mapM
                  (\ location ->
                     do
                        let
                           pLocation = retrievePrimitiveLocation
                              thisVersionData location
                        key <- retrieveKey thisVersionData pLocation
                        diff <- mkDiff location key
                        return (location,diff)
                     )
                  (getLocations thisVersionData)

      let
         diffFM :: Map.Map Location Location -> Map.Map Location Location
            -> [(Location,Location)]
         diffFM thisFM parentFM =
            filter
               (\ (location,parent1) ->
                  (Map.lookup location parentFM /= Just parent1)
                  )
               (Map.toList thisFM)

         parentParentsMap = case parentData of
            [] -> Map.empty
            ((_,headParentVersionData):_) -> parentsMap headParentVersionData


         diffs2 :: [(Location,Location)]
         diffs2 = diffFM (parentsMap thisVersionData) parentParentsMap

      return (diffs1,diffs2)
