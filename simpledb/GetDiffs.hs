-- | 'GetDiffs.getDiffs' implements the GetDiffs command.
module GetDiffs(
   getDiffs
   ) where

import Monad

import Data.FiniteMap

import Computation(done)

import PasswordFile(User)

import BDBOps
import SimpleDBTypes
import VersionInfo
import VersionState
import ServerErrors
import PrimitiveLocation
import VersionData
import Retrieve
import SetGetSecurityData

-- | Implement 'SimpleDBTypes.GetDiffs'.
-- Security Note.  We enforce the following checks:
--    (1) the user must have read access to all data we send.
--    (2) the versions in the second argument must be ancestors of
--        the version given by the first argument.
-- Thus it is possible for the user to find out if something has
-- changed since an older ancestor version, even without read access
-- to the ancestor version. 
getDiffs :: SimpleDB -> User -> ObjectVersion -> [ObjectVersion] 
   -> IO [(Location,Diff)]
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

      case parentData of
         [] -> -- we just have to return IsNew for everything
            mapM
               (\ location ->
                  do
                     icsl <- retrieve simpleDB user thisVersion location
                     newParentOpt <- getParentLocation simpleDB user 
                        (thisVersion,location)
                     let
                        diff = IsNew {
                           changed = Left icsl,
                           newParentOpt = newParentOpt
                           }
                     return (location,diff)
                  )
               (getLocations thisVersionData)
         ((headParentVersion,headParentVersionData):_) ->
            do
               -- Construct a map back from BDBKey 
               -- -> (ObjectVersion,Location) for the parents.
               (bdbDict :: FiniteMap BDBKey (ObjectVersion,Location)) <- foldM
                  (\ map0 (parentVersion,parentVersionData) ->
                     foldM
                        (\ map0 location ->
                           do
                              let
                                 primitiveLocation = retrievePrimitiveLocation
                                    parentVersionData location
                              bdbKey <- retrieveKey parentVersionData 
                                 primitiveLocation 
                              return (addToFM map0 bdbKey (
                                 parentVersion,location))
                           )
                        map0
                        (getLocations parentVersionData) 
                     )
                  emptyFM
                  parentData

               let
                  -- Construct a map from each location used in at least
                  -- one parent version to one such parent version.
                  locationMap :: FiniteMap Location ObjectVersion
                  locationMap = foldl
                     (\ map0 (parentVersion,parentVersionData) ->
                        foldl
                           (\ map0 location 
                              -> addToFM map0 location parentVersion)
                           map0
                           (getLocations parentVersionData)
                        )
                     emptyFM
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
                                 changed <- case lookupFM bdbDict key1 of
                                    Just locVers -> return (Right locVers)
                                    Nothing -> 
                                       do
                                          icsl <- retrieve simpleDB
                                             user thisVersion location
                                          return (Left icsl)
                                 case lookupFM locationMap location of
                                    Nothing -> 
                                       do
                                          newParentOpt <- getParentLocation
                                             simpleDB user 
                                             (thisVersion,location)
                                          let
                                             diff = IsNew { 
                                                changed = changed,
                                                newParentOpt = newParentOpt
                                                }
                                          return diff
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
 