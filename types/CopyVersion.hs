{- This module contains the code for moving the contents of a version from
   one repository to another. -} 
module CopyVersion(
   copyVersion,
   FromTo(..),
   ) where

import Maybe

import Data.FiniteMap

import ObjectSource(fromICSL)
import VersionDB
import VersionInfo

-- | Denote corresponding elements for the source and destination.
data FromTo a = FromTo {from :: a,to :: a}

-- In the arguments t
copyVersion :: 
   FromTo Repository 
      -- ^ Source and target repositories.
   -> [FromTo ObjectVersion] 
      -- ^ The closest common ancestors (which had better match)
   -> ObjectVersion
      -- ^ The object version to copy.
   -> IO ObjectVersion
      -- ^ The copied object version
copyVersion (FromTo {from = fromRepository,to = toRepository}) parents 
     fromVersion =
  do
     -- (0) compute a map from old parent versions to new parent versions
     let
        parentsMap :: FiniteMap ObjectVersion ObjectVersion
        parentsMap =
           foldl
              (\ map0 (FromTo {from = fromParent,to = toParent})
                 -> addToFM map0 fromParent toParent
                 )
              emptyFM
              parents

        mapParent :: ObjectVersion -> ObjectVersion
        mapParent fromParent = lookupWithDefaultFM parentsMap
           (error "copyVersion: given a parent version that does not exist")
           fromParent

     -- (1) get diffs for fromVersion from its parents.
     diffs <- getDiffs fromRepository fromVersion (map from parents)
     let
        -- compute commit-changes for commit.
        toCommitChange :: (Location,Diff) -> Maybe (Location,CommitChange)
        toCommitChange (_,IsOld) = Nothing
        toCommitChange (location,IsChanged {changed = changed}) 
           = Just (mapChanged location changed)
        toCommitChange (location,IsNew {changed = changed}) 
           = Just (mapChanged location changed)

        mapChanged location (Left icsl) = (location,Left (fromICSL icsl))
        mapChanged location (Right (location1,fromVersion)) 
           = (location,Right (location1,mapParent fromVersion))

        commitChanges :: [(Location,CommitChange)]
        commitChanges = mapMaybe toCommitChange diffs

        -- compute redirects for commit.
        toRedirect :: (Location,Diff) -> Maybe (Location,Maybe ObjectVersion)
        toRedirect (_,IsOld) = Nothing
        toRedirect (location,IsChanged {existsIn = fromVersion}) 
           = Just (location,Just (mapParent fromVersion))
        toRedirect (location,IsNew _ ) = Just (location,Nothing)
      
        redirects :: [(Location,Maybe ObjectVersion)]
        redirects = mapMaybe toRedirect diffs
 
     -- (2) get version info for fromVersion
     versionInfo0 <- retrieveVersionInfo fromRepository fromVersion

     -- (3) Allocate the new version
     toVersion <- newVersion toRepository
 
     -- (4) Modify the versionInfo.
     let
        user0 = user versionInfo0
        user1 = user0 {
           version = toVersion,
           parents = map to parents
           }
        versionInfo1 = versionInfo0 {user = user1}

     -- (5) Commit
     commit toRepository (Right versionInfo1) redirects commitChanges

     return toVersion 