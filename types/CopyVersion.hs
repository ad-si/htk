-- | This module contains the code for moving the contents of a version from
-- one repository to another.
module CopyVersion(
   copyVersion,
   FromTo(..),
   ) where

import Maybe

import DeprecatedFiniteMap

import Computation
import VariableSet(HasKey(..))
import ICStringLen(ICStringLen)

import VersionDB
import qualified ObjectSource
import VersionInfo

import Link
import View
import ViewType
import CodedValue
import ObjectTypes
import GlobalRegistry
import MergeTypes
import MergeReAssign
import Merging
import VersionGraph

-- | Denote corresponding elements for the source and destination.
data FromTo a = FromTo {from :: a,to :: a}

copyVersion ::
   FromTo VersionGraph
      -- ^ Source and target version graphs.
   -> ObjectVersion
      -- ^ The object version to copy (original version).
   -> VersionInfo
      -- ^ The version info of the version in the new repository
      -- (which will already have been copied by the function in
      -- the CopyVersionInfos module)
   -> [FromTo ObjectVersion]
      -- ^ the version of the known committed parents known to both
      -- the source and destination repository.
   -> (ObjectVersion -> IO VersionInfo)
      -- ^ a map which will goes from an object version in the source
      -- repository, to the corresponding VersionInfo in the destination graph.
   -> IO SimpleDBCommand
      -- ^ the command to execute to do the copying.  (We do not actually
      -- perform it.
copyVersion (FromTo {from = fromVersionGraph,to = toVersionGraph})
     fromVersion toVersionInfo parents toNewVersionInfo =
  do
     let
        fromRepository = toVersionGraphRepository fromVersionGraph
        toRepository = toVersionGraphRepository toVersionGraph

        fromVersionGraphClient
           = VersionGraph.toVersionGraphClient fromVersionGraph
        toVersionGraphClient
           = VersionGraph.toVersionGraphClient toVersionGraph

        parentsMap :: FiniteMap ObjectVersion ObjectVersion
        parentsMap = listToFM (map
           (\ parent -> (from parent,to parent))
           parents
           )

        mapParent :: ObjectVersion -> ObjectVersion
        mapParent fromVersion = case lookupFM parentsMap fromVersion of
           Nothing -> error "CopyVersion: unknown parent"
           Just toVersion -> toVersion

     -- (1) Get a View for the old version.
     view0 <- getView fromRepository fromVersionGraphClient fromVersion

     -- (2) get diffs for fromVersion from its parents.
     (diffs1 :: [(Location,Diff)],diffs2 :: [(Location,Location)])
         <- getDiffs fromRepository fromVersion (map from parents)

     -- (3) construct view for new version.
     view1 <- createView toRepository toVersionGraphClient toVersionInfo

     -- (4) Turn diffs1 into object versions.

     -- We need to get at ALL the WrappedMergeLinks for the source view,
     -- so we can know to which links copyObject needs to be applied.  We
     -- also use this to discard any inaccessible links.
     wrappedMergeLinks <- getAllWrappedMergeLinks view0

     let
        wmlMap :: FiniteMap Location WrappedMergeLink
        wmlMap = listToFM
           (map
              (\ wml -> (toKey wml,wml))
              wrappedMergeLinks
              )

        -- now we can process the diffs.

        mapChanged :: Location -> ChangeData -> IO (Maybe CommitChange)
        mapChanged location changeData =
           case lookupFM wmlMap location of
              Nothing -> return Nothing
                 -- This location is inaccessible, and so the change can
                 -- be discarded.
              Just (WrappedMergeLink link) ->
                 case changeData of
                    Right (fromVersion,location1) ->
                       return (Just (Right (mapParent fromVersion,location1)))
                    Left icsl ->
                       do
                          objectSource <- mapLink icsl link
                          return (Just (Left objectSource))

        mapLink :: HasMerging object
           => ICStringLen -> Link object -> IO ObjectSource
        mapLink icsl (link :: Link object) =
           let
              copyObject1
                 :: Maybe (View -> object -> View -> (ObjectVersion
                    -> IO VersionInfo) -> IO object)
              copyObject1 = copyObject
           in
              case copyObject1 of
                 Nothing -> -- easy case, no transformation necessary
                    return (importICStringLenPure icsl)
                 Just copyObject2 ->
                    do
                       object0 <- doDecodeIO icsl view0
                       object1
                          <- copyObject2 view0 object0 view1 toNewVersionInfo
                       codedValue1 <- doEncodeIO object1 view1
                       objectSource1 <- importICStringLen codedValue1
                       return objectSource1

     (commitChanges0 :: [Maybe (Location,CommitChange)])
        <- mapM
           (\ (location,diff) ->
              let
                 mkCommitChange :: ChangeData
                    -> IO (Maybe (Location,CommitChange))
                 mkCommitChange changed0 =
                    do
                       commitChangedOpt <- mapChanged location changed0
                       return (fmap
                          (\ commitChanged -> (location,commitChanged))
                          commitChangedOpt
                          )
              in
                 case diff of
                    IsOld -> return Nothing
                    IsNew {changed = changed0} -> mkCommitChange changed0
                    IsChanged {changed = changed0} -> mkCommitChange changed0
              )
           diffs1

     let
        commitChanges1 :: [(Location,CommitChange)]
        commitChanges1 = catMaybes commitChanges0

        -- compute redirects for commit.
        toRedirect :: (Location,Diff) -> Maybe (Location,Maybe ObjectVersion)
        toRedirect (_,IsOld) = Nothing
        toRedirect (location,IsChanged {existsIn = fromVersion})
           = Just (location,Just (mapParent fromVersion))
        toRedirect (location,IsNew _ ) = Just (location,Nothing)

        redirects :: [(Location,Maybe ObjectVersion)]
        redirects = mapMaybe toRedirect diffs1



     (commitChanges2
        :: [(Location,Either ICStringLen (ObjectVersion,Location))])
        <- mapM
            (\ (location,newItem) ->
               case newItem of
                  Left objectSource ->
                     do
                        icsl <- ObjectSource.exportICStringLen objectSource
                        return (location,Left icsl)
                  Right locVers -> return (location,Right locVers)
               )
            commitChanges1

     -- (6) we can now commit.
     let
        headVersionOpt = case parents of
           [] -> Nothing
           parent : _ -> Just (to parent)

        version1 = version (user toVersionInfo)

        versionInformation = case headVersionOpt of
           Nothing -> Version1 version1
           Just headVersion -> Version1Plus version1 headVersion

     let
        command = Commit versionInformation redirects commitChanges2 diffs2

     return command
-- ------------------------------------------------------------------------
-- Computing a dictionary of the accessible links within a view.
-- We will use this (a) to filter out those links which do not need to
-- be copied (since they are inaccessible); (b) to find out which ones
-- need special treatment.  (MergeTypes.copyObject)
-- ------------------------------------------------------------------------

getAllWrappedMergeLinks :: View -> IO [WrappedMergeLink]
getAllWrappedMergeLinks view =
   do
      -- This carries out a stripped-down version of Merging.mergeViews
      -- to call mkLinkReAssigner.  Instead we could probably strip down
      -- mkLinkReAssigner, but it doesn't seem worthwhile.

      (allObjectTypeTypes :: [WrappedObjectTypeTypeData])
         <- getAllObjectTypeTypes

      (allTypes :: [(WrappedObjectTypeTypeData,
         [(GlobalKey,[(View,WrappedObjectType)])])])
         <- mapM
            (\ wrappedObjectTypeTypeData ->
               do
                  theseTypes <- mergeObjectTypeTypeData
                     (error "CopyVersion.A") [view] view
                     wrappedObjectTypeTypeData
                  return (wrappedObjectTypeTypeData,theseTypes)
               )
            allObjectTypeTypes


      linkReAssignerWE <- mkLinkReAssigner [view] allTypes

      linkReAssigner
         <- coerceWithErrorStringIO "CopyVersion.B" linkReAssignerWE

      let
         wmls :: [WrappedMergeLink]
         wmls = (map snd) . keysFM . linkMap $ linkReAssigner

      return wmls
