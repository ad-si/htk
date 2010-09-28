-- | The code in this module has the job of copying the VersionInfo for a version
-- (such things as the user description and so on, but not the actual
-- checked-in objects) from one repository to another, unless that has
-- already been done.  It also does the same for all the version's ancestors.
module Types.CopyVersionInfos(
   copyVersionInfos,
   -- :: FromTo VersionGraph -> [ObjectVersion]
   -- -> IO (ObjectVersion -> IO VersionInfo)
   -- Copy all the object versions in the list from the from VersionGraph
   -- to the toVersionGraph.
   --
   -- We return a function which takes any ObjectVersion in the from repository
   -- and returns the corresponding VersionInfo (current or planned) in the
   -- to repository.
   ) where

import Maybe
import Monad

import Util.DeprecatedFiniteMap

import Util.Computation(done)
import Util.AtomString(toString)

import SimpleDB.ServerErrors
import Types.VersionDB
import SimpleDB.VersionInfo

import Types.VersionGraph
import Types.VersionGraphClient
import Types.CopyVersion(FromTo(..))

-- ------------------------------------------------------------------------
-- Datatypes
-- ------------------------------------------------------------------------

-- We make an initial pass over all the versions that need to be copied,
-- computing a ProtoState.

data ProtoState = ProtoState {
      -- we give all ProtoVersions both in a finite map, and in a list.
   fm :: FiniteMap ServerInfo ProtoVersion,
   list :: [ProtoVersion]
      -- parents always occur before their children in this list.
   }


-- A ProtoVersion corresponds to one old version whose info needs to be
-- copied.
data ProtoVersion = ProtoVersion {
   version1 :: ObjectVersion, -- old version number
   versionInfo1 :: VersionInfo,
   parents1 :: [Either ObjectVersion ObjectVersion]
      -- left for version number in destination repository
      -- right if parent does not exist, and version in source repository.
   }

-- Information we compute about the versions in the target graph.
newtype ServerInfoDict = ServerInfoDict (FiniteMap ServerInfo ObjectVersion)

-- ------------------------------------------------------------------------
-- Top-level function
-- ------------------------------------------------------------------------

-- | Copy all the object versions in the list from the from VersionGraph
-- to the toVersionGraph.
--
-- We return a function which takes any ObjectVersion in the from repository
-- and returns the corresponding VersionInfo (current or planned) in the
-- to repository.
copyVersionInfos
   :: FromTo VersionGraph -> [ObjectVersion]
   -> IO (ObjectVersion -> IO VersionInfo)
copyVersionInfos (FromTo {from = fromGraph,to = toGraph}) oldVersions =
   do
      let
         fromClient = toVersionGraphClient fromGraph
         toClient = toVersionGraphClient toGraph

         fromTo = FromTo {
            from = fromClient,
            to = toClient
            }

      (protoState,serverInfoDict) <- prepareVersions fromTo oldVersions

      newInfosFM <- copyProtoStateVersions toGraph protoState


      let
         resultMap :: ObjectVersion -> IO VersionInfo
         resultMap fromVersion =
            case lookupFM newInfosFM fromVersion of
               Just versionInfo -> return versionInfo
               Nothing ->
                  do
                     fromVersionInfo <- getVersionInfo fromClient fromVersion
                     case lookupServerInfo serverInfoDict
                           (server fromVersionInfo) of
                        Nothing -> error ("CopyVersionInfo bug: unknown or "
                           ++ "uncopied version " ++ toString fromVersion)
                        Just toVersion ->
                           do
                              toVersionInfo
                                 <- getVersionInfo toClient toVersion
                              return toVersionInfo
      return resultMap

-- ------------------------------------------------------------------------
-- Initial pass for all the versions
-- ------------------------------------------------------------------------

prepareVersions :: FromTo VersionGraphClient -> [ObjectVersion]
   -> IO (ProtoState,ServerInfoDict)
prepareVersions (FromTo {from = fromGraph,to = toGraph}) versions =
   do
      -- (1) compute ServerInfoDict.
      serverInfoDict <- mkServerInfoDict toGraph

      -- (2) compute initial ProtoState
      let
         protoState0 = ProtoState {fm = emptyFM,list = []}

      -- (3) compute final ProtoState
      protoState1 <- foldM
         (\ protoState0 oldVersion ->
            do
               (protoState1,_) <- prepareOneVersion fromGraph serverInfoDict
                  protoState0 oldVersion
               return protoState1
            )
         protoState0
         versions

      return (protoState1,serverInfoDict)

-- ------------------------------------------------------------------------
-- Initial pass for one version
-- ------------------------------------------------------------------------

prepareOneVersion :: VersionGraphClient -> ServerInfoDict -> ProtoState
   -> ObjectVersion -> IO (ProtoState,Either ObjectVersion ObjectVersion)
prepareOneVersion fromGraph serverInfoDict protoState0 fromVersion =
   do
      versionInfo <- getVersionInfo fromGraph fromVersion
      let
         serverInfo = server versionInfo
      case lookupServerInfo serverInfoDict serverInfo of
         Just toVersion -> -- already in destination repository
            return (protoState0,Left toVersion)
         Nothing -> case lookupVersion protoState0 serverInfo of
            Just _ -> -- already in protoState0
               return (protoState0,Right fromVersion)
            Nothing ->
               -- This node must be inserted in protoState0, along with all
               -- its ancestors not alreadyd entered.
               do
                  (protoState1,toParentVersionsRev) <- foldM
                     (\ (protoState0,toParentVersions0) fromParentVersion ->
                        do
                           (protoState1,toParentVersion)
                              <- prepareOneVersion fromGraph serverInfoDict
                                 protoState0 fromParentVersion
                           return (protoState1,
                              toParentVersion : toParentVersions0)
                        )
                     (protoState0,[])
                     (parents (user versionInfo))
                  let
                     protoVersion = ProtoVersion {
                        version1 = fromVersion,
                        versionInfo1 = versionInfo,
                        parents1 = reverse toParentVersionsRev
                        }

                     protoState2 = ProtoState {
                        fm = addToFM (fm protoState1) serverInfo protoVersion,
                        list = protoVersion : (list protoState1)
                        }

                  return (protoState2,Right fromVersion)

-- -------------------------------------------------------------------------
-- ProtoState
-- -------------------------------------------------------------------------

lookupVersion :: ProtoState -> ServerInfo -> Maybe ProtoVersion
lookupVersion protoState = lookupFM (fm protoState)

-- -------------------------------------------------------------------------
-- ServerInfoDict
-- -------------------------------------------------------------------------

lookupServerInfo :: ServerInfoDict -> ServerInfo -> Maybe ObjectVersion
lookupServerInfo (ServerInfoDict map) = lookupFM map

mkServerInfoDict :: VersionGraphClient -> IO ServerInfoDict
mkServerInfoDict versionGraphClient =
   do
      versionInfo1s <- getVersionInfos versionGraphClient
      let
         -- eliminate all versions with a View attached.
         versionInfos :: [VersionInfo]
         versionInfos = mapMaybe
            (\ versionInfo1 -> case toViewOpt versionInfo1 of
               Just _ -> Nothing
               Nothing -> Just (toVersionInfo versionInfo1)
               )
            versionInfo1s

      return (ServerInfoDict (listToFM (
         map
            (\ versionInfo -> (server versionInfo,version (user versionInfo)))
            versionInfos
         )))

-- ------------------------------------------------------------------------
-- Do actual copying, given the ProtoState
-- ------------------------------------------------------------------------

-- | Do actual copying.  Returns a map from the old version-number to the
-- corresponding VersionInfo in the new repository, for all versions in the
-- ProtoState.
--
-- The first argument is the destination VersionGraph.
copyProtoStateVersions :: VersionGraph -> ProtoState
   -> IO (FiniteMap ObjectVersion VersionInfo)
copyProtoStateVersions toGraph protoState =
   do
      let
         toRepository = toVersionGraphRepository toGraph

         -- versions in order in which we will copy them, with parents before
         -- children.
         protoVersions :: [ProtoVersion]
         protoVersions = reverse (list protoState)

      -- We try to structure this to use only a constant number of
      -- server-requests.
      -- (1) Pass 1.  Allocate the new ObjectVersions.
      let
         command1 :: SimpleDBCommand
         command1 = MultiCommand [ NewVersion | _ <- protoVersions ]
      response1 <- queryRepository toRepository command1

      (protoVersions2 :: [(ObjectVersion,ProtoVersion)]) <-
         case response1 of
            MultiResponse responses1 ->
               return (
                  zipWith
                     (\ (IsObjectVersion newVersion) protoVersion ->
                        (newVersion,protoVersion)
                         )
                     responses1 protoVersions
                  )

      let
         -- Map from old object versions which have not yet been constructed
         -- to the corresponding new object version.
         oldToNew :: FiniteMap ObjectVersion ObjectVersion
         oldToNew =
            listToFM (
               map
                  (\ (newVersion,protoVersion) ->
                     (version1 protoVersion,newVersion)
                     )
                  protoVersions2
               )

         mkParents :: FiniteMap ObjectVersion ObjectVersion ->
            [Either ObjectVersion ObjectVersion] -> [ObjectVersion]
         mkParents oldToNew parents =
             map
                (\ toOrFromVersion -> case toOrFromVersion of
                   Left toVersion -> toVersion
                   Right fromVersion ->
                      lookupWithDefaultFM
                         oldToNew
                         (error "CopyVersionInfo.mkParents")
                         fromVersion
                   )
                parents

      -- (2) Pass 2.  Set the data.
      -- Here we have a potential problem: someone may be simultaneously
      -- trying to copy the identical version to the repository.  In this
      -- case, it can happen that we try to set a VersionInfo which has
      -- already been set, and ModifyUserInfo will return an IsObjectVersion
      -- for the existing version.
      --     Should that happen, what we do is (1) modify oldToNew1
      -- appropriately; (2) for all versionInfos which we committed with a
      -- parent whose VersionInfo was set elsewhere, recommit with the
      -- parent number correctly.  Thus this will require a third pass.

      let
         -- the first ObjectVersion is the old objectVersion.
         newVersionInfos :: [(ObjectVersion,VersionInfo)]
         newVersionInfos = map
            (\ (toVersion,protoVersion) ->
               let
                  fromVersionInfo = versionInfo1 protoVersion

                  fromUser = user fromVersionInfo
                  toUser = fromUser {
                     version = toVersion,
                     parents = mkParents oldToNew (parents1 protoVersion)
                     }

                  toVersionInfo = fromVersionInfo {user = toUser}
               in
                  (version fromUser,toVersionInfo)
               )
            protoVersions2

         -- If there is no pass 3, newVersionInfosFM will be the
         -- return value.
         newVersionInfosFM :: FiniteMap ObjectVersion VersionInfo
         newVersionInfosFM = listToFM newVersionInfos

         command2 :: SimpleDBCommand
         command2 = MultiCommand
            (map
               (\ (_,newVersionInfo)
                  -> ModifyUserInfo (VersionInfo1 newVersionInfo))
               newVersionInfos
               )

      (MultiResponse responses2) <- queryRepository toRepository command2

      -- (3) Pass 3.  (See comments to Pass 2)
      -- fixups lists pairs in which the first element is the
      -- new version number we tried to give a version, and the second
      -- the version number it turns out already to have.
      (fixups1 :: [Maybe FixOldNew]) <-
         zipWithM
            (\ response (oldVersion,newVersionInfo) -> case response of
               IsOK -> return Nothing
               IsObjectVersion actualNewVersion ->
                  return (Just (FixOldNew {
                     oldVersion = oldVersion,
                     failedNewVersion = version (user (newVersionInfo)),
                     actualNewVersion = actualNewVersion
                     }))
               IsError errorType mess ->
                  throwError ClientError ("Copying failed: " ++ show errorType
                     ++ ": " ++ mess)
               )
            responses2 newVersionInfos

      let
         fixups2 :: [FixOldNew]
         fixups2 = catMaybes fixups1

         -- map from old parent to new parent.
         fixupFM :: FiniteMap ObjectVersion ObjectVersion
         fixupFM = listToFM
            (map
               (\ fixOldNew ->
                  (failedNewVersion fixOldNew,actualNewVersion fixOldNew)
                  )
               fixups2
               )

         -- fixedUpVersionInfos contains the altered version infos which
         -- do not belong to versions which were already entered.
         --
         -- newVersionInfosFM2 contains all the version infos.
         (fixedUpVersionInfos1 :: [VersionInfo],
               newVersionInfosFM2 :: FiniteMap ObjectVersion VersionInfo) =
            foldFM
               (\ oldVersion oldVersionInfo
                     (state0 @ (fixedUpVersionInfos0,fm0)) ->
                  let
                     oldParents :: [ObjectVersion]
                     oldParents = parents (user oldVersionInfo)

                     newParents :: [ObjectVersion]
                     newParents = map
                        (\ oldParent ->
                           lookupWithDefaultFM fixupFM
                              oldParent oldParent
                           )
                        oldParents

                     newVersion1 :: ObjectVersion
                     newVersion1 = version (user oldVersionInfo)

                     newVersion2 :: ObjectVersion
                     newVersion2 = case lookupFM fixupFM newVersion1 of
                        Nothing -> newVersion1
                        Just newVersion2 -> newVersion2

                     newVersionInfo = oldVersionInfo {
                        user = (user oldVersionInfo) {
                           version = newVersion2,
                           parents = newParents
                           }
                        }
                  in
                     if newVersionInfo == oldVersionInfo
                        then
                           state0
                        else
                           (if newVersion1 == newVersion2
                              then
                                 fixedUpVersionInfos1
                              else
                                 newVersionInfo : fixedUpVersionInfos1
                              ,
                              addToFM fm0 oldVersion newVersionInfo
                              )
                  )

               ([],newVersionInfosFM)
               newVersionInfosFM

      case (fixups2,fixedUpVersionInfos1) of
         ([],_) -> return newVersionInfosFM -- no fixups
         (_,[]) -> return newVersionInfosFM2
            -- fixups, but no need to change parent settings in repository.
         (_,_) ->
            do
               let
                  command3 = MultiCommand (
                     map
                        (\ versionInfo
                           -> ModifyUserInfo (VersionInfo1 versionInfo))
                        fixedUpVersionInfos1
                     )

               (MultiResponse responses3)
                  <- queryRepository toRepository command3

               let
                  (responseErrors :: [String]) = map
                     (\ response -> case response of
                        IsOK -> ""
                        IsError errorType mess -> show errorType ++ ": "
                          ++ mess ++ "\n"
                        _ -> "CopyVersionInfo: mysterious error\n"
                        )
                     responses3

               case concat responseErrors of
                  "" -> done
                  errorMess -> throwError ClientError errorMess
               return newVersionInfosFM2

-- Record for a version which needs to be fixed in Pass 3 (see above).
data FixOldNew = FixOldNew {
   oldVersion :: ObjectVersion, -- the version it had in the from repository.
   failedNewVersion :: ObjectVersion,
      -- the version we tried to give it in the new version
   actualNewVersion :: ObjectVersion
      -- the version someone else had already saved it under.
   }
