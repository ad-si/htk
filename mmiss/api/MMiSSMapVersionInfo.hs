-- | This module contains the functions we need for mapping to and from 
-- MMiSSRequest.VersionInfo and VersionInfo.VersionInfo 
module MMiSSMapVersionInfo(
   fromOurVersionInfo,
   toOurUserInfo,
   ) where

import Maybe

import Text.XML.HaXml.Xml2Haskell

import ClockTimeToString
import ExtendedPrelude
import AtomString
import Computation
import DeepSeq

import qualified VersionInfo

import XmlExtras

import MMiSSImportExportErrors

import MMiSSRequest

-- -----------------------------------------------------------------------
-- Converting from our VersionInfo's.
-- -----------------------------------------------------------------------

fromOurVersionInfo :: VersionInfo.VersionInfo -> MMiSSRequest.VersionInfo
fromOurVersionInfo versionInfo0 =
   let
      versionInfoIsPresent1 = 
         if VersionInfo.isPresent versionInfo0
            then
               Default VersionInfo_isPresent_present
            else
               NonDefault VersionInfo_isPresent_absent
   in
      VersionInfo 
         (VersionInfo_Attrs {
            versionInfoIsPresent = versionInfoIsPresent1})
         (fromOurUserInfo (VersionInfo.user versionInfo0))
         (fromOurServerInfo (VersionInfo.server versionInfo0))

-- -----------------------------------------------------------------------
-- Converting to and from our UserInfo's.
-- -----------------------------------------------------------------------

fromOurUserInfo :: VersionInfo.UserInfo -> MMiSSRequest.UserInfo
fromOurUserInfo user0 =
   let
      private1 = if VersionInfo.private user0
         then
            NonDefault UserInfo_private_noAutoExport
         else
            Default UserInfo_private_autoExport
   in
      UserInfo {
         userInfoLabel = Just (VersionInfo.label user0),
         userInfoContents = Just (VersionInfo.contents user0),
         userInfoPrivate = private1,
         userInfoVersion 
            = Just (toString (VersionInfo.version user0)),
         userInfoParents 
            = Just (unsplitByChar0 ' '
               (map toString (VersionInfo.parents user0)))
            }

-- The first UserInfo is used to fill in unspecified values.
-- seq'ing the return result will force evaluation of anything
-- that might cause an error provoked by this function.
toOurUserInfo :: VersionInfo.UserInfo -> MMiSSRequest.UserInfo 
   -> VersionInfo.UserInfo 
toOurUserInfo defaultUser user0 =
   let
      label1 = fromMaybe (VersionInfo.label defaultUser) (userInfoLabel user0)

      contents1 = fromMaybe (VersionInfo.contents defaultUser) 
         (userInfoContents user0)

      private1 = case fromDefaultable (userInfoPrivate user0) of
         UserInfo_private_autoExport -> False
         UserInfo_private_noAutoExport -> True

      version1 = 
         fromMaybe (VersionInfo.version defaultUser)
            (fmap
               sToV
               (userInfoVersion user0)
            )

      parents1 =
         fromMaybe (VersionInfo.parents defaultUser)
            (fmap
               (\ parentStr ->
                  let
                     parentStrs = filter (/= "") (splitByChar ' ' parentStr)
                  in 
                     map sToV parentStrs
                  )
               (userInfoParents user0)
               )

      sToV :: String -> VersionInfo.ObjectVersion
      sToV s = case fromWithError (fromStringWE s) of
         Left mess -> importExportError (show s ++ " is not a valid version")
         Right v -> v

   in
      (version1 : parents1) `deepSeq`
         (VersionInfo.UserInfo {
            VersionInfo.label = label1,
            VersionInfo.contents = contents1,
            VersionInfo.private = private1,
            VersionInfo.version = version1,
            VersionInfo.parents = parents1
            })

-- -----------------------------------------------------------------------
-- Converting from our ServerInfo's.
-- -----------------------------------------------------------------------

fromOurServerInfo :: VersionInfo.ServerInfo -> MMiSSRequest.ServerInfo
fromOurServerInfo server0 =
   ServerInfo {
      serverInfoServerId = VersionInfo.serverId server0,
      serverInfoSerialNo = show (VersionInfo.serialNo server0),
      serverInfoTimeStamp 
         = clockTimeToString (VersionInfo.timeStamp server0),
      serverInfoUserId = VersionInfo.userId server0
      }

