-- | This module contains the functions we need for mapping to and from
-- MMiSSRequest.VersionInfo and VersionInfo.VersionInfo
module MMiSS.API.MapVersionInfo(
   fromOurVersionInfo,
   toOurUserInfo,
   ) where

import Maybe

import Text.XML.HaXml.Xml2Haskell

import Util.ClockTimeToString
import Util.ExtendedPrelude
import Util.AtomString
import Util.Computation
import Util.DeepSeq

import qualified SimpleDB.VersionInfo as VersionInfo

import MMiSS.ImportExportErrors

import MMiSS.API.Request as Request

-- -----------------------------------------------------------------------
-- Converting from our VersionInfo's.
-- -----------------------------------------------------------------------

fromOurVersionInfo :: VersionInfo.VersionInfo -> Request.VersionInfo
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

fromOurUserInfo :: VersionInfo.UserInfo -> Request.UserInfo
fromOurUserInfo user0 =
   let
      userInfoAttrs =
         UserInfo_Attrs {
            userInfoLabel = Just (VersionInfo.label user0),
            userInfoContents = Just (VersionInfo.contents user0),
            userInfoVersion
               = Just (toString (VersionInfo.version user0)),
            userInfoParents
               = Just (unsplitByChar0 ' '
                  (map toString (VersionInfo.parents user0)))
               }
   in
      UserInfo userInfoAttrs (Just (fromOurVersionAttributes
         (VersionInfo.versionAttributes user0)))

-- The first UserInfo is used to fill in unspecified values.
-- seq'ing the return result will force evaluation of anything
-- that might cause an error provoked by this function.
toOurUserInfo :: VersionInfo.UserInfo -> Request.UserInfo
   -> VersionInfo.UserInfo
toOurUserInfo defaultUser (UserInfo userInfoAttrs0 attributesOpt0) =
   let
      label1 = fromMaybe (VersionInfo.label defaultUser)
         (userInfoLabel userInfoAttrs0)

      contents1 = fromMaybe (VersionInfo.contents defaultUser)
         (userInfoContents userInfoAttrs0)

      version1 =
         fromMaybe (VersionInfo.version defaultUser)
            (fmap
               sToV
               (userInfoVersion userInfoAttrs0)
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
               (userInfoParents userInfoAttrs0)
               )

      versionAttributes1 = fromMaybe
         (VersionInfo.versionAttributes defaultUser)
         (do
            attributes0 <- attributesOpt0
            return (toOurVersionAttributes attributes0)
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
            VersionInfo.version = version1,
            VersionInfo.parents = parents1,
            VersionInfo.versionAttributes = versionAttributes1
            })

-- -----------------------------------------------------------------------
-- Converting to and from our VersionAttributes
-- -----------------------------------------------------------------------

fromOurVersionAttributes
   :: VersionInfo.VersionAttributes -> Request.Attributes
fromOurVersionAttributes va =
   let
      kvs :: [(String,String)]
      kvs = VersionInfo.exportVersionAttributes va

      attributes :: [Attribute]
      attributes = map
         (\ (k,v) -> Attribute {attributeKey = k,attributeValue = v})
         kvs
   in
      Attributes attributes

toOurVersionAttributes
   :: Request.Attributes -> VersionInfo.VersionAttributes
toOurVersionAttributes (Attributes attributes) =
   let
      kvs :: [(String,String)]
      kvs = map
         (\ attribute -> (attributeKey attribute,attributeValue attribute))
         attributes
   in
      VersionInfo.importVersionAttributes kvs

-- -----------------------------------------------------------------------
-- Converting from our ServerInfo's.
-- -----------------------------------------------------------------------

fromOurServerInfo :: VersionInfo.ServerInfo -> Request.ServerInfo
fromOurServerInfo server0 =
   ServerInfo {
      serverInfoServerId = VersionInfo.serverId server0,
      serverInfoSerialNo = show (VersionInfo.serialNo server0),
      serverInfoTimeStamp
         = clockTimeToString (VersionInfo.timeStamp server0),
      serverInfoUserId = VersionInfo.userId server0
      }

