-- | This implements setting and modifying the user information.
module ModifyUserInfo(
   modifyUserInfo,
   modifyUserInfo1,
   versionInformationToVersion,
   ) where

import Control.Exception

import Computation

import PasswordFile

import BDBOps
import SimpleDBTypes
import VersionInfo
import VersionState
import VersionAllocation

modifyUserInfo :: SimpleDB -> User -> VersionInformation -> IO SimpleDBResponse
modifyUserInfo simpleDB user versionInformation =
   do
      txn <- beginTransaction 
      
      isAlreadyEnteredOpt 
         <- Control.Exception.catch
            (modifyUserInfo1 simpleDB user versionInformation txn)
            (\ exception ->
               do
                  abortTransaction txn
                  Control.Exception.throw exception
               )
      endTransaction txn
      case isAlreadyEnteredOpt of
         Nothing -> return IsOK
         Just oldVersion -> return (IsObjectVersion oldVersion)
         

-- | Return Nothing if user info successfully modified (modulo
-- TXN) or (Right objectVersion) if it couldn't be because the version
-- already exists but with a different version number.  (Something that
-- can occur when we are copying versions between servers.)
--
-- This can also throw an error if the version is new and the user
-- didn't allocate (using 'VersionAllocation.useVersion').
modifyUserInfo1 :: SimpleDB -> User -> VersionInformation -> TXN 
   -> IO (Maybe ObjectVersion)
modifyUserInfo1 simpleDB user versionInformation txn =
   case versionInformation of
      Version1 _ -> return Nothing
      Version1Plus _ _ -> return Nothing
      _ ->
         do
            let
               versionState1 = versionState simpleDB
               objectVersion = versionInformationToVersion versionInformation

            oldVersionOpt <- case versionInformation of
               VersionInfo1 versionInfo -> lookupServerInfo 
                  versionState1 (server versionInfo)
               _ -> return Nothing

            case oldVersionOpt of
               Just oldVersion | oldVersion /= objectVersion ->
                  return (Just oldVersion)
               _ ->
                  do
                     oldVersionInfoOpt <- lookupVersionInfo 
                        versionState1 objectVersion
                     case oldVersionInfoOpt of
                        Nothing -> 
                           do
                              useVersion simpleDB user objectVersion

                              let
                                 mkVersionInfoArg = case versionInformation of
                                    UserInfo1 userInfo -> Left userInfo
                                    VersionInfo1 versionInfo 
                                       -> Right versionInfo
                              versionInfo1 <- mkVersionInfo versionState1 user
                                 mkVersionInfoArg
                              addVersionInfo versionState1 versionInfo1 txn
                        Just versionInfo0 ->
                           do
                              versionInfo1 <- case versionInformation of
                                 UserInfo1 userInfo -> 
                                    changeUserInfo user versionInfo0 
                                       userInfo
                                 VersionInfo1 versionInfo -> 
                                    changeVersionInfo user versionInfo0 
                                       versionInfo
                              addVersionInfo versionState1 versionInfo1 txn
                     return Nothing   


versionInformationToVersion :: VersionInformation -> ObjectVersion
versionInformationToVersion versionInformation = case versionInformation of
   UserInfo1 userInfo -> version userInfo
   VersionInfo1 versionInfo -> version (user versionInfo)
   Version1 objectVersion -> objectVersion
