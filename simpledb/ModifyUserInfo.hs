-- | This implements setting and modifying the VersionInfo\/UserInfo 
-- information.
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
import ServerErrors

modifyUserInfo :: SimpleDB -> User -> VersionInformation -> IO SimpleDBResponse
modifyUserInfo simpleDB user versionInformation =
   do
      txn <- beginTransaction 
      
      isAlreadyEnteredOpt 
         <- Control.Exception.catch
            (modifyUserInfo1 simpleDB user versionInformation txn False)
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
modifyUserInfo1 :: 
   SimpleDB -> User -> VersionInformation -> TXN 
   -> Bool -- ^ True if we are inside a commit.
   -> IO (Maybe ObjectVersion)
modifyUserInfo1 simpleDB user versionInformation txn insideCommit =
   case versionInformation of
      Version1 _ -> noInfoSupplied
      Version1Plus _ _ -> noInfoSupplied
      _ ->
         do
            oldVersionOpt <- case versionInformation of
               VersionInfo1 versionInfo -> lookupServerInfo 
                  versionState1 (server versionInfo)
               _ -> return Nothing

            case oldVersionOpt of
               Just oldVersion | oldVersion /= objectVersion ->
                  return (Just oldVersion)
               _ ->
                  do
                     modifyUserInfo2 
                        (\ oldVersionInfoOpt -> case oldVersionInfoOpt of
                           Nothing -> 
                              do
                                 useVersion simpleDB user objectVersion

                                 let
                                    mkVersionInfoArg = 
                                       case versionInformation of
                                          UserInfo1 userInfo -> Left userInfo
                                          VersionInfo1 versionInfo 
                                             -> Right versionInfo
                                 versionInfo1 <- mkVersionInfo versionState1 
                                    user mkVersionInfoArg
                                 return (versionInfo1 {
                                    isPresent = insideCommit})
                           Just versionInfo0 ->
                              do
                                 versionInfo1 <- case versionInformation of
                                    UserInfo1 userInfo -> 
                                       changeUserInfo user versionInfo0 
                                          userInfo
                                    VersionInfo1 versionInfo -> 
                                       changeVersionInfo user versionInfo0 
                                          versionInfo
                                 let
                                    isPresent1 = 
                                       isPresent versionInfo1 || insideCommit
                                    
                                 return (versionInfo1 {
                                    isPresent = isPresent1}) 
                           )
                     return Nothing  
   where
      versionState1 :: VersionState
      versionState1 = versionState simpleDB

      objectVersion :: ObjectVersion
      objectVersion = versionInformationToVersion versionInformation

      noInfoSupplied :: IO (Maybe ObjectVersion)
      noInfoSupplied =
         do
            if insideCommit
               then
                  commitUserInfo
               else
                  done
            return Nothing

      commitUserInfo :: IO ()
      commitUserInfo = modifyUserInfo2 
         (\ oldVersionInfoOpt -> case oldVersionInfoOpt of
            Nothing -> throwError InternalError
               "Commit of version without any known VersionInfo"
               -- querySimpleDB is supposed to have checked this. 
            Just versionInfo -> return (versionInfo {isPresent = True})
            )


      modifyUserInfo2 :: (Maybe VersionInfo -> IO VersionInfo) -> IO ()
      modifyUserInfo2 updateAct =
         do
            oldVersionInfoOpt <- lookupVersionInfo versionState1 objectVersion
            newVersionInfo <- updateAct oldVersionInfoOpt
            addVersionInfo versionState1 newVersionInfo txn



versionInformationToVersion :: VersionInformation -> ObjectVersion
versionInformationToVersion versionInformation = case versionInformation of
   UserInfo1 userInfo -> version userInfo
   VersionInfo1 versionInfo -> version (user versionInfo)
   Version1 objectVersion -> objectVersion
   Version1Plus objectVersion _ -> objectVersion
