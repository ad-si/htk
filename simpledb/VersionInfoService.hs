module VersionInfoService(
   toVersionInfoServiceWrapped, -- :: VersionState -> Service
   versionInfoService, -- use only for calling the service!
   ) where

import BinaryAll

import ServiceClass

import VersionInfo

-- This service is unusual as it needs to be created dynamically.

toVersionInfoServiceWrapped :: VersionState -> Service
toVersionInfoServiceWrapped versionState =
   Service (
      error "VI1" :: (Bool,VersionInfo),
      error "VI2" :: (Bool,VersionInfo),
      versionState :: VersionState
      )

versionInfoService = serviceArg :: ((Bool,VersionInfo),(Bool,VersionInfo),
   VersionState)
-- For the meaning of the Bools see addVersionInfo in VersionInfo.


instance ServiceClass (Bool,VersionInfo) (Bool,VersionInfo) VersionState where

   serviceId _ = "VersionInfo"

   serviceMode (_,_,versionState) = External (registerAct versionState)

   initialState (_,_,versionState) = return versionState

   handleRequest _ _ vv = return vv

   getBackupDelay _ =  return BackupNever

   -- We send the VersionInfos, in the order in which they must be applied.
   sendOnConnectWrapped _ _ versionState =
      do
         versionInfos <- getVersionInfos versionState
         return (WrappedBinary versionInfos)
