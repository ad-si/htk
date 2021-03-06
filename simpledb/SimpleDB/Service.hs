-- | This module defines the service that implements the SimpleDB for
-- an external server.
module SimpleDB.Service(
   simpleDBService,
   simpleDBServiceWrapped,
   mkSimpleDBServices,
   ) where

import Util.Thread

import Server.ServiceClass

import SimpleDB.Server

import SimpleDB.VersionInfoService
import SimpleDB.VersionState
import SimpleDB.VersionAllocation(forgetUsersVersions)
import SimpleDB.LocationAllocation(forgetUsersLocations)

mkSimpleDBServices :: IO [Service]
mkSimpleDBServices =
   do
      versionState <- mkVersionState False

      simpleDB <- openSimpleDB versionState
      let
         simpleDBService :: (SimpleDBCommand,SimpleDBResponse,SimpleDB)
         simpleDBService =
            (error "SimpleDBService.1",error "SimpleDBService.2",simpleDB)

      return [Service simpleDBService,toVersionInfoServiceWrapped versionState]


simpleDBService :: (SimpleDBCommand,SimpleDBResponse,SimpleDB)
simpleDBService = serviceArg

simpleDBServiceWrapped :: Service
simpleDBServiceWrapped = Service simpleDBService



instance ServiceClass SimpleDBCommand SimpleDBResponse SimpleDB where
   serviceId _ = "SimpleDB"
   serviceMode _ = Reply
   getBackupDelay _ = return BackupNever
      -- SimpleDB backups up automatically after every commit anyway.
   initialState (_,_,simpleDB) = return simpleDB
   handleRequest _ user (command,simpleDB) =
      do
         response <- querySimpleDB user simpleDB command
         return (response,simpleDB)
   handleClientDisconnect _ user simpleDB =
      do
         forgetUsersVersions simpleDB user
         forgetUsersLocations simpleDB user
         return simpleDB
   -- For sendOnConnect we use the default action of sending ""


