-- | This module defines the service that implements the SimpleDB for 
-- an external server. 
module SimpleDBService(
   simpleDBService,
   simpleDBServiceWrapped,
   mkSimpleDBServices,
   ) where

import Thread

import ServiceClass

import SimpleDBServer

import VersionInfoService
import VersionState

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
   -- For sendOnConnect we use the default action of sending ""


