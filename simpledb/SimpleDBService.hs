{- This module defines the service that implements the SimpleDB -}
module SimpleDBService(
   simpleDBService,
   simpleDBServiceWrapped,
   ) where

import Thread
import BinaryIO

import ServiceClass

import SimpleDBServer

simpleDBService = serviceArg 
   :: (ReadShow SimpleDBCommand,ReadShow SimpleDBResponse,SimpleDB)
simpleDBServiceWrapped = Service simpleDBService 



instance ServiceClass 
      (ReadShow SimpleDBCommand) (ReadShow SimpleDBResponse) SimpleDB where
   serviceId _ = "SimpleDB"
   serviceMode _ = Reply
   getBackupDelay _ = return (BackupAfter (secs 2.0))
   initialState _ = initialiseSimpleDB
   backupAction _ simpleDB = backupSimpleDB simpleDB
   handleRequest _ _ (ReadShow command,simpleDB) = 
      do
         response <- querySimpleDB simpleDB command
         return (ReadShow response,simpleDB)
   -- For sendOnConnect we use the default action of sending ""


