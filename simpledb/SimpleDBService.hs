{- This module defines the service that implements the SimpleDB -}
module SimpleDBService(
   simpleDBService,
   simpleDBServiceWrapped,
   ) where

import Thread

import ServiceClass

import SimpleDBServer

simpleDBService = serviceArg :: (SimpleDBCommand,SimpleDBResponse,SimpleDB)
simpleDBServiceWrapped = Service simpleDBService 



instance ServiceClass SimpleDBCommand SimpleDBResponse SimpleDB where
   serviceId _ = "SimpleDB"
   serviceMode _ = Reply
   getBackupDelay _ = return (BackupAfter (secs 2.0))
   initialState _ = initialiseSimpleDB
   backupAction _ simpleDB = backupSimpleDB simpleDB
   handleRequest _ (command,simpleDB) = 
      do
         response <- querySimpleDB simpleDB command
         return (response,simpleDB)
   -- For sendOnConnect we use the default action of sending ""


