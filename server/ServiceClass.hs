{- The ServiceClass class needs to be instanced (at least partially)
   to define a new service. -}
module ServiceClass(
   -- all commented in body of module
   Service(..),
   serviceArg,
   ServiceClass(..),
   ServiceMode(..),
   BackupDelay(..)
   ) where

import Directory

import FileNames
import WBFiles
import CopyFile

import Thread

data Service = forall inType outType stateType . 
   ServiceClass inType outType stateType => 
   Service (inType,outType,stateType)
   -- The inType,outType and stateType values in Service are never looked
   -- at; this is only a type parameter.  It can be generated from
   -- serviceArg:

serviceArg :: (ServiceClass inType outType stateType) => 
      (inType,outType,stateType)
serviceArg = serviceArg

class (Read inType,Show inType,Read outType,Show outType) =>
-- inType is input to requests
-- outType is output to requests.
-- stateType is state.  This is shared between all clients of this
--    service on this server.
   ServiceClass inType outType stateType where
-- Because of Haskell's type system all functions need to include all
-- of inType/outType/stateType in either argument or result.
-- We achieve this by including a phony parameter at the start
-- of each function with type (inType,outType,stateType); this
-- parameter may as well be _|_

   serviceId :: (inType,outType,stateType) -> String
   -- this is used to identify the service to the socket after a
   -- socket is opened.  The String should be unique to the
   -- service and should not contain a newline.

   serviceMode :: (inType,outType,stateType) -> ServiceMode 
      -- see type definition
  
   initialState :: (inType,outType,stateType) -> IO stateType
   -- called when the server is initialised.  May for example
   -- read backups, if there are any.

   handleRequest :: (inType,outType,stateType) -> 
      (inType,stateType) -> IO (outType,stateType)
   -- This handles a particular request from a client.

   getBackupDelay :: (inType,outType,stateType) -> IO BackupDelay
   -- how often to backup - see type definition.  Read when
   -- server is initialised

   backupAction :: (inType,outType,stateType) -> stateType -> IO ()
   -- this is the action to be performed for backups.

   sendOnConnect :: (inType,outType,stateType) -> stateType -> IO String
   -- On connection we send the sendOnConnect string to the client
   -- before anything else.  Defaults to "".  Computing the string
   -- requires the whole service to wait, so it shouldn't take too long.
   sendOnConnect _ _ = return ""

   -- If the following functions are defined, the initialState and
   -- backupAction are automatically defined, to restore and save the
   -- state from a file obtained by a file of name serviceId in the
   -- directory backupDir (see WBFiles.hs).
   initialStateFromString :: (inType,outType,stateType) -> Maybe String 
      -> IO stateType
    -- Nothing corresponds to no backup file and presumably initialisation.

   backupToString :: (inType,outType,stateType) -> stateType -> IO String

   initialState service =
      do
         filePath <- getBackupFile service
         exists <- doesFileExist filePath
         contentsOpt <-
            if exists
               then
                  do
                     contents <- copyFileToString filePath
                     return (Just contents)
               else
                  return Nothing
         initialStateFromString service contentsOpt

   backupAction service state =
      do
         contents <- backupToString service state
         filePath <- getBackupFile service
         writeFile filePath contents
          
getBackupFile :: ServiceClass inType outType stateType =>
   (inType,outType,stateType) -> IO FilePath
getBackupFile service =
   do
      backupDir <- getBackupDir
      return (combineNames (trimDir backupDir) ((serviceId service)
         ++".backup"))
 
data ServiceMode =
      Reply     -- Send output just to client sending input
   |  Broadcast -- Send output to all clients waiting on this service.
   |  BroadcastOther -- Send output to all clients waiting on this server,
                -- except the one which sent the input.

data BackupDelay =
      BackupNever
   |  BackupAfter Duration -- wait this long (after previous backup)
   |  BackupEvery Int -- after this number of updates.
