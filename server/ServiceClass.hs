-- | The ServiceClass class needs to be instanced (at least partially)
-- to define a new service. 
module ServiceClass(
   -- all commented in body of module
   Service(..),
   serviceArg,
   ServiceClass(..),
   ServiceMode(..),
   BackupDelay(..)
   ) where

import Directory

import WBFiles
import CopyFile
import BinaryAll

import Thread

import PasswordFile

data Service = forall inType outType stateType . 
   ServiceClass inType outType stateType => 
   Service (inType,outType,stateType)
   -- The inType,outType and stateType values in Service are never looked
   -- at; this is only a type parameter.  It can be generated from
   -- serviceArg:

serviceArg :: (ServiceClass inType outType stateType) => 
      (inType,outType,stateType)
serviceArg 
   = (error "ServiceClass.1",error "ServiceClass.2",error "ServiceClass.3")

class (HasBinary inType IO,HasBinary outType IO,HasBinary inType StateBinArea,HasBinary outType StateBinArea) =>
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

   serviceMode :: (inType,outType,stateType) -> ServiceMode inType 
      -- see type definition
  
   initialState :: (inType,outType,stateType) -> IO stateType
   -- called when the server is initialised.  May for example
   -- read backups, if there are any.

   handleRequest :: (inType,outType,stateType) 
      -> User -> (inType,stateType) -> IO (outType,stateType)
   -- This handles a particular request from a client, identified by
   -- the given User item.

   getBackupDelay :: (inType,outType,stateType) -> IO BackupDelay
   -- how often to backup - see type definition.  Read when
   -- server is initialised

   backupAction :: (inType,outType,stateType) -> stateType -> IO ()
   -- this is the action to be performed for backups.

   sendOnConnect :: (inType,outType,stateType) -> User -> stateType 
      -> IO String
   -- On connection we send the sendOnConnect string to the client
   -- before anything else.  Defaults to "".  Computing the string
   -- requires the whole service to wait, so it shouldn't take too long.
   sendOnConnect _ _ _ = return ""

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
         copyStringToFile contents filePath

   -- This is the function we actually use on connect.
   sendOnConnectWrapped :: (inType,outType,stateType) -> User -> stateType 
       -> IO WrappedBinary
   sendOnConnectWrapped service user state =
       do
          str <- sendOnConnect service user state
          return (WrappedBinary str)
          
getBackupFile :: ServiceClass inType outType stateType =>
   (inType,outType,stateType) -> IO FilePath
getBackupFile service = getServerFile (serviceId service ++ ".backup")
 
data ServiceMode inType =
      Reply     -- Send output just to client sending input
   |  Broadcast -- Send output to all clients waiting on this service.
   |  BroadcastOther -- Send output to all clients waiting on this server,
                -- except the one which sent the input.
   |  External ( (IO inType -> IO ()) -> IO ())
         -- apologies for the obscenely long type.

         -- The service provider is basically providing a source of "inType"
         --    values.
         -- The server functions make use of this source by registering
         --    a function of type (IO inType -> IO ()); incidentally this 
         --    function is only ever registered once.
         -- When a new "inType" value is about to occur, the service provider
         --    calls the function with the action of type "IO inType" as
         --    argument.  This action will then be invoked before we return.
         -- It is guaranteed that this action of type "IO inType" will not
         --    run simultaneously with sendOnConnect(Wrapped) or handleRequest.
         --    Indeed the function of type "IO inType -> IO ()" will if
         --    necessary block until sendOnConnect(Wrapped) or handleRequest
         --    terminates, before invoking the IO inType action.
                 
data BackupDelay =
      BackupNever
   |  BackupAfter Duration -- wait this long (after previous backup)
   |  BackupEvery Int -- after this number of updates.
