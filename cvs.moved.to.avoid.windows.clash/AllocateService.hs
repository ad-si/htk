{- The AllocateService provides an implementation of the 
   ServiceClass type allocating new (CVSFile,CVSVersion) pairs.
   -}
module AllocateService(
   allocateService,
   allocateServiceWrapped,
   AllocateRequest(..),
   AllocateAnswer(..)      
   ) where

import Debug(debug)

import ServiceClass

import CVSTypes
import Allocate

allocateService = serviceArg :: (AllocateRequest,AllocateAnswer,AllocateState)
allocateServiceWrapped = Service allocateService

data AllocateRequest = 
      GetNewCVSFile 
   |  GetNewCVSVersion CVSFile CVSVersion
   deriving (Read,Show)

data AllocateAnswer =
      NewCVSFile CVSFile
   |  NewCVSVersion CVSVersion
   deriving (Read,Show) 

instance ServiceClass AllocateRequest AllocateAnswer AllocateState where
   serviceId _ = "Allocate"
   
   serviceMode _ = Reply

   initialState _ =return  initialAllocateState

   handleRequest _ (GetNewCVSFile,allocateState) =
      do
         debug "newCVSFile request received"
         let
            (newState,cvsFile) = newCVSFile allocateState
         return (NewCVSFile cvsFile,newState)
   handleRequest _ (GetNewCVSVersion cvsFile oldCVSVersion,allocateState) =
      do
         debug "newCVSVersion request received"
         let
            (newState,cvsVersion) = 
               newCVSVersion allocateState cvsFile oldCVSVersion
         return (NewCVSVersion cvsVersion,newState)

   initialStateFromString _ Nothing = return initialAllocateState
   initialStateFromString _ (Just allocateStr) = return (read allocateStr)

   backupToString _ state = return (show state)

   getBackupDelay _ = return (BackupEvery 1)

