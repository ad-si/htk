{- The AllocateService provides an implementation of the 
   ServiceClass type allocating new (CVSFile,CVSVersion) pairs.

   TBD: add backuping server and recovery.

   -}
module AllocateService(
   allocateService,
   allocateServiceWrapped,
   AllocateRequest(..),
   AllocateAnswer(..)      
   ) where

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
      let
         (newState,cvsFile) = newCVSFile allocateState
      in
         return (NewCVSFile cvsFile,newState)
   handleRequest _ (GetNewCVSVersion cvsFile oldCVSVersion,allocateState) =
      let
         (newState,cvsVersion) = 
            newCVSVersion allocateState cvsFile oldCVSVersion
      in
         return (NewCVSVersion cvsVersion,newState)

   getBackupDelay _ = return BackupNever

