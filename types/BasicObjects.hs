{- This file describes the basic untyped objects stored in the 
   repository.  Since they are versioned, they need to be
   wrapped in Versioned; EG (Versioned SimpleFile) represents
   a file (with no attributes.
   -}
module BasicObjects(
   SimpleFile, -- a file with no frills. 
      -- SimpleFile is designed to be used as part of other CodedValues,
      -- EG [(String,SimpleFile)] would be a simple directory listing.
      -- SimpleFile is an instance of HasCodedValue.
   newSimpleFile, -- :: View -> IO SimpleFile
   getSimpleFileName, -- :: SimpleFile -> FilePath
      -- This gets the real file name here corresponding to the simple file
      -- (NB - this will be different for different running instances of
      -- the repository!)
   ) where

import Concurrent

import Dynamics
import TempFile

import VersionDB

import CodedValue
import View

data SimpleFile = SimpleFile {
   location :: Location, -- where the real file is stored in CVS
   filePath :: FilePath, -- where it is stored here
   parentVersionMVar :: MVar (Maybe ObjectVersion) -- parent version, if any.
   }

-- ------------------------------------------------------------------------
-- Creating new values
-- ------------------------------------------------------------------------

newSimpleFile :: View -> IO SimpleFile
newSimpleFile view =
   do
      let repository = getRepository view
      filePath <- newTempFile
      location <- newLocation repository
      parentVersionMVar <- newMVar Nothing
      return (SimpleFile {
         location = location,
         filePath = filePath,
         parentVersionMVar = parentVersionMVar
         })

getSimpleFileName :: SimpleFile -> FilePath
getSimpleFileName simpleFile = filePath simpleFile


-- ------------------------------------------------------------------------
-- Instances
-- ------------------------------------------------------------------------

simpleFile_tyCon = mkTyCon "BasicObjects" "SimpleFile"

instance HasTyCon SimpleFile where
   tyCon _ = simpleFile_tyCon

instance HasCodedValue SimpleFile where
   -- We represent the file as a pair (Location,ObjectVersion)
   encodeIO (SimpleFile {location = location,filePath = filePath,
         parentVersionMVar = parentVersionMVar}) codedValue0 view =
      do
         let repository = getRepository view
         parentVersionOpt <- takeMVar parentVersionMVar
         objectSource <- importFile filePath
         objectVersion <- commit repository objectSource location 
            parentVersionOpt
         putMVar parentVersionMVar (Just objectVersion)
         encodeIO (location,objectVersion) codedValue0 view
   decodeIO codedValue0 view =
      do
         ((location,objectVersion),codedValue1) <- decodeIO codedValue0 view
         filePath <- newTempFile
         let repository = getRepository view
         retrieveFile repository location objectVersion filePath
         parentVersionMVar <- newMVar (Just objectVersion)
         return (SimpleFile {
            location = location,
            filePath = filePath,
            parentVersionMVar = parentVersionMVar
            },codedValue1)
