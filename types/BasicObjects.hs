{- This file describes the basic untyped objects stored in the 
   repository.  Since they are versioned, they need to be
   wrapped in Versioned; EG (Versioned SimpleFile) represents
   a file (with no attributes.
   -}
module BasicObjects(
   SimpleFile, -- a file with no frills. 
      -- SimpleFile is designed to be used as part of other CodedValues,
      -- EG [(String,SimpleFile)] would be a simple directory listing.
      -- SimpleFile is an instance of HasCodedValue and HasFilePath
   newSimpleFile, -- :: View -> IO SimpleFile

   HasFilePath(toFilePath),
      -- class of things with an associated file path.

   Attributes, -- a set of variables keyed by String's.
      -- Attributes is an instance of HasCodedValue (so you can
      -- read and write them).
      -- To access them, we have
      --    instance KeyOpsRegistry Attributes String
      -- and
      --    instance HasCodedValue to => GetSetRegistry Attributes String to

   newEmptyAttributes, -- :: View -> IO Attributes

   ) where

import Concurrent

import Dynamics
import TempFile
import Registry

import VersionDB

import CodedValue
import CodedValueStore
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
-- HasFilePath
-- ------------------------------------------------------------------------

class HasFilePath fileItem where
   toFilePath :: fileItem -> FilePath

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

instance HasFilePath SimpleFile where
   toFilePath simpleFile = filePath simpleFile

-- ------------------------------------------------------------------------
-- Attributes
-- ------------------------------------------------------------------------

data Attributes = Attributes {
   view :: View,
   registry :: Registry String CodedValue
   }

newEmptyAttributes :: View -> IO Attributes
newEmptyAttributes view =
   do
      registry <- newRegistry
      return (Attributes {view = view,registry = registry})

attributes_tyCon = mkTyCon "BasicObjects" "Attributes"

instance HasTyCon Attributes where
   tyCon _ = attributes_tyCon

instance HasCodedValue Attributes where
   encodeIO (Attributes {registry = registry}) codedValue0 view =
      do
         contents <- listRegistryContents registry
         codedValue1 <- encodeIO contents codedValue0 view
         return codedValue1

   decodeIO codedValue0 view =
      do
         (contents,codedValue1) <- decodeIO codedValue0 view
         registry <- listToNewRegistry contents
         let attributes = Attributes {view = view,registry = registry}
         return (attributes,codedValue1)

instance HasCodedValue to => GetSetRegistry Attributes String to where
   transformValue (Attributes{view = view,registry = registry}) from
         transformer =
      let
         transformIn Nothing = return Nothing
         transformIn (Just codedValue) =
            do
               toVal <- doDecodeIO codedValue view
               return (Just toVal)
         transformOut Nothing = return Nothing
         transformOut (Just toVal) =
            do
               codedVal <- doEncodeIO toVal view
               return (Just codedVal)
      in
         transformValue registry from (\ codedValueInOpt ->
            do
               toValInOpt <- transformIn codedValueInOpt
               (toValOutOpt,extra) <- transformer toValInOpt
               codedValueOutOpt <- transformOut toValOutOpt
               return (codedValueOutOpt,extra)
            )

   getValueOpt (Attributes{view = view,registry = registry}) from =
      do
         codedValueOpt <- getValueOpt registry from
         case codedValueOpt of
            Nothing -> return Nothing
            Just codedValue ->
               do
                  toVal <- doDecodeIO codedValue view
                  return (Just toVal)
            
   setValue (Attributes{view = view,registry = registry}) from to =
      do
         codedValue <- doEncodeIO to view
         setValue registry from codedValue

instance KeyOpsRegistry Attributes String where
   deleteFromRegistryBool (Attributes {registry = registry}) from =
      deleteFromRegistryBool registry from

   deleteFromRegistry (Attributes {registry = registry}) from =
      deleteFromRegistry registry from

   listKeys (Attributes {registry = registry}) = listKeys registry
