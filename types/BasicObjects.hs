-- | This file describes the basic untyped objects stored in the 
-- repository.  Since they are versioned, they need to be
-- wrapped in Versioned; EG (Versioned SimpleFile) represents
-- a file (with no attributes.
-- 
-- Attributes describes a set of variables keyed by Strings.
-- We also implement an AttributesType type which describes the
-- types of the variables in an Attributes.
module BasicObjects(
   SimpleFile, -- a file with no frills. 
      -- SimpleFile is designed to be used as part of other CodedValues,
      -- EG [(String,SimpleFile)] would be a simple directory listing.
      -- SimpleFile is an instance of HasCodedValue and HasFilePath
   newSimpleFile, -- :: View -> IO SimpleFile

   HasFilePath(toFilePath),
      -- class of things with an associated file path.

   HasContents(..),
      -- class of things which can be represented by an ICStringLen.


   Attributes, -- a set of variables keyed by String's.
      -- Attributes is an instance of HasCodedValue (so you can
      -- read and write them).
      -- To access them, we have
      --    instance KeyOpsRegistry Attributes String
      -- and
      --    instance HasCodedValue to => GetSetRegistry Attributes String to

   newEmptyAttributes, -- :: View -> IO Attributes

   HasAttributes(..), -- a class for things containing a set of attributes

   getMergeLinksSimpleFile, -- :: SimpleFile -> ObjectLinks ()
   attemptMergeSimpleFile, 
      -- :: MergeTypes.LinkReAssigner -> View -> SimpleFile -> SimpleFile

   ) where

import IO

import TempFile
import Registry
import ICStringLen

import CopyFile

import CodedValue
import Link
import MergeTypes
import ViewType

newtype SimpleFile = SimpleFile {
   filePath :: FilePath -- where it is stored here
   } 

-- ------------------------------------------------------------------------
-- Creating new values
-- ------------------------------------------------------------------------

newSimpleFile :: View -> IO SimpleFile
newSimpleFile view =
   do
      filePath <- newTempFile
      writeFile filePath ""
      return (SimpleFile {
         filePath = filePath
         })


{-
getSimpleFileName :: SimpleFile -> FilePath
getSimpleFileName simpleFile = filePath simpleFile
-}

-- ------------------------------------------------------------------------
-- HasFilePath
-- ------------------------------------------------------------------------

class HasFilePath fileItem where
   toFilePath :: fileItem -> FilePath

-- ------------------------------------------------------------------------
-- HasContents
-- ------------------------------------------------------------------------

class HasContents fileItem where
   getAsICSL :: View -> fileItem -> IO ICStringLen

-- ------------------------------------------------------------------------
-- Instances
-- ------------------------------------------------------------------------

instance HasBinary SimpleFile CodingMonad where
   writeBin = mapWriteIO 
      (\ simpleFile -> copyFileToICStringLen (filePath simpleFile))
   readBin = mapReadIO 
      (\ icsl ->
         do
            filePath <- newTempFile
            copyICStringLenToFile icsl filePath
            return (SimpleFile {filePath = filePath})
         )

instance HasFilePath SimpleFile where
   toFilePath simpleFile = filePath simpleFile

instance HasContents SimpleFile where
   getAsICSL _ simpleFile = copyFileToICStringLen (filePath simpleFile)
      

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

instance HasBinary Attributes CodingMonad where
   writeBin = mapWriteIO
      (\ attributes -> listRegistryContents (registry attributes))
   readBin = mapReadViewIO
      (\ view contents -> 
         do
            registry <- listToNewRegistry contents
            let 
               attributes = Attributes {
                  view = view,
                  registry = registry
                  }
            return attributes
         )

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

-- ------------------------------------------------------------------------
-- HasAttributes
-- ------------------------------------------------------------------------

-- | The HasAttributes class is instanced by objects that contain an
-- Attributes 
class HasCodedValue object => HasAttributes object where
   ---
   -- readAttributes extracts the attributes for an object
   readAttributes :: View -> Link object -> IO Attributes

   ---
   -- readPrimAttributes does this directly from the object..
   readPrimAttributes :: object -> Attributes

   readAttributes view link = 
      do
         versioned <- fetchLink view link
         object <- readObject view versioned
         return (readPrimAttributes object)
 
-- ------------------------------------------------------------------------
-- SimpleFile's and merging.
-- It is not possible to merge two distinct SimpleFile's.  Furthermore the
-- data inside a SimpleFile is all inside a containing File.  Thus we 
-- implement just two trivial functions.
--
-- The location inside a SimpleFile is not touched by the merging procedure,
-- and never changed.
--
-- Thus at the moment these functions are basically just stubs, which may
-- be made less trivial should SimpleFile's be made more useful.
-- ------------------------------------------------------------------------

getMergeLinksSimpleFile :: SimpleFile -> ObjectLinks ()
getMergeLinksSimpleFile _ = ObjectLinks []

-- | Change the link inside a SimpleFile
attemptMergeSimpleFile :: MergeTypes.LinkReAssigner -> View -> SimpleFile 
   -> SimpleFile
attemptMergeSimpleFile linkReAssigner view simpleFile = simpleFile

