#if (__GLASGOW_HASKELL__ >= 503)
#define NEW_GHC 
#else
#undef NEW_GHC
#endif

#ifndef NEW_GHC
{-# OPTIONS -#include "bdbclient.h" #-}
#endif /* NEW_GHC */

{- Functions we need for calling the Berkeley Database -}
module BDBClient(
   BDB, -- A connection to a Berkeley Database
   openBDB, -- :: IO BDB

   BDBKey, -- Type of record numbers in the database.  Instance of Read/Show.  
   writeBDB, -- :: BDB -> ObjectSource -> IO BDBKey
   readBDB, -- :: BDB -> BDBKey -> IO ObjectSource

   ObjectSource,
   -- type of data as retrieved from the repository.
   exportString, -- :: ObjectSource -> IO String
   -- exportString extracts the contents of the object as a String
   exportFile, -- :: ObjectSource -> FilePath -> IO ()
   -- exportFile writes the contents of the object as a file with the
   -- supplied name, overwriting whatever was there before.
   importString, -- :: String -> IO ObjectSource
   -- importString makes an object with the given contents.
   importFile, -- :: FilePath -> IO ObjectSource
   -- importFile makes an object from the given file.
   ) where

import Foreign
import CString
import CForeign
import Posix

import WBFiles
import QuickReadShow

import FdRead
import CopyFile

-- --------------------------------------------------------------
-- How to create a BDB
-- --------------------------------------------------------------

openBDB :: IO BDB
openBDB =
   do
      serverStringOpt <- getServer
      case serverStringOpt of
         Nothing -> error "BDBClient: --uni-server option is not set"
         Just serverString -> withCString serverString dbConnect

-- --------------------------------------------------------------
-- Writing and Reading from BDB's.
-- --------------------------------------------------------------

writeBDB :: BDB -> ObjectSource -> IO BDBKey
writeBDB bdb (ObjectSource foreignPtr length) =
   alloca 
      (\ recNoPtr ->
         do
            let len = fromIntegral length
            withForeignPtr foreignPtr
               (\ dataPtr -> dbStore bdb dataPtr len recNoPtr)
            word <- peek recNoPtr
            return (BDBKey word)
         )

readBDB :: BDB -> BDBKey -> IO ObjectSource
readBDB bdb (BDBKey word) =
   do
      (cString,len) <- 
         alloca
            (\ (cStringPtr :: Ptr (Ptr CChar)) ->
               do
                  len <-
                     alloca
                        (\ lenPtr ->
                           do
                              dbRetrieve bdb word cStringPtr lenPtr
                              peek lenPtr
                        )
                  cString <- peek cStringPtr
                  return (cString,len)
               )

      foreignPtr <- mkForeignPtr cString
      return (ObjectSource foreignPtr len)

-- --------------------------------------------------------------
-- The ObjectSource type, and functions for it.
-- --------------------------------------------------------------

-- Using the ForeignPtr allows us to attach a free function
-- when the Ptr comes from the C world.
-- The second component is the length.
data ObjectSource = ObjectSource (ForeignPtr CChar) Word32

exportString :: ObjectSource -> IO String
exportString objectSource = exportToCStringLen objectSource peekCStringLen

exportFile :: ObjectSource -> FilePath -> IO ()
exportFile objectSource filePath =
   exportToCStringLen objectSource
      (\ cStringLen -> 
         do
            copyCStringLenToFile cStringLen filePath
         )

exportToCStringLen :: ObjectSource -> (CStringLen -> IO a) -> IO a
exportToCStringLen (ObjectSource foreignPtr length) cStringLenSink =
   do
      let 
         len = fromIntegral length
      withForeignPtr foreignPtr (\ ptr -> cStringLenSink (ptr,len))

importString :: String -> IO ObjectSource
importString str =
   do
      cStringLen <- newCStringLen str
      importCStringLen cStringLen

importFile :: FilePath -> IO ObjectSource
importFile file =
   do
      cStringLen <- copyFileToCStringLen file
      importCStringLen cStringLen

importCStringLen :: CStringLen -> IO ObjectSource
importCStringLen (ptr,len) =
   do
      foreignPtr <- mkForeignPtr ptr
      let
         length = fromIntegral len
      return (ObjectSource foreignPtr length)

-- mkForeignPtr attaches a "free" function to a Ptr.
mkForeignPtr :: Ptr a -> IO (ForeignPtr a)
mkForeignPtr ptr = newForeignPtr ptr (free ptr)
   
-- --------------------------------------------------------------
-- The foreign function interface
-- --------------------------------------------------------------

-- represents C type DB *.  The CChar is immaterial.
newtype DB = DB CChar
type BDB = Ptr DB

newtype BDBKey = BDBKey Word32

#ifndef NEW_GHC

foreign import "db_connect" unsafe dbConnect 
   :: CString -> IO BDB
foreign import "db_store" unsafe dbStore 
   :: BDB -> CString -> Word32 -> Ptr (Word32) -> IO ()
foreign import "db_retrieve" unsafe dbRetrieve 
   :: BDB -> Word32 -> Ptr (CString) -> Ptr (Word32) -> IO ()

#else

foreign import ccall unsafe "bdbclient.h db_connect" dbConnect
   :: CString -> IO BDB
foreign import ccall unsafe "bdbclient.h db_store" dbStore
   :: BDB -> CString -> Word32 -> Ptr (Word32) -> IO ()
foreign import ccall unsafe "bdbclient.h db_retrieve" dbRetrieve
   :: BDB -> Word32 -> Ptr (CString) -> Ptr (Word32) -> IO ()

#endif

-- --------------------------------------------------------------
-- BDBKey as an instance of Read and Show.
-- --------------------------------------------------------------

instance QuickRead BDBKey where
   quickRead = WrapRead (\ key -> BDBKey key)

instance QuickShow BDBKey where
   quickShow = WrapShow (\ (BDBKey key) -> key)