{- Functions we need for calling the Berkeley Database -}
module BDBClient(
   BDB, -- A connection to a Berkeley Database
   openBDB, -- :: IO BDB

   BDBKey, -- Type of record numbers in the database.
           -- Instance of HasBinaryIO, Eq, Ord.
   TXN, -- Handle to a transaction in the database.

   writeBDB, -- :: BDB -> TXN -> ICStringLen -> IO BDBKey
   readBDB, -- :: BDB -> BDBKey -> IO (Maybe ICStringLen)
      -- Nothing indicates that the key wasn't found.
   flushBDB, -- :: BDB -> IO ()

   beginTransaction, -- :: IO TXN
   endTransaction, -- :: TXN -> IO ()
   abortTransaction, -- :: TXN -> IO ()
   ) where

import System.IO.Unsafe
import Data.Word

import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types

import WBFiles
import ICStringLen
import BinaryIO

import BSem

-- --------------------------------------------------------------
-- How to create a BDB
-- --------------------------------------------------------------

openBDB :: IO BDB
openBDB = 
   do
      serverDir <- getServerDir
      withCString serverDir (\ serverDirCString 
         -> dbConnect serverDirCString
         )

-- --------------------------------------------------------------
-- Writing and Reading from BDB's.
-- --------------------------------------------------------------

writeBDB :: BDB -> TXN -> ICStringLen -> IO BDBKey
writeBDB bdb txn icsl =
   withICStringLen icsl
      (\ len dataPtr ->
         alloca (\ (recNoPtr :: Ptr Word32) ->
            do
               dbStore bdb txn dataPtr (fromIntegral len) recNoPtr
               
               key <- peek recNoPtr
               return (BDBKey key)
            )
         )


readBDB :: BDB -> BDBKey -> IO (Maybe ICStringLen)
readBDB bdb (BDBKey key) =
   synchronize readBDBLock (
      alloca (\ (cStringPtr :: Ptr (Ptr CChar)) ->
         do
            len <- alloca
               (\ (lenPtr :: Ptr Word32) ->
                  do
                     dbRetrieve bdb key cStringPtr lenPtr
                     peek lenPtr
                  )
            temporaryData <- peek cStringPtr

            if temporaryData == nullPtr 
               then
                  return Nothing
               else
                  do
                     let
                        lenInt = fromIntegral len

                     cStringLen <- mkICStringLen lenInt
                        (\ permanentData ->
                           copyArray permanentData temporaryData lenInt
                           )
                     return (Just cStringLen) 
         )
      )

readBDBLock :: BSem
readBDBLock = unsafePerformIO newBSem
{-# NOINLINE readBDBLock #-}
    
-- --------------------------------------------------------------
-- The foreign function interface
-- --------------------------------------------------------------

-- represents C type DB *.  The CChar is immaterial.
newtype DB = DB CChar
type BDB = Ptr DB

newtype BDBKey = BDBKey Word32 deriving (HasBinaryIO,Eq,Ord,Show)

foreign import ccall unsafe "bdbclient.h db_connect" dbConnect
   :: CString -> IO BDB
foreign import ccall unsafe "bdbclient.h db_store" dbStore
   :: BDB -> TXN -> CString -> Word32 -> Ptr (Word32) -> IO ()
foreign import ccall unsafe "bdbclient.h db_retrieve" dbRetrieve
   :: BDB -> Word32 -> Ptr (CString) -> Ptr (Word32) -> IO ()
foreign import ccall unsafe "bdbclient.h db_flush" flushBDB
   :: BDB -> IO ()

-- --------------------------------------------------------------
-- FFI (transactions)
-- --------------------------------------------------------------

-- represents C type DB_TXN *, like (B)DB types
newtype XN = TXN CChar
type TXN = Ptr XN

foreign import ccall unsafe "bdbclient.h db_begin_trans" beginTransaction
   :: IO TXN

foreign import ccall unsafe "bdbclient.h db_end_trans" endTransaction
   :: TXN -> IO ()

foreign import ccall unsafe "bdbclient.h db_abort_trans" abortTransaction
   :: TXN -> IO ()
