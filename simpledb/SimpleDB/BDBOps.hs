-- |
-- Description: Interface to Berkeley Database
--
-- Functions we need for calling the Berkeley Database
module SimpleDB.BDBOps(
   BDB, -- A connection to a Berkeley Database
   openBDB, -- :: String -> IO BDB

   BDBKey, -- Type of record numbers in the database.
           -- Instance of HasBinaryIO, Eq, Ord, Integral.
   TXN, -- Handle to a transaction in the database.

   writeBDB, -- :: BDB -> TXN -> ICStringLen -> IO BDBKey
   writeBDBHere, -- :: BDB -> TXN -> BDBKey -> ICStringLen -> IO ()
   readBDB, -- :: BDB -> BDBKey -> IO (Maybe ICStringLen)
      -- Nothing indicates that the key wasn't found.
   flushBDB, -- :: BDB -> IO ()

   beginTransaction, -- :: IO TXN
   endTransaction, -- :: TXN -> IO ()
   abortTransaction, -- :: TXN -> IO ()

   -- * Cursors
   Cursor,
   mkCursor, -- :: BDB -> IO Cursor
   readBDBAtCursor, -- :: Cursor -> IO (Maybe (BDBKey,ICStringLen))
   closeCursor, -- :: Cursor -> IO ()

   ) where

import System.IO.Unsafe
import Foreign.C.Types

import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types

import Util.WBFiles
import Util.ICStringLen
import Util.BinaryAll

import Events.Synchronized

import Reactor.BSem

-- --------------------------------------------------------------
-- How to create a BDB
-- --------------------------------------------------------------

openBDB :: String -> IO BDB
openBDB dbName =
   do
      serverDir <- getServerDir
      withCString serverDir (\ serverDirCString
         -> withCString dbName (\ dbNameCString
            -> dbConnect serverDirCString dbNameCString
            )
         )

-- --------------------------------------------------------------
-- Writing and Reading from BDB's.
-- --------------------------------------------------------------

writeBDB :: BDB -> TXN -> ICStringLen -> IO BDBKey
writeBDB bdb txn icsl =
   withICStringLen icsl
      (\ len dataPtr ->
         alloca (\ (recNoPtr :: Ptr CSize) ->
            do
               dbStore bdb txn dataPtr (fromIntegral len) recNoPtr

               key <- peek recNoPtr
               return (BDBKey key)
            )
         )

writeBDBHere :: BDB -> TXN -> BDBKey -> ICStringLen -> IO ()
writeBDBHere bdb txn (BDBKey key) icsl =
   withICStringLen icsl
      (\ len dataPtr ->
         dbStoreHere bdb txn dataPtr (fromIntegral len) key
         )


readBDB :: BDB -> BDBKey -> IO (Maybe ICStringLen)
readBDB bdb (BDBKey key) =
   synchronize readLock (
      alloca (\ (cStringPtr :: Ptr (Ptr CChar)) ->
         do
            len <- alloca
               (\ (lenPtr :: Ptr CSize) ->
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

-- --------------------------------------------------------------
-- Cursor operations.
-- --------------------------------------------------------------

mkCursor :: BDB -> IO Cursor
mkCursor bdb =
   alloca (\ cursorPtr ->
      do
         mkCursor0 bdb cursorPtr
         peek cursorPtr
      )

readBDBAtCursor :: Cursor -> IO (Maybe (BDBKey,ICStringLen))
readBDBAtCursor cursor =
   synchronize readLock (
      alloca (\ (cStringPtr :: Ptr (Ptr CChar)) ->
         alloca (\ (recNoPtr :: Ptr CSize) ->
            alloca (\ (lenPtr :: Ptr CSize) ->
               do
                   readAtCursor cursor recNoPtr cStringPtr lenPtr
                   (temporaryDataPtr :: Ptr CChar) <- peek cStringPtr
                   if temporaryDataPtr == nullPtr
                      then
                         return Nothing
                      else
                         do
                            (recNo :: CSize) <- peek recNoPtr
                            (len :: CSize) <- peek lenPtr
                            let
                               lenInt = fromIntegral len

                            cStringLen <- mkICStringLen lenInt
                              (\ permanentDataPtr ->
                                 copyArray permanentDataPtr temporaryDataPtr
                                    lenInt
                                 )
                            return (Just (BDBKey recNo,cStringLen))
               )
            )
         )
      )

-- --------------------------------------------------------------
-- The read lock
-- --------------------------------------------------------------

-- | We lock all reads.  This is because of the way BDB allocates
-- memory for the the results of get operations, namely to
-- provide a pointer, but only seems to guarantee that it points
-- to something useful until the next read operation.
readLock :: BSem
readLock = unsafePerformIO newBSem
{-# NOINLINE readLock #-}

-- --------------------------------------------------------------
-- The foreign function interface
-- --------------------------------------------------------------

-- | represents C type DB *.  The CChar is immaterial.
newtype DB = DB CChar
type BDB = Ptr DB

-- | Similar type for cursor
newtype Cursor0 = Cursor0 CChar
type Cursor = Ptr Cursor0

newtype BDBKey = BDBKey CSize deriving (Eq,Ord,Show,Integral,Real,Enum,Num)

instance Monad m => HasBinary BDBKey m where
   writeBin = mapWrite (\ (BDBKey w) -> w)
   readBin = mapRead BDBKey

foreign import ccall unsafe "bdbclient.h db_connect" dbConnect
   :: CString -> CString -> IO BDB
foreign import ccall unsafe "bdbclient.h db_store" dbStore
   :: BDB -> TXN -> CString -> CSize -> Ptr (CSize) -> IO ()
foreign import ccall unsafe "bdbclient.h db_store_here" dbStoreHere
   :: BDB -> TXN -> CString -> CSize -> CSize -> IO ()
foreign import ccall unsafe "bdbclient.h db_retrieve" dbRetrieve
   :: BDB -> CSize -> Ptr (CString) -> Ptr (CSize) -> IO ()
foreign import ccall unsafe "bdbclient.h db_flush" flushBDB
   :: BDB -> IO ()
foreign import ccall unsafe "bdbclient.h db_cursor" mkCursor0
   :: BDB -> Ptr Cursor -> IO ()
foreign import ccall unsafe "bdbclient.h db_read_cursor" readAtCursor
   :: Cursor -> Ptr CSize -> Ptr CString -> Ptr CSize -> IO ()
foreign import ccall unsafe "bdbclient.h db_close_cursor" closeCursor
   :: Cursor -> IO ()

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
