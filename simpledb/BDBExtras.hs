-- | The functions in this module provide some extra utilities for
-- manipulating the BDB database.
module BDBExtras(
   getObject,
   getObjectOpt,
   getObjectAtCursor,
   setObject,
   setObjectHere,
   setObjectHere1,
   ) where

import BinaryAll
import ICStringLen

import ServerErrors
import BDBOps

-- -------------------------------------------------------------------
-- Retrieving an object from a data base by key.
-- -------------------------------------------------------------------

getObject :: HasBinary a StateBinArea => BDB -> BDBKey -> IO a
getObject bdb bdbKey =
   do
      aOpt <- getObjectOpt bdb bdbKey
      case aOpt of
         Nothing -> throwError InternalError
            ("Unexpected missing key " ++ show bdbKey)
         Just a -> return a
getObjectOpt :: HasBinary a StateBinArea => BDB -> BDBKey -> IO (Maybe a)
getObjectOpt bdb bdbKey =
   do
      icslOpt <- readBDB bdb bdbKey
      case icslOpt of
         Nothing -> return Nothing
         Just icsl ->
            do
               a <- readICStringLen icsl
               return (Just a)

getObjectAtCursor :: HasBinary a StateBinArea
   => Cursor -> IO (Maybe (BDBKey,a))
getObjectAtCursor cursor =
   do
      dataOpt <- readBDBAtCursor cursor
      case dataOpt of
         Nothing -> return Nothing
         Just (key,icsl) ->
            do
               a <- readICStringLen icsl
               return (Just (key,a))

setObject :: HasBinary a StateBinArea => BDB -> a -> IO BDBKey
setObject bdb a =
   do
      icsl <- writeToICStringLen a
      txn <- beginTransaction
      key <- writeBDB bdb txn icsl
      endTransaction txn
      return key

setObjectHere :: HasBinary a StateBinArea => BDB -> BDBKey -> a -> IO ()
setObjectHere bdb key a =
   do
      txn <- beginTransaction
      setObjectHere1 bdb key txn a
      endTransaction txn

setObjectHere1 :: HasBinary a StateBinArea => BDB -> BDBKey -> TXN -> a
   -> IO ()
setObjectHere1 bdb key txn a =
   do
      icsl <- writeToICStringLen a
      writeBDBHere bdb txn key icsl
