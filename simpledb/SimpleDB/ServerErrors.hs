{- This module handles server errors. -}
module SimpleDB.ServerErrors(
   ErrorType(..),
   throwError,
   catchError,
   isServerError,
   ) where

import System.IO.Unsafe
import Control.Exception
import Data.Dynamic

import Util.Object
import Util.ExtendedPrelude
import Util.BinaryAll

-- ------------------------------------------------------------------
-- Error types
-- ------------------------------------------------------------------

data ErrorType =
      AccessError
   |  NotFoundError
   |  InternalError
   |  MiscError
   |  ClientError -- ^ an error thrown on the client side.
   deriving (Enum)

-- ------------------------------------------------------------------
-- Show instance
-- ------------------------------------------------------------------

instance Show ErrorType where
   showsPrec n et acc =
      let
         st = case et of
            AccessError -> "Access"
            NotFoundError -> "Not Found"
            InternalError -> "Server Internal"
            MiscError -> "Server"
            ClientError -> "Client"
      in
         st ++ " Error" ++ acc

-- ------------------------------------------------------------------
-- HasBinary instance
-- ------------------------------------------------------------------

instance Monad m => HasBinary ErrorType m where
   writeBin = mapWrite ViaEnum
   readBin = mapRead enum

-- ------------------------------------------------------------------
-- User interface
-- ------------------------------------------------------------------

-- | Throw an error
throwError :: ErrorType -> String -> a
throwError errorType mess
   = mkBreakFn (fst fallOut) (encodeError errorType mess)

catchError :: IO b -> (ErrorType -> String -> b) -> IO b
catchError act wrapError =
   do
      errorOrB <- (snd fallOut) act
      return (case errorOrB of
         Right b -> b
         Left str ->
            let
               (errorType,mess) = decodeError str
            in
               wrapError errorType mess
         )

isServerError :: Dynamic -> Maybe (ErrorType,String)
isServerError exception =
   do
      str <- isOurFallOut (fst fallOut) exception
      return (decodeError str)

fallOut :: (ObjectID,IO a -> IO (Either String a))
fallOut = unsafePerformIO newFallOut
{-# NOINLINE fallOut #-}




-- ------------------------------------------------------------------
-- Encode and Decode access/misc errors as strings
-- ------------------------------------------------------------------

encodeError :: ErrorType -> String -> String
encodeError errorType mess =
   let
      c = case errorType of
         AccessError -> 'A'
         NotFoundError -> 'N'
         InternalError -> 'I'
         MiscError -> 'M'
         ClientError -> 'C'
   in
      c:mess

decodeError :: String -> (ErrorType,String)
decodeError "" = error "Missing error type"
decodeError (c:mess) =
   let
      errorType = case c of
         'A' -> AccessError
         'N' -> NotFoundError
         'I' -> InternalError
         'M' -> MiscError
         'C' -> ClientError
         _ -> error "Unknown error type"
   in
      (errorType,mess)
