{- This module handles server errors. -}
module ServerErrors(
   ErrorType(..),
   throwError,
   catchError,
   ) where

import System.IO.Unsafe

import Object
import ExtendedPrelude

-- ------------------------------------------------------------------
-- Error types
-- ------------------------------------------------------------------

data ErrorType =
      AccessError
   |  NotFoundError
   |  InternalError
   |  MiscError

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
         _ -> error "Unknown error type"
   in
      (errorType,mess)         