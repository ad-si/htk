-- | This module contains try-catch functions for error conditions that are
-- likely to arise during import and export. 
module MMiSSImportExportErrors(
   importExportError, -- :: BreakFn
   catchImportExportErrors, -- :: IO a -> IO (Either String a)
   coerceImportExportIO, -- :: WithError a -> IO a
   coerceImportExport, -- :: WithError a -> a
   catchAllErrorsWE, -- :: IO a -> IO (WithError a)
   displayImportExportErrors, -- :: a -> IO a -> IO a
   ) where

import System.IO.Unsafe

import Computation(done)
import Messages

import Object
import ExtendedPrelude
import Computation

importExportErrorFallOut :: (ObjectID,IO a -> IO (Either String a))
importExportErrorFallOut = unsafePerformIO newFallOut
{-# NOINLINE importExportErrorFallOut #-}

importExportError :: BreakFn
importExportError = mkBreakFn (fst importExportErrorFallOut)

catchImportExportErrors :: IO a -> IO (Either String a)
catchImportExportErrors = snd importExportErrorFallOut

coerceImportExportIO :: WithError a -> IO a
coerceImportExportIO = coerceWithErrorOrBreakIO importExportError

coerceImportExport :: WithError a -> a
coerceImportExport = coerceWithErrorOrBreak importExportError

catchAllErrorsWE :: IO a -> IO (WithError a)
catchAllErrorsWE act =
   do
      exceptionOrerrorOrUnit <- try (catchImportExportErrors act)
      case exceptionOrerrorOrUnit of
         Left excep -> 
            do
               return (hasError ("System error: " ++ show excep))
         Right (Left mess) ->
            return (hasError mess)
         Right (Right a) -> return (hasValue a)

displayImportExportErrors :: a -> IO a -> IO a
displayImportExportErrors defaultA act =
   do
      aWE <- catchAllErrorsWE act
      case fromWithError aWE of
         Left mess ->
            do
               errorMess mess
               return defaultA
         Right a -> return a
