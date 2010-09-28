-- | This module contains try-catch functions for error conditions that are
-- likely to arise during import and export.
module MMiSS.ImportExportErrors(
   importExportError, -- :: BreakFn
   catchImportExportErrors, -- :: IO a -> IO (Either String a)
   coerceImportExportIO, -- :: WithError a -> IO a
   coerceImportExportIOPrefix, -- :: String -> WithError a -> IO a
   coerceImportExport, -- :: WithError a -> a
   catchAllErrorsWE, -- :: IO a -> IO (WithError a)
   displayImportExportErrors, -- :: a -> IO a -> IO a
   makeOtherExcepsToOurs, -- :: IO a -> IO a

   ) where

import Control.Exception(catchJust)
import System.IO.Unsafe

import Util.Messages

import Util.Object
import Util.ExtendedPrelude
import Util.Computation

import SimpleDB.ServerErrors

importExportErrorFallOut :: (ObjectID,IO a -> IO (Either String a))
importExportErrorFallOut = unsafePerformIO newFallOut
{-# NOINLINE importExportErrorFallOut #-}

importExportError :: BreakFn
importExportError = mkBreakFn (fst importExportErrorFallOut)

makeOtherExcepsToOurs :: IO a -> IO a
makeOtherExcepsToOurs act =
   do
      let
         tryGeneral excep = case isServerError excep of
            Just (errorType,mess) -> importExportError (
               show errorType ++ ": " ++ mess)
            Nothing ->
               case isOurFallOut (fst importExportErrorFallOut) excep of
                  Just _ -> Nothing
                  Nothing -> importExportError (showException2 excep)

      catchJust tryGeneral act (return . id)

catchImportExportErrors :: IO a -> IO (Either String a)
catchImportExportErrors = snd importExportErrorFallOut

coerceImportExportIOPrefix :: String -> WithError a -> IO a
coerceImportExportIOPrefix errorPrefix = coerceWithErrorOrBreakIOPrefix
   errorPrefix importExportError

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
