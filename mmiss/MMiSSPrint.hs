{- This module contains the functions for printing objects -}
module MMiSSPrint(
   printMMiSSObject
   ) where

import Computation
import ExtendedPrelude
import Sources

import DialogWin

import View
import Link

import MMiSSObjectType
import MMiSSObjectExtract
import MMiSSFormat
import MMiSSLaTeX

import {-# SOURCE #-} MMiSSExportFiles

printMMiSSObject :: View -> Link MMiSSObject -> IO ()
printMMiSSObject view link =
   do
      result <- addFallOut (\ break ->
         do
            (result1WE :: WithError (String,ExportFiles))
               <- extractMMiSSObject view link LaTeX
            (string,exportFiles0) <- coerceWithErrorOrBreakIO break result1WE
 
            object <- readLink view link
            objectTitle <- objectName object

            mmissLaTeX view objectTitle string exportFiles0
         )
      case result of
         Right () -> done
         Left mess -> createErrorWin mess []

