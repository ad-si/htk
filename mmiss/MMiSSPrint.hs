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


printMMiSSObject :: View -> Link MMiSSObject -> IO ()
printMMiSSObject view link =
   do
      result <- addFallOut (\ break ->
         do
            stringWE <- extractMMiSSObject view link LaTeX
            string <- coerceWithErrorOrBreakIO break stringWE
 
            object <- readLink view link
            objectTitle <- objectName object

            mmissLaTeX objectTitle string
         )
      case result of
         Right () -> done
         Left mess -> createErrorWin mess []
