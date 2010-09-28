-- | This module contains the functions for printing objects
module MMiSS.Print(
   printMMiSSObject
   ) where

import Util.Computation
import Util.ExtendedPrelude
import Util.Messages

import Types.View
import Types.Link

import MMiSS.ObjectType
import MMiSS.ObjectExtract
import MMiSS.Format
import MMiSS.LaTeX

import {-# SOURCE #-} MMiSS.ExportFiles

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
         Left mess -> errorMess mess

