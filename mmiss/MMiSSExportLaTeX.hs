{- This module contains functions for exporting an object to a file as
   LaTeX -}
module MMiSSExportLaTeX(
   exportMMiSSObjectLaTeX
   ) where

import Computation
import ExtendedPrelude
import WBFiles
import FileNames

import Events

import CopyFile

import FileDialog
import DialogWin

import View
import Link

import MMiSSObjectExtract
import MMiSSFormat
import MMiSSObjectType

exportMMiSSObjectLaTeX :: View -> Link MMiSSObject -> IO ()
exportMMiSSObjectLaTeX view link =
   do
      result <- addFallOut (\ break ->
         do
            object <- readLink view link

            top <- getTOP
            let
               fullName = unbreakName [top,"mmiss","test","files"]

	    dialogEvent <- fileDialog "Export LaTeX sources" fullName

            filePathOpt <- sync dialogEvent
	    
            case filePathOpt of
               Just filePath ->
                  do
                     stringWE <- extractMMiSSObject view link LaTeX
                     let
                        string = coerceWithErrorOrBreak break stringWE


                     -- Write to the file
                     resultWE <- copyStringToFileCheck string filePath
                     coerceWithErrorOrBreakIO break resultWE
               Nothing -> createMessageWin "Export cancelled" []
         )

      case result of
         Right () -> done
         Left mess -> createErrorWin mess []