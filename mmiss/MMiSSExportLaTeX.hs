{- This module contains functions for exporting an object to a file as
   LaTeX -}
module MMiSSExportLaTeX(
   exportMMiSSObjectLaTeX,
   exportMMiSSObjectXML,
   ) where

import Computation
import ExtendedPrelude
import WBFiles
import FileNames
import Messages

import Events

import CopyFile

import FileDialog
import DialogWin

import View
import Link

import MMiSSObjectExtract
import MMiSSFormat
import MMiSSObjectType

import {-# SOURCE #-} MMiSSExportFiles

exportMMiSSObjectLaTeX :: View -> Link MMiSSObject -> IO ()
exportMMiSSObjectLaTeX = exportMMiSSObjectGeneral LaTeX

exportMMiSSObjectXML :: View -> Link MMiSSObject -> IO ()
exportMMiSSObjectXML = exportMMiSSObjectGeneral XML

exportMMiSSObjectGeneral :: Format -> View -> Link MMiSSObject -> IO ()
exportMMiSSObjectGeneral format view link =
   do
      result <- addFallOut (\ break ->
         do
            object <- readLink view link

            top <- getTOP
            let
               fullName = unbreakName [top,"mmiss","test","files"]

	    dialogEvent <- newFileDialog 
               ("Export "++show format++" sources") fullName

            filePathOpt <- sync dialogEvent
	    
            case filePathOpt of
               Just filePath ->
                  do
                     (result1WE :: WithError (String,ExportFiles))
                        <- extractMMiSSObject view link format
                     let
                        (string,exportFiles0) 
                           = coerceWithErrorOrBreak break result1WE


                     -- Write to the file
                     resultWE <- copyStringToFileCheck string filePath
                     coerceWithErrorOrBreakIO break resultWE

                     -- Write the attached files.
                     let
                        writeDir = fst (splitName filePath)
                     exportFiles view writeDir exportFiles0 
               Nothing -> messageMess "Export cancelled"
         )

      case result of
         Right () -> done
         Left mess -> errorMess mess