-- | This module contains functions for exporting an object to a file as
-- LaTeX
module MMiSS.ExportLaTeX(
   exportMMiSSObjectLaTeX,
   exportMMiSSObjectXML,
   pathRef
   ) where

import System.IO.Unsafe(unsafePerformIO)
import Reactor.ReferenceVariables
import Util.Computation
import Util.ExtendedPrelude
import Util.WBFiles
import Util.FileNames
import Util.Messages

import Events.Events

import Posixutil.CopyFile

import HTk.Toolkit.FileDialog

import Types.View
import Types.Link

import MMiSS.ObjectExtract
import MMiSS.Format
import MMiSS.ObjectType

import {-# SOURCE #-} MMiSS.ExportFiles

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

            dialogEvent <- newFileDialog
               ("Export "++show format++" sources") pathRef

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

-- The reference in which to keep the path name in between calls of the file
-- dialog. Shared with MMiSSPackageFolder.
pathRef :: Ref String
pathRef =
  unsafePerformIO $ do top <- getTOP
                       let fullName = unbreakName [top,"mmiss","test","files"]
                       newRef fullName
{-# NOINLINE pathRef #-}
