{- This module contains the code for importing an MMiSSObject as LaTeX. -}
module MMiSSImportLaTeX(
   importMMiSSLaTeX,
   ) where

import Maybe

import Computation
import ExtendedPrelude
import WBFiles
import FileNames
import Delayer

import Events

import FileDialog
import DialogWin

import CopyFile

import ViewType
import View
import Link
import LinkManager
import EntityNames

import LaTeXParser

import MMiSSDTDAssumptions
import MMiSSObjectTypeType
import MMiSSObjectType
import MMiSSPreamble
import {-# SOURCE #-} MMiSSWriteObject

---
-- Import a new object from a LaTeX file and attach it to the subdirectory
-- of a given LinkedObject.
--
-- The preamble should be written to the link given by the first
-- argument.
--
-- The complicated last argument constructs or retrieves the parent linked 
-- object, given the name of the package.
importMMiSSLaTeX :: Link MMiSSPreamble -> MMiSSObjectType -> View
   -> (EntityName -> IO (WithError LinkedObject)) 
   -> IO (Maybe (Link MMiSSObject))
importMMiSSLaTeX preambleLink objectType view getLinkedObject =
   do
      result <- addFallOut (\ break ->
         do
	    top <- getTOP 
            let
               fullName = unbreakName [top,"mmiss","test","files"]
            dialogEvent <- fileDialog "Import LaTeX sources" fullName
            filePathOpt <- sync dialogEvent
            case filePathOpt of
               Nothing -> return Nothing
               Just filePath ->
                  do
	             inputStringWE <- copyFileToStringCheck filePath
                     inputString 
                        <- coerceWithErrorOrBreakIO break inputStringWE
                     let
                        parseResultWE = parseMMiSSLatex inputString

                     (element,preambleOpt) 
                        <- coerceWithErrorOrBreakIO break parseResultWE

                     fullLabel 
                        <- coerceWithErrorOrBreakIO break (getLabel element)

                     let
                        baseLabel = fromMaybe
                           (break "Element label has no name!")
                           (entityBase fullLabel)

                     seq baseLabel done

                     linkedObjectWE <- getLinkedObject baseLabel
                     linkedObject 
                        <- coerceWithErrorOrBreakIO break linkedObjectWE

                     preamble <- case preambleOpt of
                        Just preamble -> return preamble
                        Nothing -> break "Object has no preamble!"

                     writePreamble preambleLink view preamble

                     linkWE <- writeToMMiSSObject objectType view
                        linkedObject Nothing element True

                     (link,_) <- coerceWithErrorOrBreakIO break linkWE
                     return (Just link)
         )
      case result of
         Left str ->
            do
               createMessageWin str []
               return Nothing
         Right (linkOpt@(Just link)) -> 
            do
               createMessageWin "Import successful!" [] 
               return linkOpt
         Right Nothing -> return Nothing 
            -- message has already been displayed by fileDialog.
