{- This module contains the code for importing an MMiSSObject as LaTeX. -}
module MMiSSImportLaTeX(
   importMMiSSLaTeX,
   ) where

import Computation
import ExtendedPrelude
import WBFiles
import FileNames

import Events

import FileDialog
import DialogWin

import CopyFile

import View
import Link
import LinkManager

import LaTeXParser

import MMiSSObjectTypeType
import MMiSSObjectType
import MMiSSPreamble
import {-# SOURCE #-} MMiSSWriteObject

---
-- Import a new object from a LaTeX file and attach it to the subdirectory
-- of a given LinkedObject
importMMiSSLaTeX :: MMiSSObjectType -> View -> LinkedObject 
   -> IO (Maybe (Link MMiSSObject))
importMMiSSLaTeX objectType view linkedObject =
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
                     preamble <- case preambleOpt of
                        Just preamble -> return preamble
                        Nothing -> break "Object has no preamble!"

                     mmissPreamble <- createPreamble view preamble

                     linkWE <- writeToMMiSSObject mmissPreamble objectType view
                        linkedObject Nothing element True

                     link <- coerceWithErrorOrBreakIO break linkWE
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
