{- Parses the editor string and calls the editor, returning when it's
   finished. 
   -}
module CallEditor(
   callEditor,
   editObject,
   ) where

import System

import qualified IOExts(unsafePerformIO)

import Computation
import CommandStringSub
import WBFiles

import SafeSystem

import Link
import ObjectTypes
import View
import BasicObjects

editorCommand :: CompiledFormatString
editorCommand = IOExts.unsafePerformIO (
   do
      editorStringOpt <- getEditorString
      case editorStringOpt of
         Nothing -> error "No editor is defined"
         Just editorString -> 
            return (coerceWithError (compileFormatString editorString))

   )
{-# NOINLINE editorCommand #-}

---
-- Parses the editor string and calls the editor, returning when it's
-- finished.  The first argument gives the location of the file;
-- the second the name it should be called.
callEditor :: FilePath -> String -> IO ()
callEditor filePath name =
   do
      let
         dict ch = case ch of
            'F' -> Just filePath
            'N' -> Just name
            _ -> Nothing

         command = coerceWithError (runFormatString editorCommand dict)

      exitCode <- safeSystem command

      case exitCode of
         ExitFailure c ->
            error ("CallEditor: "++command++" returned error "++show c)
         ExitSuccess -> done

---
-- Edits an object which instances HasFilePath and edits it.
editObject :: (ObjectType objectType object,HasFilePath object)
   => View -> Link object -> IO ()
editObject view link =
   do
      versioned <- fetchLink view link
      object <- readObject view versioned
      callEditor (toFilePath object) (nodeTitlePrim object)
      dirtyObject view versioned
