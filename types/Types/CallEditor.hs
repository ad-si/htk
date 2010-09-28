-- | Parses the editor string and calls the editor, returning when it's
-- finished.
module Types.CallEditor(
   callEditor,
   editObject,
   ) where

import System

import System.IO.Unsafe

import Util.Computation
import Util.CommandStringSub
import Util.WBFiles
import Util.Sources

import Posixutil.SafeSystem

import Types.Link
import Types.ObjectTypes
import Types.View
import Types.BasicObjects

editorCommand :: CompiledFormatString
editorCommand = unsafePerformIO (
   do
      editorStringOpt <- getEditorString
      case editorStringOpt of
         Nothing -> error "No editor is defined"
         Just editorString ->
            return (coerceWithError (compileFormatString editorString))

   )
{-# NOINLINE editorCommand #-}

-- | Parses the editor string and calls the editor, returning when it\'s
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

-- | Edits an object which instances HasFilePath and edits it.
editObject :: (ObjectType objectType object,HasFilePath object)
   => View -> Link object -> IO ()
editObject view link =
   do
      versioned <- fetchLink view link
      object <- readObject view versioned
      title <- readContents (nodeTitleSourcePrim object)
      callEditor (toFilePath object) title
      dirtyObject view versioned
