{- Parses the editor string and calls the editor, returning when it's
   finished. 
   -}
module CallEditor(
   callEditor,
   ) where

import System

import Computation
import WBFiles

import SafeSystem

---
-- Parses the editor string and calls the editor, returning when it's
-- finished.  For the moment, various error cases are handled by throwing 
-- error.
callEditor :: FilePath -> IO ()
callEditor filePath =
   do
      editorStringOpt <- getEditorString
      let
         editorString = case editorStringOpt of
            Nothing -> error "No editor is defined"
            Just editorString -> editorString
      
         -- Now find the editor command.
         mapEditorString "" = ""
         mapEditorString ('%':'s':rest) = filePath++mapEditorString rest
         mapEditorString ('%':c:rest) = c:mapEditorString rest
         mapEditorString (c:rest) = c:mapEditorString rest

         command = mapEditorString editorString

      exitCode <- safeSystem command

      case exitCode of
         ExitFailure c ->
            error ("CallEditor: "++command++" returned error "++show c)
         ExitSuccess -> done

