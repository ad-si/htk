{- The functions in this module run a file (given as a String) through
   LaTeX, and does various things with it. -}
module MMiSSLaTeX(
   mmissLaTeX,
   ) where

import Maybe
import Directory

import WBFiles
import FileNames
import Computation
import CommandStringSub(bashEscape)
import ExtendedPrelude

import SafeSystem
import CopyFile

import TempFile
import Computation

import SimpleForm

import MMiSSRunCommand

---
-- The first String is the name of the file; the second its contents.
mmissLaTeX :: String -> String -> IO ()
mmissLaTeX fileName contents =
   do
      -- Create a directory to work in.
      laTeXDir <- newTempFile
      createDirectory laTeXDir

      addFallOut (\ break -> -- break does not necessarily mean an error here.
         do
            -- Initial definitions
            top <- getTOP

            let
               cancel = break ""

               -- returns location of script file
               script :: String -> String
               script str =
                  (trimDir top) `combineNames`
                  ("mmiss") `combineNames`
                  ("scripts") `combineNames`
                  ("do"++str) 

               -- runs a script, with the given arguments.
               -- the first argument is always provided and is
               -- laTeXDir.
               --
               -- returns True for Success.  (In case of errors, a message
               -- will already have been displayed.)
               run :: String -> String -> [String] -> IO Bool
               run title scriptName args =
                  do
                     let
                        -- construct the command string
                        commandArgs = script scriptName : laTeXDir : args
                        commandArgsEscaped =
                           map
                              (\ arg -> '\"':(bashEscape arg ++ "\""))
                              commandArgs
                        command = unwords commandArgsEscaped
                     runCommand title command

               laTeXFile = fileName ++ ".tex"
               dviFile = fileName ++ ".dvi"

               fullLaTeXFile = (trimDir laTeXDir) `combineNames` laTeXFile

            -- Write the String to a file.
            copyStringToFile contents fullLaTeXFile

            -- Run it through LaTeX
            success <- run ("LaTeX "++fileName) "latex" [laTeXFile]
            if success 
               then
                  done
               else
                  cancel

            let
               -- The following loop is run until the user clicks cancel at the
               -- start.
               doOutput :: IO ()
               doOutput =               
                  do
                     -- Find out what the user wants to do with the output
                     outputTypeOpt 
                        <- doForm ("How to display "++fileName) outputTypeForm
                     let
                        outputType = fromMaybe cancel outputTypeOpt
                     case outputType of
                        Preview -> 
                           do 
                              run ("Preview "++fileName) "xdvi" [dviFile]
                              done
                        File ->
                           do
                              let
                                 filePathForm = newFormEntry
                                    "Write PostScript to" ""
                              filePathOpt 
                                 <- doForm "Print to File" filePathForm
                              case filePathOpt of
                                 Nothing -> done
                                 Just filePath -> 
                                    do
                                       run ("Print "++fileName++" to file")
                                          "dvips" [dviFile,filePath]
                                       done
                        Printer ->
                           do
                              let
                                 printerForm = newFormEntry
                                    "Printer" ""
                              printerOpt <- doForm "Print to File" printerForm
                              case printerOpt of
                                 Nothing -> 
                                    run ("Printing "++fileName) "lp"
                                       [dviFile]
                                 Just printer -> 
                                    run ("Printing "++fileName) "lp"
                                       [dviFile,printer]
                              done
            forever doOutput       
         )
      -- Delete the containing directory, using old-fashioned technology
      safeSystem ("rm -rf \""++bashEscape laTeXDir++"\"")
      done

-- --------------------------------------------------------------------
-- OutputType and form that reads it
-- --------------------------------------------------------------------

data OutputType = Preview | File | Printer

outputTypeForm :: Form OutputType
outputTypeForm = newFormOptionMenu2 [
   ("Preview",Preview),
   ("Print to File",File),
   ("Send to Printer",Printer)
   ]

