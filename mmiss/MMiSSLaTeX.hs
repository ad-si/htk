{- The functions in this module run a file (given as a String) through
   LaTeX, and does various things with it. -}
module MMiSSLaTeX(
   mmissLaTeX,
   ) where

import Maybe
import Directory
import Char

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

               -- Copies one file to another
               copy :: String -> String -> IO Bool
               copy source destination =
                  do
                     currentDir <- getCurrentDirectory
                     setCurrentDirectory laTeXDir
                     success <- copyFileBool source destination
                     setCurrentDirectory currentDir
                     return success

               laTeXFile = fileName ++ ".tex"

               fullLaTeXFile = (trimDir laTeXDir) `combineNames` laTeXFile

            -- Run file through misstex.
            formatOpt <- doForm ("Format for "++fileName) outputTypeForm

            let
               format = fromMaybe cancel formatOpt
            seq format done

            -- Write the String to a file.
            copyStringToFile contents fullLaTeXFile
            let
               misslatexArg = case format of
                  DVI -> "-d"
                  PS -> "-p"
                  PDF -> "-f"
            success <- run "MMiSS-LaTeX" "misslatex" [misslatexArg,laTeXFile]
            if success 
               then
                  done
               else
                  cancel

            -- Depending on the target format chosen, we now enter a loop
            -- allowing the user to do various things with the resulting
            -- output, until he/she clicks Cancel.
            let
               emptyName str = all Char.isSpace str

               dviLoop :: IO ()
               -- Choice is xdvi or send to file
               dviLoop =
                  do
                     let
                        xdvi = newFormEntry "Preview" False
                        file = newFormEntry "Or send to file: " ""

                        form1 = xdvi // file
                        form2 =
                           guardForm (\ (xdvi,fPath) ->
                              not xdvi || emptyName fPath
                              )
                              "preview and a file cannot both be specified"
                              form1
                        form3 =
                           guardForm (\ (xdvi,fPath) ->
                              xdvi || not (emptyName fPath)
                              )
                              "neither preview nor a file are specified"
                              form2
                 
                        dviFile = fileName ++ ".dvi"


                     dviOpt <- doForm "DVI destination" form3
                     case dviOpt of
                        Nothing -> cancel
                        Just (True,_) ->
                           run "xdvi" "xdvi" [dviFile]
                        Just (False,fPath) ->
                           copy dviFile fPath
                     done

               pdfLoop :: IO ()
               -- Choice is acroread or send to file
               pdfLoop =
                  do
                     let
                        acroread = newFormEntry "Preview" False
                        file = newFormEntry "Or send to file: " ""
                        pdfFile = fileName ++ ".pdf"

                        form1 = acroread // file
                        form2 =
                           guardForm (\ (acro,fPath) ->
                              not acro || emptyName fPath
                              )
                              "preview and a file cannot both be specified"
                              form1
                        form3 =
                           guardForm (\ (acro,fPath) ->
                              acro || not (emptyName fPath)
                              )
                              "neither preview nor a file are specified"
                              form2
                 
                     pdfOpt <- doForm "PDF destination" form3
                     case pdfOpt of
                        Nothing -> cancel
                        Just (True,_) ->
                           run "Acroread" "acroread" [pdfFile]
                        Just (False,fPath) ->
                           copy pdfFile fPath
                     done

               psLoop :: IO ()
               -- Choice is acroread or send to file
               psLoop =
                  do
                     let
                        print = newFormEntry 
                           "Send to printer (empty for default): " ""
                        file = newFormEntry "Or to file: " ""
                        psFile = fileName ++ ".ps"

                        form1 = print // file
                        form2 =
                           guardForm (\ (printer,fPath) ->
                              emptyName printer || emptyName fPath
                              )
                              "printer and a file cannot both be specified"
                              form1
                 
                     psOpt <- doForm "PS destination" form2
                     case psOpt of
                        Nothing -> cancel
                        Just (printer,fPath) 
                              | emptyName fPath ->
                           run "Printing" "lp" [psFile,printer]
                        Just (_,fPath) ->
                           copy psFile fPath
                     done

            forever (case format of
               DVI -> dviLoop
               PDF -> pdfLoop
               PS -> psLoop
               )
         )
      -- Delete the containing directory, using old-fashioned technology
      safeSystem ("rm -rf \""++bashEscape laTeXDir++"\"")
      done

-- --------------------------------------------------------------------
-- OutputType and form that reads it
-- --------------------------------------------------------------------

data OutputType = DVI | PS | PDF

outputTypeForm :: Form OutputType
outputTypeForm = newFormOptionMenu2 [
   ("DVI",DVI),
   ("PostScript",PS),
   ("PDF",PDF)
   ]

