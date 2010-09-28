-- | The functions in this module run a file (given as a String) through
-- LaTeX, and does various things with it.
module MMiSS.LaTeX(
   mmissLaTeX,
   ) where

import Directory

import Maybe
import Char

import Util.WBFiles
import Util.FileNames
import Util.Computation
import Util.CommandStringSub(bashEscape)
import Util.ExtendedPrelude

import Reactor.WithDir

import Posixutil.SafeSystem
import Posixutil.CopyFile

import Util.TempFile
import Util.Computation

import HTk.Toolkit.SimpleForm

import Types.View(View)

import MMiSS.RunCommand
import MMiSS.Variant
import {-# SOURCE #-} MMiSS.ExportFiles
import {-# SOURCE #-} MMiSS.PackageFolder

-- | The first String is the name of the file; the second its contents.
mmissLaTeX :: View -> String -> String -> ExportFiles -> IO ()
mmissLaTeX view fileName contents exportFiles0 =
   do
      -- Create a directory to work in.
      laTeXDir <- newTempFile
      createDirectory laTeXDir
      addFallOut (\ break -> -- break does not necessarily mean an error here.
         do
            -- Initial definitions
            top <- getTOP

            let
               cancel = break "MMiSSLaTeX cancelled"

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
                  withDir laTeXDir (copyFileBool source destination)

               laTeXFile = fileName ++ ".tex"

               fullLaTeXFile = (trimDir laTeXDir) `combineNames` laTeXFile

            -- Export needed attached graphics files.
            exportFiles view laTeXDir exportFiles0

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

