-- | Call the MMiSS checker. 
module MMiSSCheck(
   mmissCheck,
   ) where

import Maybe
import Directory
import Char
import System
import Control.Exception -- catches more.
import Prelude hiding (catch)

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

import View
import Link

import qualified MarkupText as M
import HTk
import TextDisplay

import MMiSSFormat
import MMiSSObjectType
import MMiSSObjectExtract
import MMiSSRunCommand
import MMiSSVariant
import {-# SOURCE #-} MMiSSPackageFolder
import {-# SOURCE #-} MMiSSExportFiles

import qualified MMiSSCheck_DTD as DTD
import Text.XML.HaXml.Xml2Haskell


-- returns location of script file
script :: String-> String -> String
script top str = trimDir top `combineNames`
                  "mmiss" `combineNames`
                  "scripts" `combineNames`
                  ("do"++str)
--
-- Runs the checker
--
runChecker :: String -> String -> String-> [String] -> IO ()
runChecker title scriptName dir args =
  do top <- getTOP 
     let -- construct the command string
         commandArgs = script top scriptName : dir : args
         commandArgsEscaped = map (\ arg -> '\"':(bashEscape arg ++ "\""))
                                  commandArgs
         command = unwords commandArgsEscaped
     (exitCode, output) <- runTool title command
     case exitCode of
       ExitFailure code -> do errorWin title code output
       ExitSuccess -> do r <- catch (evaluate (readXml output))
			            (\_ -> return Nothing)
                         case r of 
  			   Just chckout -> procChecks chckout
                           Nothing -> errorWin title (-1) 
				       ("Can't parse this output:\n"++ output)


-- process the output from the checker, and display it.
procChecks :: DTD.Checklist -> IO ()
procChecks (DTD.Checklist (NonEmpty chcks)) = 
  do let headln  = [M.centered 
		    [M.bold 
		     [M.prose "Results of Checking on your document:"],
		     M.newline, M.newline]]
         txt     = concatMap renderOneCheck chcks
         numerrs = length (filter (\ (DTD.Check a _ _) -> 
                                      DTD.checkSuccess a == DTD.Check_success_No )
			          chcks)
         summary = if numerrs == 0 then
                     [M.colour "green" [M.prose "All checks have been succesfull."]]
                   else [M.prose $ show numerrs ++ " check(s) failed."]

         restxt  = headln ++ txt ++ summary
     (win, ed) <- createTextDisplayExt "MMiSS Consistency Checker" "" [] done
     ed # M.new restxt
     done                     

-- render result of one check
renderOneCheck :: DTD.Check -> [M.MarkupText]
renderOneCheck (DTD.Check attrs msg objs) = 
  let msgtxt= case msg of Just (DTD.Message t) -> t
                          _ -> "No further information available."
  in M.prose (DTD.checkName attrs) :
     (case DTD.checkSuccess attrs of 
        DTD.Check_success_Yes -> [M.prose " was ",
				  M.colour "green" [M.prose "successful."]]
        DTD.Check_success_No ->  [M.prose " found ",
			          M.colour "red" [M.prose "errors: ",
					          M.newline, M.prose msgtxt]])
      ++ [M.newline, M.newline]
     
-- Run the MMiSS Checker -- main function.
mmissCheck :: View -> Link MMiSSObject-> IO ()
mmissCheck view link =
   do -- Create a directory to work in.
      workingDir <- newTempFile
      createDirectory workingDir
      addFallOut (\ break -> 
         do 
            -- Get the XML representation of the object.
            (res :: WithError (String,ExportFiles)) 
                 <- extractMMiSSObject view link XML
            (contents, expFiles) <- coerceWithErrorOrBreakIO break res
 
            -- Get the name, and from that the filename
            object <- readLink view link
            fileName <- objectName object
            let xmlFile = fileName ++ ".xml" -- "missxml" ??
                fullXmlFile = (trimDir workingDir) `combineNames` xmlFile

            -- Export needed attached graphics files.
            exportFiles view workingDir expFiles

            -- Write the String to a file.
            copyStringToFile contents fullXmlFile

            -- run the checker
            runChecker "MMiSS Consistency Checker" "mmisschecker" 
		       workingDir [xmlFile]
         )
      -- Delete the containing directory, using old-fashioned technology
      safeSystem ("rm -rf \""++ bashEscape workingDir++ "\"")
      done

