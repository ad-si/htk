-- | Calling into ActivMath from the Repository. 
module MMiSSActiveMath(
   mmiss2AM,
   ) where

import Directory
import Char

import System.IO.Unsafe
import Control.Concurrent.MVar

import WBFiles
import FileNames
import Computation
import CommandStringSub(bashEscape)
import ExtendedPrelude
import Messages

import SafeSystem
import CopyFile

import TempFile
import Computation

import View
import Link

import MMiSSFormat
import MMiSSObjectType
import MMiSSObjectExtract
import MMiSSRunCommand
import {-# SOURCE #-} MMiSSExportFiles



-- It is unsafe to call ActiveMath while it is still running,
-- so this MVar guards against that. If ActiveMath is running, it
-- is filled with True. If it is empty, somebody is currently changing it.
guard :: MVar Bool
guard = unsafePerformIO (newMVar False)
{-# NOINLINE guard #-}


-- returns location of script file
script :: String-> String -> String
script top str = trimDir top `combineNames`
                  "mmiss" `combineNames`
                  "scripts" `combineNames`
                  ("do"++str)

-- Guarded call into AM.
mmiss2AM :: View-> Link MMiSSObject-> IO ()
mmiss2AM view linke = 
  do active <- takeMVar guard
     if not active then do putMVar guard True
			   mmiss2AMu view linke
                           takeMVar guard -- empty MVar, then reset to False
			   putMVar guard False
       else do putMVar guard True -- we need to fill it again
  	       alertMess "ActiveMath already running."

-- Unguarded call into AM
mmiss2AMu :: View -> Link MMiSSObject-> IO ()
mmiss2AMu view link =
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
            exportFiles view fullXmlFile expFiles

            -- Write the String to a file.
            copyStringToFile contents fullXmlFile

            top <- getTOP 
	    let -- construct the command string
               commandArgs = script top "activemath" : [workingDir, xmlFile ]
	       commandArgsEscaped = map (\ arg -> '\"':(bashEscape arg ++ "\""))
				           commandArgs
	       cmdline = unwords commandArgsEscaped

            -- call ActiveMatch script
            runCommand "ActiveMath" cmdline

         )
      -- Delete the containing directory, using old-fashioned technology
      safeSystem ("rm -rf \""++ bashEscape workingDir++ "\"")
      done

