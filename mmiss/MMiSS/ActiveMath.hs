-- | Calling into ActivMath from the Repository.
module MMiSS.ActiveMath(
   mmiss2AM,
   ) where

import Directory
import Data.Char

import System.IO.Unsafe
import Control.Concurrent.MVar

import Util.WBFiles
import Util.FileNames
import Util.Computation
import Util.CommandStringSub(bashEscape)
import Util.ExtendedPrelude
import Util.Messages

import Posixutil.SafeSystem
import Posixutil.CopyFile

import Util.TempFile
import Util.Computation

import Types.View
import Types.Link

import MMiSS.Format
import MMiSS.ObjectType
import MMiSS.ObjectExtract
import MMiSS.RunCommand
import {-# SOURCE #-} MMiSS.ExportFiles



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

