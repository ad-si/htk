{- #########################################################################

MODULE        : WBFiles
AUTHOR        : Einar Karlsen, George Russell  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1999
VERSION       : 0.2
DESCRIPTION   : 


   ######################################################################### -}


module WBFiles (
   getWBFilePath,
   getWBToolFilePath,
   getWBImageFilePath,
   getWBExportFilePath
   ) where

import IO
import qualified System
import qualified IOExts(unsafePerformIO)
import Debug(debug)

#include "top/mk/settings.h"

prefix :: String
-- use unsafePerformIO to prevent the IO action being done more than once.
prefix =
   IOExts.unsafePerformIO(
      do
         rootVar <- try (System.getEnv "WB_ROOT")
         case rootVar of
            Left error -> return (UNIDIR ++ "/database")
            Right value -> return value
      )
-- --------------------------------------------------------------------------
-- WorkBench File
-- --------------------------------------------------------------------------

getWBFilePath :: FilePath -> IO FilePath
getWBFilePath fnm = 
   do
      return (prefix ++ "/" ++ fnm)


getWBToolFilePath :: FilePath -> IO FilePath
getWBToolFilePath fnm = 
   do
      return (prefix ++ "/bin/" ++ fnm)

getWBImageFilePath :: FilePath -> IO FilePath
getWBImageFilePath fnm = 
   do 
      return (prefix ++ "/images/" ++ fnm)


getWBExportFilePath :: FilePath -> IO FilePath
getWBExportFilePath fnm = 
   do
      return (prefix ++ "/exports/" ++ fnm)


