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
        getWBExportFilePath,
        

        ) where


import qualified System
import Debug(debug)


-- --------------------------------------------------------------------------
-- WorkBench File
-- --------------------------------------------------------------------------

getWBFilePath :: FilePath -> IO FilePath
getWBFilePath fnm = do {
        prefix <- System.getEnv "WB_ROOT";
        return (prefix ++ "/" ++ fnm);
}

getWBToolFilePath :: FilePath -> IO FilePath
getWBToolFilePath fnm = do {
        prefix <- System.getEnv "WB_ROOT";
        return (prefix ++ "/bin/" ++ fnm);
}

getWBImageFilePath :: FilePath -> IO FilePath
getWBImageFilePath fnm = do {
        prefix <- System.getEnv "WB_ROOT";
        return (prefix ++ "/images/" ++ fnm);
}

getWBExportFilePath :: FilePath -> IO FilePath
getWBExportFilePath fnm = do {
        prefix <- System.getEnv "WB_ROOT";
        return (prefix ++ "/exports/" ++ fnm);
}


