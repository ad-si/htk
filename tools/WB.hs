{- This module is supposed to provide simple implementions of
   the functionality used by the tools in the original WB in UniForM,
   without needing to use the OMS.  If possible.
   -}

module WB(
   getWBToolFilePath
   ) where

getWBToolFilePath :: String -> String
getWBToolFilePath s = s