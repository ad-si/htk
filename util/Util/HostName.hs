-- | This module contains code which (supposedly) extracts the full qualified
-- name of the machine on which it is running.  (At least it does on the
-- Linux and Solaris implementations I tested.)
module Util.HostName(
   getFullHostName,
   ) where

import Network.BSD

getFullHostName :: IO String
getFullHostName =
   do
      partialName <- getHostName
      hostEntry <- getHostByName partialName
      return (hostName hostEntry)
