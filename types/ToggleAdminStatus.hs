-- | This module implements a dialog window for toggling the admin status.
module ToggleAdminStatus(
   AdminStatus,
   newAdminStatus, -- :: Repository -> IO AdminStatus
   toggleAdminStatus, -- :: AdminStatus -> IO ()
   ) where

import Control.Concurrent.MVar

import Messages

import VersionDB

-- ------------------------------------------------------------------------
-- Datatypes
-- ------------------------------------------------------------------------

-- | There should be just one of these for each repository.
data AdminStatus = AdminStatus {
   repository :: Repository,
   statusMVar :: MVar Bool
   }

-- ------------------------------------------------------------------------
-- Functions
-- ------------------------------------------------------------------------

-- | Create a new 'AdminStatus' value. 
-- NB.  This function assumes that
-- 
--     *  the user does not have admin status to the repository.
--  
--     *  there is no other AdminStatus value for the repository,
--        and after newAdminStatus is called, no other accesss to
--        toggleAdminStatus for the repository.
--
newAdminStatus :: Repository -> IO AdminStatus
newAdminStatus repository =
   do
      statusMVar <- newMVar False
      return (AdminStatus {
         repository = repository,
         statusMVar = statusMVar
         })

-- | Prompt the user to ask him if he wants to toggle the admin status.
toggleAdminStatus :: AdminStatus -> IO ()
toggleAdminStatus adminStatus =
   do
      currentlyIsAdmin <- takeMVar (statusMVar adminStatus)
      let
         message = 
            if currentlyIsAdmin
               then
                  "Revoke admin status?"
               else
                  "Claim admin status?"

      
      nextIsAdmin <- 
         do
            goAhead <- confirmMess message
            if goAhead
               then
                  do
                     let
                        newStatus = not currentlyIsAdmin

                     succeeded <- catchAccessError (
                        setAdminStatus (repository adminStatus) newStatus)
                     case succeeded of
                        Just () -> return newStatus
                        Nothing -> return currentlyIsAdmin         
               else
                  return currentlyIsAdmin

      putMVar (statusMVar adminStatus) nextIsAdmin
       