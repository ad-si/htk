-- | This module contains functions for connecting to the repository,
-- and if necessary initialising it with the top item.
module Types.Initialisation(
   initialise, -- :: IO Repository,
   openRepository, -- :: (?server :: HostPort) => IO VersionDB.Repository
   initialiseGeneral, -- :: (View -> IO ()) -> IO VersionDB.Repository
   openRepositoryInternal, -- :: VersionState -> IO VersionDB.Repository
   ) where

import Util.Computation

import Server.HostsPorts

import qualified SimpleDB.VersionState as VersionState
import qualified Types.VersionDB as VersionDB
import Types.View
import Types.Folders
import Types.Registrations


--
-- All four functions connect to the repository, if necessary initialising
-- the first view in it.
--
-- The initialiseXXX functions do the standard (not MMiSS) registrations),
-- and then connect to the repository.
--
-- The xxxGeneral functions allow an additional function to be executed
-- on the first view.
--
-- The xxxInternal functions are used for the internal server.
initialise :: (?server :: HostPort) => IO VersionDB.Repository
initialise = initialiseGeneral (\ view -> done)

openRepository :: (?server :: HostPort) => IO VersionDB.Repository
openRepository = openRepositoryGeneral (\ view -> done)


openRepositoryInternal :: VersionState.VersionState -> IO VersionDB.Repository
openRepositoryInternal = openRepositoryGeneralInternal (\ view -> done)

-- | More general initialisation, which provides an extra function
-- to be executed when the very first view is created.
initialiseGeneral :: (?server :: HostPort)
   => (View -> IO ()) -> IO VersionDB.Repository
initialiseGeneral initialiseView =
   do
      doRegistrations
      openRepositoryGeneral initialiseView

openRepositoryGeneral :: (?server :: HostPort)
   => (View -> IO ()) -> IO VersionDB.Repository
openRepositoryGeneral initialiseView =
   do
      repository <- VersionDB.initialise
      viewVersions <- listViews repository
      case viewVersions of
         [] ->
            do
               createRepository initialiseView repository
         _ -> done -- the repository is already initialised
      return repository

openRepositoryGeneralInternal
   :: (View -> IO ()) -> VersionState.VersionState -> IO VersionDB.Repository
openRepositoryGeneralInternal initialiseView versionState =
   do
      repository <- VersionDB.initialiseInternal versionState
      viewVersions <- listViews repository
      case viewVersions of
         [] -> createRepository initialiseView repository
         _ -> done -- the repository is already initialised
      return repository

-- | Create the repository.
createRepository :: (View -> IO ()) -> VersionDB.Repository -> IO ()
createRepository initialiseView repository =
   do
      -- (0) Force admin status (which we need to check in the
      -- initial version).
      VersionDB.setAdminStatus repository True
      -- (1) create a new view containing just an empty folder
      view <- newView repository
      topFolderLink <- getTopFolder view

      -- (2) run initialiseView
      initialiseView view

      -- (3) Create the version
      version <- VersionDB.newVersion repository
      if version == VersionDB.firstVersion
         then
            do
               commitView1 AllVersionInfo version view
               done
         else
            -- Someone else has simultaneously initialised the repository!
            done

      -- (4) revoke admin status again.
      VersionDB.setAdminStatus repository False

