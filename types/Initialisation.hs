{- This module contains functions for connecting to the repository,
   and if necessary initialising it with the top item. -}
module Initialisation(
   initialise, -- :: IO Repository,
   openRepository, -- :: (?server :: HostPort) => IO VersionDB.Repository
   initialiseGeneral, -- :: (View -> IO ()) -> IO VersionDB.Repository
   ) where

import Debug(debug)
import Computation
import BinaryIO

import HostsPorts
import CallServer

import Graph

import qualified VersionDB
import View
import Folders
import Files
import VersionGraph
import Registrations


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
initialise :: (?server :: HostPort) => IO VersionDB.Repository
initialise = initialiseGeneral (\ view -> done)

openRepository :: (?server :: HostPort) => IO VersionDB.Repository
openRepository = openRepositoryGeneral (\ view -> done)

---
-- More general initialisation, which provides an extra function
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

---
-- Create the repository.
createRepository :: (View -> IO ()) -> VersionDB.Repository -> IO ()
createRepository initialiseView repository =
   do
      -- (1) create a new view containing just an empty folder
      view <- newView repository
      topFolderLink <- getTopFolder view

      -- (2) initialise plain file type
      mkPlainFileType view
      initialiseView view

      -- (3) Create the version
      version <- VersionDB.newVersion repository
      if version == VersionDB.firstVersion
         then
            do
               commitView1 True version view
               done
         else
            -- Someone else has simultaneously initialised the repository!
            done
