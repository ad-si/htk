{- This module contains functions for connecting to the repository,
   and if necessary initialising it with the top item. -}
module Initialisation(
   initialise, -- :: IO Repository,
   initialiseGeneral, -- :: (View -> IO ()) -> IO VersionDB.Repository
   ) where

import Debug(debug)
import Computation
import BinaryIO

import CallServer

import Graph

import qualified VersionDB
import View
import Folders
import Files
import VersionGraph
import Registrations

---
-- Connect to the repository, if necessary initialising it.
initialise :: IO VersionDB.Repository
initialise = initialiseGeneral (\ view -> done)

---
-- More general initialisation, which provides an extra function
-- to be executed when the very first view is created. 
initialiseGeneral :: (View -> IO ()) -> IO VersionDB.Repository
initialiseGeneral initialiseView =
   do
      doRegistrations

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
               commitView view
               done
         else
            -- Someone else has simultaneously initialised the repository!
            done
