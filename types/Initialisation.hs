{- This module contains functions for connecting to the repository,
   and if necessary initialising it with the top item. -}
module Initialisation(
   initialise, -- :: IO Repository,
   ) where

import Debug(debug)
import Computation

import CallServer

import Graph

import qualified VersionDB
import View
import Folders
import Files
import VersionGraphService
import VersionGraph

---
-- Connect to the repository, if necessary initialising it.
initialise :: IO VersionDB.Repository
initialise =
   do
      repository <- VersionDB.initialise
      viewVersions <- listViews repository
      case viewVersions of
         [] -> 
            do
               createRepository repository
         _ -> done -- the repository is already initialised
      return repository

---
-- Create the repository.
createRepository :: VersionDB.Repository -> IO ()
createRepository repository =
   do
      -- (1) create a new view containing just an empty folder
      view <- newView repository
      topFolderLink <- getTopFolder view

      -- (2) initialise plain file type
      mkPlainFileType view

      version <- commitView view

      if version /= VersionDB.firstVersion
         then
            -- OK, it looks as if two clients are running initialise 
            -- simultaneously.  We let the other one take priority, and
            -- do nothing with this version (so it will become dead).
            done
         else
            do
               -- (2) connect to the server.
               (updateServer,getUpdate,disconnect,stateString) <-
                  connectBroadcastOther versionGraphService

               -- (3) Put the node and arc types into the server.
               updateServer (NewNodeType checkedInType ())
               updateServer (NewNodeType workingType ())
               updateServer (NewArcType checkedInArcType ())
               updateServer (NewArcType workingArcType ())

               -- (4) Put the version into the server.
               updateServer (NewNode (versionToNode version) checkedInType "")

               -- (5) disconnect from the server
               disconnect
