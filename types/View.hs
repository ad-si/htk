{- This module defines the fundamental structure of the (untyped) 
   objects in a repository. 

   We depend circularly on CodedValue.hs.  This module is compiled
   first and uses CodedValue.hi-boot.
-}
module View(
   View, -- A View represents a particular version being worked on
      -- by the client.
   newView, -- :: Repository -> IO View
      -- newView creates a wholly new view without respect to version.
   listViews, -- :: Repository -> IO [ObjectVersion]
      -- This lists the object versions for all currently checked-in
      -- (global) versions.
   getView, -- :: Repository -> ObjectVersion -> IO View
      -- This checks out a particular view.
   commitView, -- :: View -> IO ObjectVersion
      -- This commits all the objects in a particular view, returning
      -- a global version.
   ) where

import Directory

import Concurrent

import Registry
import Dynamics
import AtomString(fromString)

import VersionDB
import ViewType
import CodedValue
import CodedValueStore
import DisplayTypes
import ObjectTypes

-- ----------------------------------------------------------------------
-- Views
-- (The View datatype is imported from ViewType)
--
-- Conventions.  The index to the view is stored in location
-- firstLocation.  The top link is secondLocation.
-- ----------------------------------------------------------------------

newView :: Repository -> IO View
-- newView creates a wholly new view without respect to version.
-- This should be used for initialising the repository, and never again.
newView repository =
   do
      objects <- newRegistry
      parentMVar <- newMVar Nothing
      displayTypes <- newRegistry
      objectTypes <- newRegistry

      return (View {
         repository = repository,
         objects = objects,
         parentMVar = parentMVar,
         displayTypes = displayTypes,
         objectTypes = objectTypes
         })

listViews :: Repository -> IO [ObjectVersion]
listViews repository = listVersions repository firstLocation

getView :: Repository -> ObjectVersion -> IO View
getView repository objectVersion =
   do
      objectsString <- retrieveString repository firstLocation objectVersion
      let
         objectsCodedValue = fromString objectsString
         phantomView = error "CodedValue for links and version needs View!"
         -- It shouldn't, it goes via HasCodedValuePure.  But we need
         -- HasCodedValue for technical reasons since lists and tuples
         -- can't be defined for HasPureCodedValue to avoid a nasty instance
         -- overlap.
      (objectsList :: [(Location,ObjectVersion)]) 
         <- doDecodeIO objectsCodedValue phantomView
      objects <- newRegistry
      sequence_ (map
         (\ (location,objectVersion) -> 
            setValue objects location (AbsentObject objectVersion)
            )
         objectsList
         )
      parentMVar <- newMVar (Just objectVersion)
      
      return (View {
         repository = repository,
         objects = objects,
         parentMVar = parentMVar
         })

commitView :: View -> IO ObjectVersion
commitView (View {repository = repository,objects = objects,
      parentMVar = parentMVar}) =
   -- NB - this ought to lock against updates, but at the moment doesn't.
   -- But we do lock against other commits using parentMVar.
   do
      parentOpt <- takeMVar parentMVar
      locations <- listKeys objects
      (objectsList :: [(Location,ObjectVersion)]) <-
         mapM (\ location ->
            do
               objectData <- getValue objects location
               objectVersion <- case objectData of 
                  AbsentObject objectVersion -> return objectVersion
                  PresentObject _ commitAction -> commitAction
               return (location,objectVersion)
            )
            locations
      let
         phantomView = error "CodedValue for links and version needs View (2)!"
      objectsCodedValue <- doEncodeIO objectsList phantomView
      objectsObjectSource <- toObjectSource objectsCodedValue 
      newObjectVersion <- commit repository objectsObjectSource firstLocation 
         parentOpt
      putMVar parentMVar (Just newObjectVersion)
      return newObjectVersion



   