{- This module is the top-level one which does merging. -}
module Merging(
   ) where

import Control.Concurrent.MVar

import Computation
import ExtendedPrelude
import Object
import Broadcaster
import Sources
import FileSystem
import Registry
import Delayer

import VSem

import ObjectTypes
import DisplayTypes
import GlobalRegistry
import ViewType
import View

mergeViews :: [View] -> IO (WithError View)
mergeViews [] = return (hasError "No views selected")
mergeViews (views @ (firstView:_)) =
      -- We take certain things, such as the title, from the first view in
      -- the list.
   do
      -- Get stuff we need to know to backtrack if something goes wrong.
      viewIdObject <- newObject
      let
         viewId1 = ViewId viewIdObject

      (allObjectTypeTypes :: [WrappedObjectTypeTypeData]) 
         <- getAllObjectTypeTypes

      (allDisplayTypeTypes :: [WrappedDisplayType])
         <- getAllDisplayTypeTypes

      resultWE <- addFallOutWE (\ break ->
         do
            -- (1) generate the easy things.
            let
               repository1 = repository firstView

            objects1 <- newRegistry
            title <- readContents (titleSource firstView)
            titleSource1 <- newSimpleBroadcaster title
            fileSystem1 <- newFileSystem
            commitLock1 <- newVSem
            delayer1 <- newDelayer
            parentData <- readMVar (parentMVar firstView)
            parentMVar1 <- newMVar parentData

            let
               newView = View {
                  viewId = viewId1,
                  repository = repository1,
                  objects = objects1,
                  titleSource = titleSource1,
                  fileSystem = fileSystem1,
                  commitLock = commitLock1,
                  delayer = delayer1,
                  parentMVar = parentMVar1
                  }


            -- (2) Merge the global-registry data for object types and
            -- display types.
            let
               mergeObjectTypeTypeData :: WrappedObjectTypeTypeData -> IO ()
               mergeObjectTypeTypeData (WrappedObjectTypeTypeData objectType)
                     =
                  do
                     unitWE <- mergeViewsInGlobalRegistry 
                        (objectTypeGlobalRegistry objectType) views newView
                     coerceWithErrorOrBreakIO break unitWE

               mergeDisplayTypeTypeData :: WrappedDisplayType -> IO ()
               mergeDisplayTypeTypeData (WrappedDisplayType displayType)
                     =
                  do
                     unitWE <- mergeViewsInGlobalRegistry
                        (displayTypeGlobalRegistry displayType) views newView
                     coerceWithErrorOrBreakIO break unitWE

            mapM_ mergeObjectTypeTypeData allObjectTypeTypes
            mapM_ mergeDisplayTypeTypeData allDisplayTypeTypes

            -- (3) Compute reassignments.
            error "Not done"
         )

      if isError resultWE
         then
            do
               -- Delete view from global registries.
               let
                  deleteObjectTypeTypeData :: WrappedObjectTypeTypeData 
                     -> IO ()
                  deleteObjectTypeTypeData 
                        (WrappedObjectTypeTypeData objectType) =
                     deleteViewIdFromGlobalRegistry 
                        (objectTypeGlobalRegistry objectType) viewId1

                  deleteDisplayTypeTypeData :: WrappedDisplayType -> IO ()
                  deleteDisplayTypeTypeData (WrappedDisplayType displayType) =
                     deleteViewIdFromGlobalRegistry 
                        (displayTypeGlobalRegistry displayType) viewId1

               mapM_ deleteObjectTypeTypeData allObjectTypeTypes
               mapM_ deleteDisplayTypeTypeData allDisplayTypeTypes
         else
            done

      return resultWE
            