{- This module is the top-level one which does merging. -}
module Merging(
   mergeNodes,
   ) where

import Maybe

import Control.Concurrent.MVar

import Computation
import ExtendedPrelude
import Object
import Broadcaster
import Sources
import FileSystem
import Registry
import Delayer

import Destructible

import VSem

import Graph

import VersionDB
import ObjectTypes
import DisplayTypes
import GlobalRegistry
import Link
import ViewType
import View
import MergeTypes
import MergeReAssign

---
-- Do all the work of merging, checking out views before merging as
-- necessary.  
mergeNodes :: Repository -> [Either View ObjectVersion] 
   -> IO (WithError View)
mergeNodes repository nodes =
   do
      -- The Bool is True if the view was especially checked-out for this
      -- merge.
      (viewData :: [(View,Bool)]) <- mapM
         (\ node -> case node of
            Left view -> return (view,False)
            Right version ->
               do
                  view <- getView repository version
                  return (view,True)
            )
         nodes

      viewWE <- mergeViews (map fst viewData)

      mapM_ 
         (\ (view,doDestroy) -> if doDestroy then destroy view else done)
         viewData

      return viewWE


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

            parentOpts <- mapM parentVersion views
            let
               parents = catMaybes parentOpts

            parentsMVar1 <- newMVar parents

            let
               newView = View {
                  viewId = viewId1,
                  repository = repository1,
                  objects = objects1,
                  titleSource = titleSource1,
                  fileSystem = fileSystem1,
                  commitLock = commitLock1,
                  delayer = delayer1,
                  parentsMVar = parentsMVar1
                  }


            -- (2) Merge the global-registry data for object types and
            -- display types.
            let
               mergeObjectTypeTypeData :: WrappedObjectTypeTypeData 
                  -> IO [(GlobalKey,[(View,WrappedObjectType)])]
               mergeObjectTypeTypeData (WrappedObjectTypeTypeData objectType)
                     =
                  do
                     allTypesWE <- mergeViewsInGlobalRegistry 
                        (objectTypeGlobalRegistry objectType) views newView
                     allTypes <- coerceWithErrorOrBreakIO break allTypesWE
                     return (map 
                        (\ (key,viewTypes) ->
                           (key,map
                              (\ (view,objectType) 
                                 -> (view,WrappedObjectType objectType))
                              viewTypes
                              )
                           )
                        allTypes
                        )

               mergeDisplayTypeTypeData :: WrappedDisplayType -> IO ()
               mergeDisplayTypeTypeData (WrappedDisplayType displayType)
                     =
                  do
                     resultWE <- mergeViewsInGlobalRegistry
                        (displayTypeGlobalRegistry displayType) views newView
                     coerceWithErrorOrBreakIO break resultWE
                     done

            (allTypes :: [(WrappedObjectTypeTypeData,
               [(GlobalKey,[(View,WrappedObjectType)])])])
               <- mapM 
                  (\ wrappedObjectTypeTypeData ->
                     do
                        theseTypes <- mergeObjectTypeTypeData
                           wrappedObjectTypeTypeData
                        return (wrappedObjectTypeTypeData,theseTypes)
                     )
                  allObjectTypeTypes

            mapM_ mergeDisplayTypeTypeData allDisplayTypeTypes

            -- (3) Compute reassignments.
            linkReAssignerWE <- mkLinkReAssigner views allTypes
            linkReAssigner <- coerceWithErrorOrBreakIO break linkReAssignerWE

            -- (4) Do merging
            let
               mergeOne :: (WrappedLink,[(View,WrappedLink)]) -> IO ()
               mergeOne (WrappedLink (newLink :: Link object),linkViewData0) =
                  do
                     (linkViewData1 :: [(View,Link object,object)]) <-
                        mapM
                           (\ (view,wrappedLink) ->
                              do 
                                 let
                                    linkOpt = unpackWrappedLink wrappedLink

                                    link = fromMaybe
                                       (break ("Merging.mergeOne - "++
                                          "unexpected type clash"))
                                       linkOpt
                                 seq link done
                                 object <- readLink view link
                                 return (view,link,object)
                              )      
                           linkViewData0

                     unitWE <- attemptMerge linkReAssigner newView newLink 
                        linkViewData1
                     coerceWithErrorOrBreakIO break unitWE

            mapM_ mergeOne (allMerges linkReAssigner)

            return newView
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
            