{- This module is the top-level one which does merging. -}
module Merging(
   mergeNodes,
   ) where

import Maybe
import List

import Data.FiniteMap
import Control.Concurrent.MVar
import Control.Exception
import System.IO.Unsafe

import Computation
import ExtendedPrelude
import Object
import Broadcaster
import Sources
import FileSystem
import Registry
import Delayer
import DeepSeq
import Thread

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
--
-- For the time being, we permit only ObjectVersion's to be merged.
-- The reason for this is we cannot guarantee that identical nodes have a 
-- unique distinguishing originating version (as returned by 
-- Link.getLastChange)
mergeNodes :: Repository -> [Either View ObjectVersion] 
   -> IO (WithError View)
mergeNodes repository nodes =
   addFallOutWE (\ break ->
      do
         -- The Bool is True if the view was especially checked-out for this
         -- merge.
         (viewData :: [(View,Bool)]) <- mapM
            (\ node -> case node of
               Left view -> 
                  break "Sorry, we can only merge checked-in versions."
  
                  {- return (view,False) -}
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

         coerceWithErrorOrBreakIO break viewWE
      )


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
            parents0 <- mapM parentVersions views
            let
               -- parents0 is a list of list of ObjectVersions.  We
               -- (a) turn this into a list; (b) remove duplicates.
               parents1 = nub (concat parents0)

            parentsMVar1 <- newMVar parents1

            committingVersion <- newMVar Nothing

            let
               newView = View {
                  viewId = viewId1,
                  repository = repository1,
                  objects = objects1,
                  titleSource = titleSource1,
                  fileSystem = fileSystem1,
                  commitLock = commitLock1,
                  delayer = delayer1,
                  parentsMVar = parentsMVar1,
                  committingVersion = committingVersion
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
               mergeOne :: (WrappedMergeLink,[(View,WrappedMergeLink)]) 
                  -> IO ()
               mergeOne (WrappedMergeLink (newLink :: Link object),
                    linkViewData0) =
                  do
                     (linkViewData1 :: [(View,Link object,object)]) <-
                        mapM
                           (\ (view,wrappedLink) ->
                              do 
                                 let
                                    linkOpt 
                                       = unpackWrappedMergeLink wrappedLink

                                    link = fromMaybe
                                       (break ("Merging.mergeOne - "++
                                          "unexpected type clash"))
                                       linkOpt
                                 seq link done
                                 object <- unsafeInterleaveIO (
                                    readLink view link)
                                 return (view,link,object)
                              )      
                           linkViewData0

                     unitWE <- attemptMerge linkReAssigner newView newLink 
                        linkViewData1
                     coerceWithErrorOrBreakIO break unitWE

            breaks <- mapMConcurrent 
               (\ wrappedMergeLink 
                  -> Control.Exception.try (mergeOne wrappedMergeLink))
               (fmToList (allMergesMap linkReAssigner))

            mapM_ propagate breaks

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
            