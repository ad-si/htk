-- | This module is the top-level one which does merging. 
module Merging(
   mergeNodes,

   mergeObjectTypeTypeData,

   ) where

import Maybe
import List

import Data.FiniteMap
import Control.Exception
import System.IO.Unsafe

import Computation
import ExtendedPrelude
import Object
import Sources
import Thread
import Registry (setValue)

import Destructible

import VersionInfo
import VersionGraphClient
import VersionDB
import ObjectTypes
import DisplayTypes
import GlobalRegistry
import Link
import ViewType
import View
import MergeTypes
import MergeReAssign
import MergeComputeParents

-- | Do all the work of merging, checking out views before merging as
-- necessary.
-- 
-- For the time being, we permit only ObjectVersion\'s to be merged.
-- The reason for this is we cannot guarantee that identical nodes have a 
-- unique distinguishing originating version (as returned by 
-- Link.getLastChange)
mergeNodes :: Repository -> VersionGraphClient
   -> [Either View ObjectVersion] 
   -> IO (WithError View)
mergeNodes repository versionGraph nodes =
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
                     view <- getView repository versionGraph version
                     return (view,True)
               )
            nodes

         viewWE <- mergeViews (map fst viewData)

         mapM_ 
            (\ (view,doDestroy) -> if doDestroy then destroy view else done)
            viewData

         view <- coerceWithErrorOrBreakIO break viewWE
         return view

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
            -- (1) make the new view.
            let
               repository1 = repository firstView
               graphClient = graphClient1 firstView


            viewInfo0 <- readContents (viewInfoBroadcaster firstView)
            parents0 <- mapM parentVersions views
            let
               -- parents0 is a list of list of ObjectVersions.  We
               -- (a) turn this into a list; (b) remove duplicates;
               -- (c) modify viewInfo appropriately.
               parents1 = nub (concat parents0)

               user0 = user viewInfo0
               user1 = user0 {parents = parents1}
               viewInfo1 = viewInfo0 {user = user1}

            newView <- createView repository1 graphClient viewInfo1

            -- (2) Merge the global-registry data for object types and
            -- display types.
            let

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
                           break views newView
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
                  -> IO PostMerge
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
                                    readLink view link
                                    )
                                 return (view,link,object)
                              )      
                           linkViewData0

                     postMergeWE <- attemptMergeWithPostMerge linkReAssigner 
                        newView newLink linkViewData1
                     coerceWithErrorOrBreakIO break postMergeWE

            -- (5) compute parent information for merging.
            (parentLocations :: [(Location,Location)]) 
               <- computeParents firstView linkReAssigner
            mapM_ 
               (\ (object,parent) 
                  -> setValue (parentChanges newView) object parent
                  )
               parentLocations

            -- (6) do post-merge operations
            postMergesOrExcep <- mapMConcurrent 
               (\ wrappedMergeLink 
                  -> Control.Exception.try (mergeOne wrappedMergeLink))
               (fmToList (allMergesMap linkReAssigner))

            mapM_ 
               (\ postMergeOrExcep ->
                  do
                     postMerge <- propagate postMergeOrExcep
                     doPostMerge postMerge
                  )
               postMergesOrExcep

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
            

-- | mergeObjectTypeTypeData is used to get all the WrappedObjectTypes
-- with corresponding GlobalKey's obtained by merging the argument views
-- into the new view.
-- It is exported because CopyVersion.hs also finds it handy.
mergeObjectTypeTypeData 
   :: BreakFn -> [View] -> View -> WrappedObjectTypeTypeData 
   -> IO [(GlobalKey,[(View,WrappedObjectType)])]
mergeObjectTypeTypeData break views newView 
    (WrappedObjectTypeTypeData objectType) =
   do
      allRegistryTypesWE <- mergeViewsInGlobalRegistry 
         (objectTypeGlobalRegistry objectType) views newView
      allRegistryTypes 
         <- coerceWithErrorOrBreakIO break allRegistryTypesWE
      let
         allRegistryTypes1 = map 
            (\ (key,viewTypes) ->
               (key,map
                  (\ (view,objectType) 
                     -> (view,WrappedObjectType objectType))
                  viewTypes
                  )
               )
            allRegistryTypes

         getExtraObjectTypes1 :: ObjectType objectType object =>
            objectType -> IO [objectType]
         getExtraObjectTypes1 _ = extraObjectTypes

         getExtraObjectTypes :: IO [WrappedObjectType]
         getExtraObjectTypes =
            do
               extraObjectTypes1 
                  <- getExtraObjectTypes1 objectType
               return (map WrappedObjectType extraObjectTypes1)

      extraObjectTypes <- getExtraObjectTypes
      let
         extraObjectTypes2 = map
            (\ wrappedObjectType  ->
               (objectTypeId wrappedObjectType,
                  map 
                     (\ view -> (view,wrappedObjectType))
                     views
                  )
               )
            extraObjectTypes
      return (allRegistryTypes1 ++ extraObjectTypes2)
