-- | This module computes the parents information to be given to a merge.
module MergeComputeParents(
   computeParents, -- :: LinkReAssigner -> IO [(Location,Location)]
   ) where

import Monad
import Maybe

import DeprecatedFiniteMap

import VariableSet(HasKey(..))

import VersionDB
import ViewType
import MergeTypes
import View
import Link
import NoAccessObject -- used for dummy wrapped link


-- | Compute all the parents information for the merge given by the
-- 'LinkReAssigner'.  This means listing *all* the parents for links not
-- in the head view.
computeParents
   :: View -- ^ the head view
   -> LinkReAssigner -- ^ the reassigner
   -> IO [(Location,Location)] -- ^ list (object, its parent)
computeParents headView linkReAssigner =
   do
      let
         allMergesList :: [(WrappedMergeLink,[(View,WrappedMergeLink)])]
         allMergesList = fmToList (allMergesMap linkReAssigner)

         allNonHeadsList :: [(WrappedMergeLink,[(View,WrappedMergeLink)])]
         allNonHeadsList =
            filter
               (\ (_,equalLinks) ->
                  not (any
                     (\ (view,_) -> viewId view == viewId headView)
                     equalLinks
                     )
                  )
               allMergesList

         -- list of all nodes with parents to find with
         -- information (target link,source view,source link)
         parentsToFind :: [(WrappedMergeLink,View,WrappedMergeLink)]
         parentsToFind = map
            (\ (wml1,(view,wml2):_) -> (wml1,view,wml2))
            allNonHeadsList

      -- Command to get parent information
      (getParentsCommands :: [SimpleDBCommand]) <-
         mapM
            (\ (_,view,wml) ->
               do
                  (Just (version :: ObjectVersion)) <- getParentVersion view
                  let
                     location :: Location
                     location = toKey wml
                  return (GetParentLocation (version,location))
               )
            parentsToFind

      (MultiResponse (parentData :: [SimpleDBResponse])) <-
         queryRepository (repository headView)
            (MultiCommand getParentsCommands)
      (parentLocations :: [Maybe Location]) <- mapM
         (\ response -> case response of
            IsLocation location -> return (Just location)
            IsOK -> return Nothing
            IsError errorType mess ->
               throwError errorType ("During getParentLocations " ++ mess)
            _ -> throwError ClientError ("Mysterious error " ++ show response)
            )
         parentData

      let
         foundParents
            :: [((WrappedMergeLink,View,WrappedMergeLink),Maybe Location)]
         foundParents = zip parentsToFind parentLocations

         -- We have to convert these parents into locations in the target view.
         translateParentLocation :: View -> Location -> Location
         translateParentLocation view location =
            let
               keyLink :: Link NoAccessObject
               keyLink = mkHackedLink location

               keyWrappedLink :: WrappedMergeLink
               keyWrappedLink = WrappedMergeLink keyLink

               Just targetLink =
                  lookupFM (linkMap linkReAssigner)
                     (viewId view,keyWrappedLink)
            in
               toKey targetLink

         parents :: [(Location,Location)]
         parents = mapMaybe
            (\ ((originalWML,view,_),locationOpt) ->
               case locationOpt of
                  Nothing -> Nothing
                  Just location ->
                     let
                        translatedLocation
                           = translateParentLocation view location
                     in
                        Just (toKey originalWML,translatedLocation)
               )
            foundParents

      return parents

