-- | The function in this module prunes a list of links to objects in various
-- views, retaining those whose version is not the ancestor of another version
-- in the same list.   This is useful in merging, since normally direct
-- ancestors of versions being merged can be disregarded.
--
-- NB.  We assume for the time being that all views are recently checked out.
-- This means every object has an originating Version (the value
-- returned by getVersion).  The module Merging enforces this
-- restriction, which perhaps will be lifted one day.
module Types.MergePrune(
   mergePrune,
      -- :: [(View,Link object,object)] -> IO [(View,Link object,object)]
      -- The type of the list is chosen to be compatible with that of
      -- ObjectTypes.attemptMerge.  The objects should match the links.
      -- All the views should, of course, come from the same repository.
   ) where

import Monad

import Util.DeprecatedFiniteMap

import Graphs.FindCommonParents(GraphBack(..))
import Graphs.RemoveAncestors

import Types.VersionDB(ObjectVersion)
import Types.Link
import Types.CodedValue
import Types.View
import Types.ViewType
import Types.VersionGraphClient

mergePrune :: HasCodedValue object
   => [(View,Link object,object)] -> IO [(View,Link object,object)]
mergePrune [] = return []
mergePrune [oneItem] = return [oneItem]
mergePrune ((linkList0 @ ((firstView,_,_):_)) :: [(View,Link object,object)]) =
   do
      -- (0) get the version graph
      let
         graphClient = graphClient1 firstView

      -- (1) cluster identical Link values together, to reorganise the
      -- list to have type [[(View,Link object,object)]]
      let
         fMap :: FiniteMap (Link object) [(View,Link object,object)]
         fMap = foldl
            (\ map0 (vlo@(view,link,object)) ->
               addToFM map0 link
                  (vlo : (lookupWithDefaultFM map0 [] link))
               )
            emptyFM
            linkList0

         linkList1 :: [[(View,Link object,object)]]
         linkList1 = eltsFM fMap

      -- (2) Do the job for the individual lists.
      (linkList2 :: [[(View,Link object,object)]]) <-
         mapM (mergePruneInner graphClient) linkList1

      -- (3) un-cluster linkList2.
      let
         linkList3 :: [(View,Link object,object)]
         linkList3 = concat linkList2

      return linkList3

mergePruneInner :: HasCodedValue object
   => VersionGraphClient -> [(View,Link object,object)]
   -> IO [(View,Link object,object)]
mergePruneInner graphClient (linkList0 :: [(View,Link object,object)])
      =
   do
      -- We use an inputGraphBack to find out what parents an ObjectVersion
      -- has.
      (inputGraphBack :: GraphBack VersionGraphNode ())
         <- getInputGraphBack graphClient (\ _ _ -> ())
      let
         getVersionParents :: ObjectVersion -> [ObjectVersion]
         getVersionParents ov = case getParents inputGraphBack
               (CheckedInNode ov) of
            Nothing -> error ("MergePrune.mergePruneInner: View not in "
               ++ "version graph")
            Just parents -> map
               (\ parentNode -> case parentNode of
                  CheckedInNode ov -> ov
                  )
               parents

      -- Turn the linkList0 into a map from Nodes in the version graph
      -- to the corresponding element of linkList0.
      --
      -- It may well happen that the same Node corresponds to several elements.
      -- This will mean that an object is unchanged in several views.  In
      -- that case we drop all but one of those elements.  Thus this is the
      -- first stage of pruning.
      (nodeMap :: FiniteMap ObjectVersion (View,Link object,object)) <- foldM
         (\ map0 (vlo@(view,link,object))->
            do
               origVersion <- getVersion view link
               return (addToFM map0 origVersion vlo)
            )
         emptyFM
         linkList0

      -- Do the pruning
      let
         versions = keysFM nodeMap

         prunedVersions = removeAncestorsByPure getVersionParents versions

         result =
            map
               (lookupWithDefaultFM nodeMap (error "MergePrune.1"))
               prunedVersions

      return result

getVersion :: HasCodedValue object => View -> Link object -> IO Version
getVersion view link =
   do
      objectVersionOpt <- getLastChange view link
      case objectVersionOpt of
         Just objectVersion -> return objectVersion

