{- This describes the various types relevant to the interface between the
   repository and object implementations and merging. -}
module MergeTypes where

import Data.FiniteMap

import Dynamics

import CodedValue
import Link
import ViewType
import {-# SOURCE #-} ObjectTypes

-- | This describes all the links which leave an object and which need to
-- be preserved by the merge.
data ObjectLinks key = ObjectLinks [(WrappedLink,key)]

instance Functor ObjectLinks where
   fmap fn (ObjectLinks l) =
      ObjectLinks (map (\ (wrappedLink,key) -> (wrappedLink,fn key)) l)

concatObjectLinks :: [ObjectLinks key] -> ObjectLinks key
concatObjectLinks l 
   = ObjectLinks (concat (map (\ (ObjectLinks links) -> links) l))


---
-- This is the function objects need to provide.
--
-- The "Show" instance is used in error messages.  The Typeable instance
-- is needed to get the key out and compare it with other keys for the
-- same object type in other views.
data MergeLinks object = forall key . (Ord key,Show key,Typeable key) 
   => MergeLinks (View -> Link object -> IO (ObjectLinks key))


emptyMergeLinks :: MergeLinks object
emptyMergeLinks 
   = MergeLinks (\ _ _ -> return ((ObjectLinks []) :: ObjectLinks ()))

singletonMergeLinks 
   :: HasCodedValue object => (object -> WrappedLink) -> MergeLinks object
singletonMergeLinks toWrappedLink =
   let
      fn view link =
         do
            object <- readLink view link
            return (ObjectLinks [(toWrappedLink object,())])
   in
      MergeLinks fn


-- pairMergeLinks 
pairMergeLinks :: MergeLinks object -> MergeLinks object -> MergeLinks object
pairMergeLinks (MergeLinks fn1) (MergeLinks fn2) =
   let
      fn view link =
         do
            (objectLinks1 @ (ObjectLinks list1)) <- fn1 view link
            (objectLinks2 @ (ObjectLinks list2)) <- fn2 view link
            let
               list =
                     (map (\ (wrappedLink,key1) -> (wrappedLink,Left key1)) 
                        list1)
                  ++ (map (\ (wrappedLink,key2) -> (wrappedLink,Right key2))
                        list2)
            return (ObjectLinks list)
   in
      MergeLinks fn
 
-- | This contains the reassignments made in merging, mapping each link to
-- its corresponding link in the final merge.
data LinkReAssigner = LinkReAssigner {
   linkMap :: FiniteMap (ViewId,WrappedLink) WrappedLink,
   allMerges :: [(WrappedLink,[(View,WrappedLink)])]
   } 


