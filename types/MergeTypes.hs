{- This describes the various types relevant to the interface between the
   repository and object implementations and merging. -}
module MergeTypes where

import Data.FiniteMap

import Dynamics

import Link
import ViewType
import {-# SOURCE #-} ObjectTypes

-- | This describes all the links which leave an object and which need to
-- be preserved by the merge.
data ObjectLinks key = ObjectLinks [(WrappedLink,key)]


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
 
-- | This contains the reassignments made in merging, mapping each link to
-- its corresponding link in the final merge.
data LinkReAssigner = LinkReAssigner {
   linkMap :: FiniteMap (ViewId,WrappedLink) WrappedLink,
   allMerges :: [(WrappedLink,[(View,WrappedLink)])]
   } 


