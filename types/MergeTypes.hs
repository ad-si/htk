{- This describes the various types relevant to the interface between the
   repository and object implementations and merging. -}
module MergeTypes where

import FiniteMap

import ViewType
import {-# SOURCE #-} ObjectTypes

-- | This describes all the links which leave an object and which need to
-- be preserved by the merge.
--
-- The "Show" instance is used in error messages.
data ObjectLinks =
   forall key . (Ord key,Show key) => ObjectLinks [(WrappedLink,key)]


-- | This contains the reassignments made in merging, mapping each link to
-- its corresponding link in the final merge.
data LinkReAssigner = LinkReAssigner {
   linkMap :: FiniteMap (ViewId,WrappedLink) View
   } 