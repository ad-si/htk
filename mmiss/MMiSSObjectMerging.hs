{- This module contains the functions for merging MMiSSObjects -}
module MMiSSObjectMerging(

   -- These functions correspond to ObjectTypes.getMergeLinks and
   -- ObjectTypes.attemptMerge.
   mmissMergeLinks, -- :: MergeLinks MMiSSObject

   mmissAttemptMerge, 
      -- :: LinkReAssigner -> View -> Link MMiSSObject
      -- -> [(View,Link MMiSSObject,MMiSSObject)] -> IO (WithError ())
   ) where

import Computation
import ExtendedPrelude

import Link
import MergeTypes
import ObjectTypes
import MergePrune
import LinkManager
import View


import MMiSSObjectType

mmissMergeLinks :: MergeLinks MMiSSObject
mmissMergeLinks = error "TBD"

mmissAttemptMerge :: LinkReAssigner -> View -> Link MMiSSObject
   -> [(View,Link MMiSSObject,MMiSSObject)] -> IO (WithError ())
mmissAttemptMerge = error "TBD"