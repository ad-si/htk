{- This module contains various low-level functions for manipulating 
   MMiSSRequest types. -}
module MMiSSAPIBasics(
   ) where

import ExtendedPrelude

import MMiSSRequest

-- -------------------------------------------------------------------------
-- Instances
-- -------------------------------------------------------------------------

instance Ord ServerRef where
   compare = mapOrd serverRefRef

instance Ord VersionRef where
   compare = mapOrd versionRefRef