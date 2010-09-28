-- | This module contains various low-level functions for manipulating
-- MMiSSRequest types.
module MMiSS.API.Basics(
   ) where

import Util.ExtendedPrelude

import MMiSS.API.Request

-- -------------------------------------------------------------------------
-- Instances
-- -------------------------------------------------------------------------

instance Ord ServerRef where
   compare = mapOrd serverRefRef

instance Ord VersionRef where
   compare = mapOrd versionRefRef

