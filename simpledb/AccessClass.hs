module AccessClass(
   ) where

import BinaryAll

import {-# SOURCE #-} SimpleDBServer


-- -------------------------------------------------------------------------
-- Data types
-- -------------------------------------------------------------------------

-- | 'Just' 'True'  means access is permitted; 'Just' 'False', that it is
-- denied; 'Nothing', that this permission says nothing about the access.
data Permission = Permission {
   readAccess :: Maybe Bool,
   writeAccess :: Maybe Bool
   }

-- | To whom a 'Permission' applies.
-- The list of users and groups are each read from a configuration file by
-- the server
data GroupOrUser = 
      User String -- ^ User with this identifier 
   |  Group String -- ^ Group with this identifier

-- | The permissions for a particular object.
data AccessClass = AccessClass {
   permissions :: [(GroupOrUser,Permission)],
      -- ^ We check the permissions in order until we find one that matches
      -- and gives Just True or Just False.
   parent :: Maybe Location
      -- ^ If none is found, we then look for a parent access class using
      -- Location.
   }

     

   
