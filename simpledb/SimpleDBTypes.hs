{- This module contains the types for the database, and instances for them. -}
module SimpleDBTypes(
   -- * Types that will be required by the client.
   Location(..),
   SimpleDBCommand(..),
   SimpleDBResponse(..),
   VersionInformation(..),
   Diff(..),
   ChangeData,

   -- * Types that are only used internally by the server.
   SimpleDB(..),
   FrozenVersion(..),
   VersionData(..),
   PrimitiveLocation(..),
   ) where

import Control.Monad.Trans
import Data.Typeable
import Data.IORef
import Data.FiniteMap
import GHC.Weak

import DeepSeq
import ICStringLen
import BinaryAll

import PasswordFile (User)

import Permissions
import VersionInfo
import BDBOps
import ServerErrors
import {-# SOURCE #-} VersionState

-- -------------------------------------------------------------------
-- The query types.
-- -------------------------------------------------------------------

newtype Location = Location Integer 
   deriving (Eq,Ord,Show,Typeable,Integral,Real,Enum,Num,DeepSeq)

data SimpleDBCommand =
   -- All commands may additionally return IsError or IsAccess except
   -- where stated.
      NewLocation
         -- ^ returns a new location (IsLocation),
   |  NewVersion -- ^ a new object version (IsObjectVersion).
   |  ListVersions -- ^ returns list of all known objects
         -- ^ return with IsObjectVersions
   |  Retrieve ObjectVersion Location
         -- ^ returns IsData or IsNotFound.
   |  LastChange ObjectVersion Location
         -- ^ Return the ObjectVersion (IsObjectVersion) in which this
         -- object (Location inside ObjectVersion) was last changed,
         -- or IsResponse with an error message.
   |  Commit 
         VersionInformation
             -- extra data about this version.
             -- This includes a version number, which must be uniquely
             -- allocated by NewVersion, and the parent versions, which
             -- must already exist.  The first parent version we call the
             -- head parent version.
         [(Location,Maybe ObjectVersion)]
             -- Redirects.  This list can be non-null when versions are copied 
             -- between different repositories, so that Locations are 
             -- preserved, while still being unambiguous for this version.
             -- The meaning of the (Either ObjectVersion (Maybe Location)) 
             -- is as follows. 
             --
             -- If (Just objectVersion), that means this location should have
             -- exactly the same meaning as in (objectVersion).  
             --
             -- If (Nothing), that means this location is new.
         [(Location,ChangeData)]
             -- Returns IsOK, or if the operation could not
             -- be carried out because a version with the corresponding
             -- ServerInfo has already been checked in, returns 
             -- IsObjectVersion with the objectVersion of the old version.
         [(Location,Location)]
             -- Returns a list of parent links for this commit (used for
             -- security data.
             -- NB.  The elements of the tuples are (object,parent)
             -- NB2. Only the changed links (where a location is new,
             -- or an object is moved) should be supplied.
             -- NB3. The user doing this needs to have Permissions access
             -- to the object in the version it comes from.
   |  ModifyUserInfo VersionInformation
         -- ^ If the version already exists, replace its VersionInfo by
         -- that supplied, assuming the permissions permit it.
         -- If the version does not exist create it.  But the ObjectVersion
         -- should have been allocated by NewVersion.
         --    This will not change the head parent version on any account.
         --
         -- Additional restriction: If VersionInfo1 is used but the supplied 
         -- ServerInfo is already present in the database with a different
         -- versionNumber, we do nothing and return IsObjectVersion with
         -- the old version number. 
         -- Returns IsOK.
   |  GetDiffs ObjectVersion [ObjectVersion]
         -- ^ Produce a list of changes between the given object version
         -- and the parents, in the format IsDiffs.
   |  GetPermissions (Maybe (ObjectVersion,Location))
         -- ^ Get the permissions for the location indicated by the
         -- given object version and location, or the global permissions
         -- if those are not supplied.  Returns IsPermissions.
   |  SetPermissions (Maybe (ObjectVersion,Location)) Permissions
         -- ^ Set the permissions.
   |  GetParentLocation (ObjectVersion,Location)
         -- ^ Returns IsLocation containing the parent location of the 
         -- location in the given version.
         -- 
         -- If there is no parent location (presumably because this is the
         -- top object) we return IsOK.
   |  ClaimAdmin Bool
         -- ^ This command is used with a True argument to claim admin status.
         -- If this is granted the user will have read/write/permissions
         -- access to everything.
         -- 
         -- ClaimAdmin False turns off admin status.
   |  MultiCommand [SimpleDBCommand]
         -- ^ A group of commands to be executed one after another.
         -- Returns MultiResponse with the corresponding responses.
         -- NB.  If an error occurs, the other commands will still be
         -- executed.
   deriving (Show)

data SimpleDBResponse =
      IsLocation Location
   |  IsObjectVersion ObjectVersion
   |  IsObjectVersions [ObjectVersion]
   |  IsData ICStringLen
   |  IsDiffs [(Location,Diff)] [(Location,Location)]
   |  IsVersionInfo VersionInfo
   |  IsPermissions Permissions
   |  IsError ErrorType String
   |  IsOK
   |  MultiResponse [SimpleDBResponse]
  deriving (Show)

-- | Information about a Version sent on commit or ModifyUserInfo
data VersionInformation =
      UserInfo1 UserInfo 
         -- ^ information accessible to the user, such as the text description,
         -- plus the Version number.
   |  VersionInfo1 VersionInfo
         -- ^ everything we know about the version, including its timestamp.
   |  Version1 ObjectVersion
         -- ^ Just the version number.  For Commit, it is expected that
         -- ModifyUserInfo has already been used to set the information.
         -- For ModifyUserInfo, this option is illegal.
   |  Version1Plus ObjectVersion ObjectVersion
         -- ^ Just the version number and the head parent version to use
         -- (in that order)
   deriving (Show)

data Diff = 
   -- returned from GetDiffs command.  
   -- The "parent versions" are the versions in the second argument of
   --    GetDiffs.
   -- The "parent version" is the first element of this list (if any).
      IsOld
         -- version exists in the parent version with this location and
         -- is unchanged.
   |  IsChanged {
         existsIn :: ObjectVersion,
         changed :: ChangeData
         }
         -- Location exists in one of the parent versions, namely existsIn, 
         -- but has been changed.
   |  IsNew {
         changed :: ChangeData
         }
         -- Location exists in none of the parent versions.
   -- If changed is Just (location,objectVersion) then
   -- "objectVersion" is a parentVersion, and the contents in the subject
   -- version are identical with those in (location,objectVersion);
   -- indeed the contents are not just byte-for-byte identical, but
   -- can be deduced to be identical from the rules that 
   -- (a) locations in views created by "Commit" are unchanged unless
   --     otherwise specified;
   -- (b) locations whose contents are specified as (location,version1)
   --     on commit have identical contents to those of (location,version1).
   deriving (Show)

type ChangeData = Either ICStringLen (ObjectVersion,Location)
   -- This indicates the contents of a changed item.  
   -- If (Left ...) this is raw data.
   -- If (Right ...) this means this item is in fact exactly the
   --    same as the one in (ObjectVersion,Location), a situation which
   --    arises, for example, during merging.

-- -------------------------------------------------------------------
-- Internal Structures
-- -------------------------------------------------------------------

-- | All data is stored immediately in BDB databases.  This is good because
-- it means we can rely on BDB's backup facilities.
-- 
-- To avoid too much disk-throughput, we don't actually force the BDB database
-- to be synchronised except after a commit.
data SimpleDB = SimpleDB {
   miscDB :: BDB,
      -- ^ this contains miscellanous information.
      -- Location 0 contains the next object version number to allocate.
      -- Location 1 contains the next location number to allocate.
      -- Location 2 contains the global Permissions.
   versionDB :: BDB,
      -- ^ this maps version information to FrozenVersion.
   securityDB :: BDB,
      -- ^ this maps PrimitiveLocation to Permissions.
   keyDB :: BDB,
      -- ^ this maps PrimitiveLocation to the corresponding BDB key
      -- in dataDB.
   dataDB :: BDB,
      -- ^ this contains the actual committed data (as ICStringLen).

   versionData :: IORef (FiniteMap ObjectVersion VersionData),
      -- ^ This contains the version data, for example the map from
      -- the PrimitiveLocation to the corresponding BDBKey (in this
      -- version).
   openVersions :: IORef (FiniteMap ObjectVersion (Weak User)),
      -- ^ This map contains current version numbers allocated by
      -- NewVersion (by user) which have not yet had UserInfo
      -- assigned to them by Commit or ModifyUserInfo.
      -- 
      -- This allows us to block attempts to hijack a version number
      -- allocated to someone else, or to commit to a version already
      -- committed to.
      -- 
      -- We use a weak pointer to the User.  This will have a finalizer
      -- attached to it which deletes the entry should the User disappear.
      -- This should hopefully prevent openVersions accumulating 
      -- allocated but never-used versions.
   versionState :: VersionState,
      -- ^ This contains the VersionInfo.VersionState information,
      -- which manages the VersionInfos.
   nextLocation :: IORef Location,
      -- ^ Next Location to allocate.  This will be written back to
      -- miscDB.
   nextVersion :: IORef ObjectVersion
      -- ^ Next ObjectVersion to allocate, ditto.
   }

data FrozenVersion =
   FrozenVersion { 
      parent' :: Maybe ObjectVersion,
      thisVersion' :: ObjectVersion,
      objectChanges :: [(Location,Either BDBKey (ObjectVersion,Location))],
         -- a BDBKey means completely new data.
          -- (ObjectVersion,Location) means it so happens this is exactly the
          --    contents of this object are the same as those in 
          --    (ObjectVersion,Location)
      redirects' :: [(Location,Either ObjectVersion PrimitiveLocation)],
          -- this gives redirects.  Note that this list does not have to 
          -- contain locations which are the same as the corresponding 
          -- primitive location, or where a redirect for this location already
          -- exists in the parent version.  
          --    The interpretation is as follows.  For (Left objectVersion)
          --    that means we copy whatever redirect exists, if any, for
          --    the same location in objectVersion.
          --    
          --    For (Right primitiveLocation) that means this location is
          --    new, and associated with this primitive location.
      parentChanges :: [(Location,Location)]
         -- parent changes (in same order as for commit).
      }

data VersionData = VersionData {
   parent :: Maybe ObjectVersion,
      -- ^ This version's parent.
   objectDictionary :: FiniteMap PrimitiveLocation BDBKey,
      -- ^ The dictionary contains every location in the ObjectDictionary,
      -- even those which are identical to those in the parent.  However
      -- we construct the objectDictionary starting with the dictionary
      -- for the parent and adding the changes in this version.  Thus,
      -- because of persistence, the actual extra memory occupied on the
      -- server should be small, if there are only a few changes from the
      -- parent.      
   redirects :: FiniteMap Location PrimitiveLocation,
      -- ^ This maps Location to the corresponding PrimitiveLocation,
      -- when the integers inside are different.
      -- As with objectDictionary, we use persistence.
   parentsMap :: FiniteMap Location Location
      -- ^ Map from an object to its parent, if any.
   }

-- PrimitiveLocation's permit redirection of Locations.  Thus "Location"
-- is the user-visible Location, the PrimitiveLocation is the "real" one
-- as indexed by objectDictionary.
--
-- The purpose of this system is that the "Location" will remain constant,
-- even when versions are exported from one repository to another.  However
-- the repository is free to reassign PrimitiveLocation's behind the
-- scenes. 
newtype PrimitiveLocation = PrimitiveLocation Integer deriving (Eq,Ord)

-- -----------------------------------------------------------------------
-- Instances of HasBinary
-- -----------------------------------------------------------------------

instance Monad m => HasBinary Location m where
   writeBin = mapWrite (\ (Location i) -> (Unsigned i))
   readBin = mapRead (\ (Unsigned i) -> (Location i))

instance Monad m => HasBinary PrimitiveLocation m where
   writeBin = mapWrite (\ (PrimitiveLocation i) -> (Unsigned i))
   readBin = mapRead (\ (Unsigned i) -> (PrimitiveLocation i))

instance MonadIO m => HasWrapper SimpleDBCommand m where
   wraps = [
      wrap0 0 NewLocation,
      wrap0 1 NewVersion,
      wrap0 2 ListVersions,
      wrap2 3 Retrieve,
      wrap2 4 LastChange,
      wrap4 5 Commit,
      wrap1 6 ModifyUserInfo,
      wrap2 8 GetDiffs,
      wrap1 9 MultiCommand,
      wrap1 10 GetPermissions,
      wrap2 11 SetPermissions,
      wrap1 12 ClaimAdmin,
      wrap1 13 GetParentLocation
      ]
   unWrap = (\ wrapper -> case wrapper of
      NewLocation -> UnWrap 0 ()
      NewVersion -> UnWrap 1 ()
      ListVersions -> UnWrap 2 ()
      Retrieve l v -> UnWrap 3 (l,v)
      LastChange l v -> UnWrap 4 (l,v)
      Commit v r n p -> UnWrap 5 (v,r,n,p)
      ModifyUserInfo v -> UnWrap 6 v
      GetDiffs v vs -> UnWrap 8 (v,vs)
      MultiCommand l -> UnWrap 9 l
      GetPermissions pl -> UnWrap 10 pl
      SetPermissions pl s -> UnWrap 11 (pl,s)
      ClaimAdmin b -> UnWrap 12 b
      GetParentLocation l -> UnWrap 13 l
      )

instance (MonadIO m,HasWrapper SimpleDBCommand m) 
      => HasBinary SimpleDBCommand m where
   writeBin = mapWrite Wrapped
   readBin = mapRead wrapped


instance MonadIO m => HasWrapper VersionInformation m where
   wraps = [
      wrap1 0 UserInfo1,
      wrap1 1 VersionInfo1,
      wrap1 2 Version1,
      wrap2 3 Version1Plus
      ]
   unWrap = (\ wrapper -> case wrapper of
      UserInfo1 u -> UnWrap 0 u
      VersionInfo1 v -> UnWrap 1 v
      Version1 v -> UnWrap 2 v
      Version1Plus v p -> UnWrap 3 (v,p)
      )

instance (MonadIO m,HasWrapper VersionInformation m) 
      => HasBinary VersionInformation m where
   writeBin = mapWrite Wrapped
   readBin = mapRead wrapped

instance MonadIO m => HasWrapper SimpleDBResponse m where
   wraps = [
      wrap1 0 IsLocation,
      wrap1 1 IsObjectVersion,
      wrap1 2 IsObjectVersions,
      wrap1 3 IsData,
      wrap2 4 IsError,
      wrap2 5 IsDiffs,
      wrap1 6 IsVersionInfo,
      wrap0 7 IsOK,
      wrap1 8 MultiResponse,
      wrap1 11 IsPermissions

      ]
   unWrap = (\ wrapper -> case wrapper of
      IsLocation l -> UnWrap 0 l
      IsObjectVersion v -> UnWrap 1 v
      IsObjectVersions vs -> UnWrap 2 vs
      IsData d -> UnWrap 3 d
      IsError t e -> UnWrap 4 (t,e)
      IsDiffs ds ps -> UnWrap 5 (ds,ps)
      IsVersionInfo v -> UnWrap 6 v
      IsOK -> UnWrap 7 ()
      MultiResponse l -> UnWrap 8 l
      IsPermissions p -> UnWrap 11 p
      )

instance (MonadIO m,HasWrapper SimpleDBResponse m) 
      => HasBinary SimpleDBResponse m where
   writeBin = mapWrite Wrapped
   readBin = mapRead wrapped


instance MonadIO m => HasWrapper Diff m where
   wraps = [
      wrap0 1 IsOld,
      wrap2 2 IsChanged,
      wrap1 3 IsNew
      ]

   unWrap = (\ wrapper -> case wrapper of
      IsOld -> UnWrap 1 ()
      IsChanged e c -> UnWrap 2 (e,c)
      IsNew c -> UnWrap 3 c
      )

instance (MonadIO m,HasWrapper Diff m) 
      => HasBinary Diff m where
   writeBin = mapWrite Wrapped
   readBin = mapRead wrapped

instance Monad m => HasBinary FrozenVersion m where
   writeBin = mapWrite (\ --
      FrozenVersion {
         parent' = parent',
         thisVersion' = thisVersion',
         objectChanges = objectChanges,
         redirects' = redirects',
         parentChanges = parentChanges
         }
      -> (parent',thisVersion',objectChanges,redirects',parentChanges)
      )
   readBin = mapRead (\ --
      (parent',thisVersion',objectChanges,redirects',parentChanges)
      -> FrozenVersion {
         parent' = parent',
         thisVersion' = thisVersion',
         objectChanges = objectChanges,
         redirects' = redirects',
         parentChanges = parentChanges
         }
      )


